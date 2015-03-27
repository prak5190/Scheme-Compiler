(library (Compiler specify-representation)
  (export
   specify-representation
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (specify-representation program)
    (let* ((offset-car (- disp-car tag-pair))
           (offset-cdr (- disp-cdr tag-pair))
           (offset-vector-length (- disp-vector-length tag-vector))
           (offset-vector-data (- disp-vector-data tag-vector)))

      ;; Map to number of arguments -- No need i guess
      (define (value-prim? exp)                   ;get-trace-define
        (define value-prim '((+ 2) (- 2) (* 2) (car 1) (cdr 1) (cons 2) (make-vector 1) (vector-length 1) (vector-ref 2) (void 0)))
        (and (assq exp value-prim) #t))
      
      (define (effect-prim? exp)                   ;get-trace-define
        (define effect-prim '((set-car! 2) (set-cdr! 2) (vector-set! 3)))
        (and (assq exp effect-prim) #t))
      
      (define (pred-prim? exp)                   ;get-trace-define
        (define pred-prim '((< 2) (<= 2) (= 2) (>= 2) (> 2) (boolean? 1) (eq? 2) (fixnum? 1) (null? 1) (pair? 1) (vector? 1)))
        (and (assq exp pred-prim) #t))
      
      (define (Value* expls)
        (cond
         ((null? expls) expls)
         (else (let ((x (Value (car expls))))
                 (append `(,x) (Value* (cdr expls)))))))

      ;; Convert the immediate into appropriate representation 
      (define (Immediate exp)
        (match exp
          (#t $true)
          (#f $false)
          (() $nil)
          (void $void)
          ;; Shift by 3 (shift-fixnum) places, 1 becomes 8 , 11 becomes 1011000 and so on 
          (,x (guard (fixnum-range? x)) (ash x shift-fixnum))
          (,else else)))
      
      (define (Value-Prim exp)
        (match exp
          ((* ,y ,z)  (let* ((y (Value y))
                             (z (Value z)))
                        ;; TODO - do sra at compile time for constants
                        `(* ,y (sra ,z ,shift-fixnum))))
          ((car ,x) (let* ((x (Value x)))
                      `(mref ,x ,offset-car)))          
          ((cdr ,x) (let* ((x (Value x)))
                      `(mref ,x ,offset-cdr)))
          ((cons ,e1 ,e2) (let* ((e1 (Value e1))
                                 (e2 (Value e2))                                 
                                 (tmp-car (unique-name 'tm))
                                 (tmp-cdr (unique-name 'tm))
                                 (tmp (unique-name 'tm)))
                            `(let ([,tmp-car ,e1] [,tmp-cdr ,e2])
                               (let ([,tmp (+ (alloc ,size-pair) ,tag-pair)])
                                 (begin
                                   (mset! ,tmp ,offset-car ,tmp-car)
                                   (mset! ,tmp ,offset-cdr ,tmp-cdr)
                                   ,tmp)))))
          ((make-vector ,x) (let* ((x (Value x))
                                   (tmp (unique-name 'tm))
                                   (n (+ disp-vector-data x)))
                              `(let ([,tmp (+ (alloc ,n) ,tag-vector)])
                                 (begin
                                   (mset! ,tmp ,offset-vector-length ,x)
                                   ,tmp))))
          ((vector-length ,x) (let* ((x (Value x)))
                                `(mref ,x ,offset-vector-length)))
          ((vector-ref ,x ,y) (let* ((x (Value x))
                                     (y (Value y)))
                                `(mref ,x (+ ,offset-vector-data ,y))))
          ((void) (Immediate 'void))
          ((,x ,y ,z) (guard (binop? x)) (let* ((y (Value y))
                                                (z (Value z)))
                                           `(,x ,y ,z)))
          (,else else)))
      
      (define (Value exp)
        (match exp
          ((if ,x ,y ,z) (let* ((x (Pred x))
                                (y (Value y))
                                (z (Value z)))
                           `(if ,x ,y ,z)))
          ((begin ,x ... ,y) (let ((x (Effect* x)))
                               `(begin ,x ... ,(Value y))))
          ((let (,x ...) ,y) (let* ((x (Value* x)))
                               `(let (,x ...) ,(Value y))))
          ((quote ,x) (Immediate x))
          ((,x ,y ...) (guard (value-prim? x)) (Value-Prim exp))
          ((,x ...) (Value* x))
          (,else else)))
      
      (define (Pred-Prim exp)
        (match exp
          ((fixnum? ,x) (let* ((mask mask-fixnum)
                              (tag tag-fixnum)
                              (x (Value x)))
                         `(= (logand ,x ,mask) ,tag)))
          ((null? ,x) (let* ((mask #b11111111)
                             ;; Tag value of null can be determined by applying mask to null
                             (tag (logand $nil mask))
                             (x (Value x)))
                        `(= (logand ,x ,mask) ,tag)))
          ((pair? ,x) (let* ((mask mask-pair)
                             (tag tag-pair)
                             (x (Value x)))
                        `(= (logand ,x ,mask) ,tag)))
          ((vector? ,x) (let* ((mask mask-vector)
                               (tag tag-vector)
                               (x (Value x)))
                          `(= (logand ,x ,mask) ,tag)))
          ((boolean? ,x) (let* ((mask mask-boolean)
                                (tag tag-boolean)
                                (x (Value x)))
                           `(= (logand ,x ,mask) ,tag)))
          ((eq? ,x ,y) (let* ((x (Value x))
                              (y (Value y)))
                         `(= ,x ,y)))
          ((,x ,y ,z) (guard (relop? x)) (let* ((y (Value y))
                                                (z (Value z)))
                                           `(,x ,y ,z)))
          (,else else)))
      
      (define (Pred exp)
        (match exp
          ((if ,x ,y ,z) (let* ((x (Pred x))
                                (y (Pred y))
                                (z (Pred z)))
                           `(if ,x ,y ,z)))
          ((begin ,x ... ,y) (let ((x (Effect* x)))
                               `(begin ,x ... ,(Pred y))))
          ((let (,x ...) ,y) (let* ((x (Value* x)))
                               `(let (,x ...) ,(Pred y))))        
          ((,x ,y ...) (guard (pred-prim? x)) (Pred-Prim exp))
          (,else  else)))

      (define (Effect* expls)
        (cond
         ((null? expls) expls)
         (else (let ((x (Effect (car expls))))
                 (append `(,x) (Effect* (cdr expls)))))))
      
      (define (Effect-Prim exp)
        (match exp
          ((set-car! ,x ,y) (let* ((x (Value x))
                                   (y (Value y)))
                              `(mset! ,x ,offset-car ,y)))
          ((set-cdr! ,x ,y) (let* ((x (Value x))
                                   (y (Value y)))
                              `(mset! ,x ,offset-cdr ,y)))
          ((vector-set! ,x ,y ,z) (let* ((x (Value x))
                                         (y (Value y))
                                         (z (Value z)))
                                    `(mset! ,x (+ ,offset-vector-data ,y) ,z)))))
      
      (define (Effect exp)
        (match exp
          ((if ,x ,y ,z) (let* ((x (Pred x))
                                (y (Effect y))
                                (z (Effect z)))
                           `(if ,x ,y ,z)))
          ((begin ,x ... ,y) (let ((x (Effect* x)))
                               `(begin ,x ... ,(Effect y))))       
          ((let (,x ...) ,y) (let* ((x (Value* x)))
                               `(let (,x ...) ,(Effect y))))
          ((,x ,y ...) (guard (effect-prim? x)) (Effect-Prim exp))
          ((,x ...) (Value* x))
          (,else  else)))

      ;; Tail is now value
      (define (Tail exp)
        (Value exp))
      
      (define (Exp exp)                   ;get-trace-define
        (match exp
          ((,x (lambda (,y ...) ,z)) (let ((z (Tail z)))
                                       `(,x (lambda (,y ...) ,z))))))
      
      (define (Program exp)                   ;get-trace-define
        (match exp
          ((letrec (,[Exp -> x] ...) ,z) (let ((z (Tail z)))
                                           `(letrec (,x ...) ,z)))))

      (define (specify-representation exp)                   ;get-trace-define
        (Program exp))
      
     (begin
       ;; Need to determine what the count should be
       (unique-name-count 1000)
       (specify-representation program)))))
