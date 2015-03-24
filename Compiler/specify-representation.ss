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
    (define (value-prim? exp)                   ;get-trace-define
      (define value-prim '((+ 2) (- 2) (* 2) (car 1) (cdr 1) (cons 2) (make-vector 1) (vector-length 1) (vector-ref 2) (void 0)))
      (and (assq exp value-prim) #t))
    
    (define (effect-prim? exp)                   ;get-trace-define
      (define effect-prim '((set-car! 2) (set-cdr! 2) (vector-set! 3)))
      (and (assqq exp effect-prim) #t))
    
    (define (pred-prim? exp)                   ;get-trace-define
      (define pred-prim '((< 2) (<= 2) (= 2) (>= 2) (> 2) (boolean 1) (eq 2) (fixnum 1) (null 1) (pair 1) (vector 1)))
      (and (assq exp pred-prim) #t))
    
    (define (Value* expls)
      (cond
       ((null? expls) expls)
       (else (let ((x (Value (car expls))))
               (append `(,x) (Value* (cdr expls)))))))

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
        ((,x ,y ,z) (guard (binop? x)) (let* ((y (Value y))
                                              (z (Value z)))
                                         `(,x ,y ,z)))
        ((quote ,x) x)
        ((,x ,y ...) (guard (value-prim? x)) exp)
        ((,x ...) (Value* x))
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
        ((,x ,y ,z) (guard (relop? x)) (let* ((y (Value y))
                                              (z (Value z)))
                                         `(,x ,y ,z)))
        ((,x ...) (guard (pred-prim? x)) exp)
        (,else  else)))

    (define (Effect* expls)
      (cond
       ((null? expls) expls)
       (else (let ((x (Effect (car expls))))
               (append `(,x) (Effect* (cdr expls)))))))
    
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
        ((,x ...) (guard (effect-prim? x)) exp)
        ((,x ...) (Value* x))
        (,else  else)))

    ;; Tail is now value
    (define (Tail exp)
      (Value exp))
    
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,z)) (let ((z (Tail z)))
                                      `(,x (lambda (,y ...) (locals ,z)))))))
    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,z) (let ((z (Tail z)))
                                         `(letrec (,x ...) ,z)))))

    (define (specify-representation exp)                   ;get-trace-define
      (Program exp))    
    (specify-representation program)))
