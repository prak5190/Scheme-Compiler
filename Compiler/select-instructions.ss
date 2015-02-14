(library (Compiler select-instructions)
  (export
   select-instructions
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define (var? exp)                   ;get-define
    (or (register? exp) (frame-var? exp) (uvar? exp)))
  
  (define (relop? exp)                   ;get-trace-define
    (define relops '(< > = <= >=))
    (and (memq exp relops) #t))
  
  (define-who (select-instructions program)
    ;; Is it a tranisitive binary operator ?
    (define (is-commu-binop? x)
      (memq x '(+ * logand logor)))
    
    (define (labelLs->suffx ls)
      (map (lambda(x) (string->number (extract-suffix x))) ls))
    ;; Gets a unique unspillable
    (define (get-unique-name ls)
      (let ((n (unique-name 't)))      
        (if (memq (string->number (extract-suffix n)) (labelLs->suffx ls))
            (begin
              (get-unique-name ls))              
            n)))
    
    (define (add-begin ex)
      (match ex
        ((,x ,y ,z) (guard (relop? x)) ex)
        ((,x) x)
        ((if ,x ,y ,z) `(if ,x ,y ,z))
        ((begin ,x ...) `(begin ,x ...))
        ((,x ,y ...) `(begin ,x ,y ...))))
        
    (define (enforce-mc-s1 exp ls tls)
      (match exp                
        ((set! ,v ,t)
         (guard (or
                 (and (frame-var? v) (frame-var? t))
                 (and (frame-var? v) (is-int64? t))))
         (let ((n (get-unique-name (append ls tls))))
           (values (cons n ls) `((set! ,n ,t) (set! ,v ,n)))))
        ;; If it does not violate any machine constraint then just return it        
        (,else (values ls `(,exp)))))
    
    (define (enforce-mc-s2 exp ls tls)
      (match exp
        ((set! ,v (,b ,t1 ,t2))
         (guard (not (eqv? v t1)))
         ;; X
         (if (and (is-commu-binop? b) (eqv? v t2))
             (enforce-mc-s2 `(set! ,v (,b ,t2 ,t1)) ls tls)
             (let ((n (get-unique-name (append ls tls))))
               (values (cons n ls) `((set! ,n ,t1)
                                     (set! ,n (,b ,n ,t2))
                                     (set! ,v ,n))))))
        ;; V = t1 now                
        ((set! ,v (sra ,t1 ,t)) (values ls `(,exp)))
        ;; X2
        ((set! ,v (* ,t1 ,t))
         (guard (frame-var? v))
         (let ((u (get-unique-name (append ls tls))))
           (values (cons u ls)
                   `((set! ,u ,v)
                     (set! ,u (* ,u ,t))
                     (set! ,v ,u)))))
        ;; X1
        ((set! ,v (,b ,t1 ,t))
         (guard (or
                 (and (frame-var? v) (frame-var? t))
                 (and (or (frame-var? v) (eqv? b '*)) (is-int64? t))))
         (let ((u (get-unique-name (append ls tls))))
           (values (cons u ls)
                   `((set! ,u ,t) (set! ,v (,b ,v ,u))))))
        
        ;; If it does not violate any machine constraint then just return it        
        (,else (values ls `(,exp)))))

    ;; Returns inverse of operator
    (define (inverse x)
      (car (assq x '((< >=) (> <=) (>= <) (<= >) (= =)))))
    
    ;; Pred enforcers
    (define (e-x5 exp ls tls)
      (values ls exp))
    (define (e-x6 exp ls tls)
      (values ls exp))
    (define (e-x7 exp ls tls)
      (values ls exp))
    
    (define (enforce-mc-p exp ls tls)
      (match exp                        
        ;; If it does not violate any machine constraint then just return it
        ((,x ,y ,z) (cond
                     ;; X4                     
                     ((and (int32? y) (var? z)) (values ls `(,(inverse x) ,z ,y)))
                     ;; X5
                     ((or (and (is-int64? y) (not (is-int64? z)))
                          (and (int32? y) (int32? z))
                          (and (frame-var? y) (frame-var? z)))
                      (let ((u (get-unique-name (append ls tls))))
                        (values (cons u ls)
                                `(begin
                                   (set! ,u ,y)
                                   (,x ,u ,z)))))
                     ;; X6
                     ((and (is-int64? z) (int32? y))
                      (let ((u (get-unique-name (append ls tls))))
                        (values (cons u ls)
                                `(begin
                                   (set! ,u ,z)
                                   (,(inverse x) ,u ,y)))))
                     ;; X7
                     ((and (is-int64? y) (is-int64? z))
                      (let* ((u1 (get-unique-name (append ls tls)))
                            (u2 (get-unique-name (cons u1 (append ls tls)))))
                        (values (cons u2 (cons u1 ls))
                                `(begin
                                   (set! ,u1 ,y)
                                   (set! ,u2 ,z)
                                   (,x ,u1 ,u2)))))
                     (else (values ls `(,exp)))))))                      

    
    ;; Validate Pred
    (define (Pred exp ls tls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((ls x) (Pred x ls tls))
                                     ((ls y) (Pred y ls tls))
                                     ((ls z) (Pred z ls tls)))                         
                         (values ls `(if ,(add-begin x) ,(add-begin y) ,(add-begin z)))))
        ((begin ,x ... ,p) (let*-values
                               (((ls xls) (Effect* x ls tls))
                                ((ls p) (Pred p ls tls)))                       
                             (values ls `(begin ,xls ... ,p ...))))
        ((,x ,y ,z) (enforce-mc-p exp ls tls))
        ;; The else case - applies to true, false - Do nothing
        (,x (values ls `(,exp)))))

    (define (Effect exp ls tls)                   ;get-define
      (match exp
        [(if ,x ,y ,z) (let*-values (((ls x) (Pred x ls tls))
                                     ((ls y) (Effect y ls tls))
                                     ((ls z) (Effect z ls tls)))                         
                         (values ls `((if ,(add-begin x) ,(add-begin y) ,(add-begin z)))))]
        [(begin ,x ...) (Effect* x ls tls)]
        [(set! ,v (,b ,t1 ,t2)) (enforce-mc-s2 exp ls tls)]
        [(set! ,v ,t) (enforce-mc-s1 exp ls tls)]
        [,else (values ls `(,exp))]))

    
    (define (Effect* ex ls tls)
      (match ex
        ((,x ,y ...) (let*-values
                         (((ls xl) (Effect x ls tls))
                          ((ls yls) (Effect* y ls tls)))                      
                      (values ls `(,xl ... ,yls ...))))
        (,else (values ls ex))))
    
    (define (Tail exp ls tls)                   ;get-trace-define
      (match exp
        ((begin ,x ... ,t) (let*-values
                               (((ls exp) (Effect* x ls tls))
                                ((ls t) (Tail t ls tls)))
                             (values ls `(begin ,exp ... ,t))))
        ((if ,x ,y ,z) (let*-values (((ls x) (Pred x ls tls))
                                     ((ls y) (Tail y ls tls))
                                     ((ls z) (Tail z ls tls)))
                         (values ls `(if ,(add-begin x) ,y ,z))))
        ((,x ,y ...) (values ls exp))))
    ;; Validate Body
    (define (Body exp)
      (match exp
        ((locals ,x (ulocals ,a (locate ,b (frame-conflict ,c ,y))))
         (let-values (((ls exp) (Tail y '() x)))
           `(locals ,x (ulocals ,(union ls a) (locate ,b (frame-conflict ,c ,exp))))))
        ((locate (,x ...) ,y) exp)))

    ;; Validate letrec label exp :   [label (lambda() Tail)]
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x (lambda () ,(Body tail))))))

    ;; Validate Program
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))

    (define (select-instructions exp)                   ;get-trace-define
      (Program exp))
    
    (select-instructions program)))
