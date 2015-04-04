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
    
  (define-who (select-instructions program)
    ;; Is it a tranisitive binary operator ?
    (define (is-commu-binop? x)
      (memq x '(+ * logand logor)))   
    
    (define (add-begin ex)
      (match ex
        ((,x ,y ,z) (guard (relop? x)) ex)
        ((,x) x)
        ((if ,x ,y ,z) `(if ,x ,y ,z))
        ((begin ,x ...) `(begin ,x ...))
        ((,x ,y ...) `(begin ,x ,y ...))))

    (define (isFrameVar? x ls)
      (if (frame-var? x)
          #t
          (if (uvar? x)
              ;; Search in ls and decide
              (let ((v (assq x ls)))
                (if v
                    (frame-var? (cadr v))
                    #f))
              #f)))
        
    (define (enforce* f expls ls tls loc)
      (match expls
        ((begin ,x ,y ...) (let*-values
                               (((ls xl) (f x ls tls loc))
                                ((ls yls) (enforce* f y ls tls loc)))                      
                             (values ls `(begin ,xl ... ,yls ...))))
        ((,x ,y ...) (let*-values
                         (((ls xl) (f x ls tls loc))
                          ((ls yls) (enforce* f y ls tls loc)))                      
                      (values ls `(,xl ... ,yls ...))))
        (,else (values ls expls))))
    
    (define (enforce-mc-mset exp ls tls loc)
      (match exp
        ((mset! ,x ,y ,z)
         (guard (isFrameVar? x loc))
         (let ((n (get-unique-name (append ls tls))))
           (enforce* enforce-mc-mset  `((set! ,n ,x)
                                        (mset! ,n ,y ,z)) (cons n ls) tls loc)))
        ((mset! ,x ,y ,z)
         (guard (or (isFrameVar? y loc) (is-int64? y)))
         (let ((n (get-unique-name (append ls tls))))
           (enforce* enforce-mc-mset  `((set! ,n ,y)
                                        (mset! ,x ,n ,z))  (cons n ls) tls loc)))
        ((mset! ,x ,y ,z)
         (guard (or (isFrameVar? z loc) (is-int64? z)))
         (let ((n (get-unique-name (append ls tls))))
           (enforce* enforce-mc-mset  `((set! ,n ,z)
                                        (mset! ,x ,y ,n))  (cons n ls) tls loc)))
        (,else (values ls `(,exp)))))
    
    (define (enforce-mc-s2 exp ls tls loc)
      (match exp
        ((set! ,v (mref ,x ,y)) (guard (isFrameVar? v loc))
         (let ((u (get-unique-name (append ls tls))))
           (enforce* enforce-mc-s2 `((set! ,u (mref ,x ,y))
                                            (set! ,v ,u))
                     (cons u ls) tls loc)))
        ((set! ,v (mref ,x ,y)) (guard (isFrameVar? x loc))
         (let ((u (get-unique-name (append ls tls))))
           (enforce* enforce-mc-s2 `((set! ,u ,x) (set! ,v (mref ,u ,y)))
                     (cons u ls) tls loc)))
        ((set! ,v (mref ,x ,y)) (values ls `(,exp)))
        ((set! ,v (,b ,t1 ,t2))
         (guard (and (binop? b) (not (eqv? v t1))))
         ;; X
         (if (and (is-commu-binop? b) (eqv? v t2))
             (enforce-mc-s2 `(set! ,v (,b ,t2 ,t1)) ls tls loc)
             (let ((n (get-unique-name (append ls tls))))
               (enforce* enforce-mc-s2   `((set! ,n ,t1)
                                           (set! ,n (,b ,n ,t2))
                                           (set! ,v ,n)) (cons n ls) tls loc))))
        ;; V = t1 now                
        ((set! ,v (sra ,t1 ,t)) (values ls `(,exp)))
        ;; X2
        ((set! ,v (* ,t1 ,t)) 
         (guard (isFrameVar? v loc))
         (let ((u (get-unique-name (append ls tls))))
           (enforce* enforce-mc-s2 `((set! ,u ,v)
                                     (set! ,u (* ,u ,t))
                                     (set! ,v ,u)) (cons u ls) tls loc)))       
        ;; X1
        ((set! ,v (,b ,t1 ,t))
         (guard (and (binop? b)
                     (or
                      (and (isFrameVar? v loc) (isFrameVar? t loc))
                      (is-int64? t))))
         (let ((u (get-unique-name (append ls tls))))
           (enforce* enforce-mc-s2 
                   `((set! ,u ,t) (set! ,v (,b ,v ,u))) (cons u ls) tls loc)))
        ;; X
        ((set! ,v ,t)
         (guard (or
                 (and (isFrameVar? v loc) (isFrameVar? t loc))
                 (and (isFrameVar? v loc) (is-int64? t))))
         (let ((n (get-unique-name (append ls tls))))
           (enforce* enforce-mc-s2  `((set! ,n ,t) (set! ,v ,n)) (cons n ls) tls loc)))
        ;; Pred Cases 
        ((,x ,y ,z) (guard (relop? x)) (cond
                                        ;; X4                     
                                        ((and (int32? y) (var? z)) (enforce* enforce-mc-s2 `((,(inverse x) ,z ,y)) ls tls loc))
                                        ;; X5
                                        ((or (and (is-int64? y) (not (is-int64? z)))
                                             (and (int32? y) (int32? z))
                                             (and (isFrameVar? y loc) (isFrameVar? z loc)))
                                         (let ((u (get-unique-name (append ls tls))))
                                           (enforce* enforce-mc-s2  
                                                   `(begin
                                                      (set! ,u ,y)
                                                      (,x ,u ,z))(cons u ls) tls loc)))
                                        ;; X6
                                        ((and (is-int64? z) (not (is-int64? y)))
                                         (let ((u (get-unique-name (append ls tls))))
                                           (enforce* enforce-mc-s2  
                                                   `(begin
                                                      (set! ,u ,z)
                                                      (,(inverse x) ,u ,y))(cons u ls) tls loc)))
                                        ;; X7
                                        ((and (is-int64? y) (is-int64? z))
                                         (let* ((u1 (get-unique-name (append ls tls)))
                                                (u2 (get-unique-name (cons u1 (append ls tls)))))
                                           (enforce* enforce-mc-s2  
                                                   `(begin
                                                      (set! ,u1 ,y)
                                                      (set! ,u2 ,z)
                                                      (,x ,u1 ,u2))(cons u2 (cons u1 ls)) tls loc)))
                                        (else (values ls `(,exp)))))
        ;; If it does not violate any machine constraint then just return it        
        (,else (values ls `(,exp)))))

    ;; Returns inverse of operator
    (define (inverse x)
      (cadr (assq x '((< >) (> <) (>= <=) (<= >=) (= =)))))
    
    ;; Validate Pred
    (define (Pred exp ls tls loc)
      (match exp
        ((if ,x ,y ,z) (let*-values (((ls x) (Pred x ls tls loc))
                                     ((ls y) (Pred y ls tls loc))
                                     ((ls z) (Pred z ls tls loc)))                         
                         (values ls `((if ,(add-begin x) ,(add-begin y) ,(add-begin z))))))
        ((begin ,x ... ,p) (let*-values
                               (((ls xls) (Effect* x ls tls loc))
                                ((ls p) (Pred p ls tls loc)))                       
                             (values ls `((begin ,xls ... ,p ...)))))
        ((,x ,y ,z) (enforce-mc-s2 exp ls tls loc))
        ;; The else case - applies to true, false - Do nothing
        (,x (values ls `(,exp)))))
    
    ;; Returns ls and exp
    (define (Effect exp ls tls loc)                   ;get-define
      (match exp
        [(if ,x ,y ,z) (let*-values (((ls x) (Pred x ls tls loc))
                                     ((ls y) (Effect y ls tls loc))
                                     ((ls z) (Effect z ls tls loc)))                         
                         (values ls `((if ,(add-begin x) ,(add-begin y) ,(add-begin z)))))]
        [(begin ,x ...) (Effect* x ls tls loc)]
        [(mset! ,x ,y ,z) (enforce-mc-mset exp ls tls loc)]
        [(set! ,v (,b ,t1 ,t2)) (enforce-mc-s2 exp ls tls loc)]
        [(set! ,v ,t) (enforce-mc-s2 exp ls tls loc)]
        [(return-point ,x ,y) (let-values (((ls y) (Effect y ls tls loc)))
                                (values ls `((return-point ,x ,(make-begin y)))))]
        [,else (values ls `(,exp))]))

    
    (define (Effect* ex ls tls loc)
      (match ex
        ((,x ,y ...) (let*-values
                         (((ls xl) (Effect x ls tls loc))
                          ((ls yls) (Effect* y ls tls loc)))                      
                      (values ls `(,xl ... ,yls ...))))
        (,else (values ls ex))))
    
    (define (Tail exp ls tls loc)                   ;get-trace-define
      (match exp
        ((begin ,x ... ,t) (let*-values
                               (((ls exp) (Effect* x ls tls loc))
                                ((ls t) (Tail t ls tls loc)))
                             (values ls `(begin ,exp ... ,t))))
        ((if ,x ,y ,z) (let*-values (((ls x) (Pred x ls tls loc))
                                     ((ls y) (Tail y ls tls loc))
                                     ((ls z) (Tail z ls tls loc)))
                         (values ls `(if ,(add-begin x) ,y ,z))))
        ((,x ,y ...) (values ls exp))))
    ;; Validate Body
    (define (Body exp)
      (match exp
        ((locals ,x (ulocals ,a (locate ,b (frame-conflict ,c ,y))))
         (let-values (((ls exp) (Tail y '() x b)))
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
