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
  
  (define (var? exp)                   ;get-trace-define
    (or (register? exp) (frame-var? exp) (uvar? exp)))
  
  (define-who (select-instructions program)
    ;; Is it a tranisitive binary operator ?
    (define (is-commu-binop? x)
      (memq x '(+ *)))
    ;; Gets a unique unspillable
    ;; (define (get-unique-name ls c)
    ;;   (let ((n (unique-label c)))      
    ;;     (if (memq (string->number (extract-suffix n)) (labelLs->suffx ls))
    ;;         (begin
    ;;           (get-unique-name ls c))              
    ;;         n)))

    ;; All e- method swap exp with new instructions when it violates machine constraints
    ;; Handle frame var and int64 constraint
    (define (e-frame exp ls tls)
      (values ls exp))
    
    ;; make v=t1 (set! v (b t1 t2))
    (define (e-equ exp ls tls)
      (values ls exp))
    
    ;; Handle Multiplication case for frame-vars i.e * v t
    (define (e-mult exp ls tls)
      (values ls exp))

    
    (define (enforce-mc-s1 exp ls tls)
      (match exp                
        ((set! ,v ,t)
         (guard (or
                 (and (frame-var? v) (frame-var? t))
                 (and (frame-var? v) (is-int64? t)))) (e-frame exp ls tls))
        ;; If it does not violate any machine constraint then just return it        
        (,else (values ls exp))))
    
    (define (enforce-mc-s2 exp ls tls)
      (match exp
        ((set! ,v (,b ,t1 ,t2))
         (guard (not (eqv? v t1))) (if (and (is-commu-binop? b) (eqv? v t2))
                                       (enforce-mc-s2 `(set! ,v (,b ,t2 ,t1)) ls tls)
                                       (e-equ exp ls tls)))        
        ;; V = t1 now                
        ((set! ,v (sra ,t1 ,t)) (values ls exp))
        ((set! ,v (* ,t1 ,t)) (guard (frame-var? v))  (e-mult exp ls tls))
        ((set! ,v (,b ,t1 ,t))
         (guard (or
                 (and (frame-var? v) (frame-var? t))
                 (and (frame-var? v) (is-int64? t)))) (e-frame exp ls tls))        
        ;; If it does not violate any machine constraint then just return it        
        (,else (values ls exp))))

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
                          (and (frame-var? y) (frame-var? z))) (e-x5 exp ls tls))
                     ((and (is-int64? z) (int32? y)) (e-x6 exp ls tls))
                     ((and (is-int64? y) (is-int64? z)) (e-x7 exp ls tls))
                     (else (values ls exp))))))                      

    
    ;; Validate Pred
    (define (Pred exp ls tls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((ls x) (Pred x ls tls))
                                     ((ls y) (Pred y ls tls))
                                     ((ls z) (Pred z ls tls)))
                         (values ls `(if ,x ,y ,z))))
        ((begin ,x ... ,p) (let*-values
                               (((ls xls) (Effect* x ls tls))
                                ((ls p) (Pred p ls tls)))                       
                             (values ls `(begin ,xls ... ,p))))
        ((,x ,y ,z) (enforce-mc-p exp ls tls))
        ;; The else case - applies to true, false - Do nothing
        (,x (values ls exp))))
    
    (define (Effect exp ls tls)                   ;get-trace-define
      (match exp
        [(if ,x ,y ,z) (let*-values (((ls x) (Pred x ls tls))
                                     ((ls y) (Effect y ls tls))
                                     ((ls z) (Effect z ls tls)))
                         (values ls `(if ,x ,y ,z)))]
        [(begin ,x ...) (Effect* x ls tls)]
        [(set! ,v (,b ,t1 ,t2)) (enforce-mc-s2 exp ls tls)]
        [(set! ,v ,t) (enforce-mc-s1 exp ls tls)]
        [,else (values ls exp)]))

    
    (define (Effect* ex ls tls)
      (match ex
        ((,x ,y ...) (let*-values
                         (((ls x) (Effect x ls tls))
                          ((ls yls) (Effect* y ls tls)))                       
                      (values ls `(,x ,yls ...))))
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
                         (values ls `(if ,x ,y ,z))))
        ((,x ,y ...) (values ls exp))))
    ;; Validate Body
    (define (Body exp)
      (match exp
        ((locals ,x (ulocals ,a (locate ,b (frame-conflict ,c ,y))))
         (let-values (((ls exp) (Tail y '() x)))
           `(locals ,x (ulocals ,(union ls a) (locate ,b (frame-conflict ,c ,exp))))))))

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
