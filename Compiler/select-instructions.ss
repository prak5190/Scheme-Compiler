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
        ((,x ,y ,z) (values ls exp))
        ;; The else case - applies to true, false - Do nothing
        (,x (values ls exp))))

    (define (Effect exp ls tls)                   ;get-trace-define
      (match exp
        [(if ,x ,y ,z) (let*-values (((ls x) (Pred x ls tls))
                                     ((ls y) (Effect y ls tls))
                                     ((ls z) (Effect z ls tls)))
                         (values ls `(if ,x ,y ,z)))]
        [(begin ,x ...) (Effect* x ls tls)]
        [(set! ,v (,b ,t1 ,t2)) (values ls exp)]
        [(set! ,v ,t) (values ls exp)]
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
