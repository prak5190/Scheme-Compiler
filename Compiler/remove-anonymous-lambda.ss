(library (Compiler remove-anonymous-lambda)
  (export
   remove-anonymous-lambda
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (remove-anonymous-lambda program)        
    (define (Exp* expls ls)
      (cond
       ((null? expls) (values expls ls))
       (else (let*-values (((x l1) (Exp (car expls) ls))
                           ((y l2) (Exp* (cdr expls) ls)))
               (values (cons x y) (union l1 l2))))))

    (define (Exp exp ls)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,z)) (let-values (((z ls) (Expr z ls)))
                                     (values `(,x (lambda (,y ...) (free ,(difference ls y) ,z))) (difference ls y))))))
    
    (define (Expr* expls ls)
      (cond
       ((null? expls) (values '() '()))
       (else (let*-values (((x l1) (Expr (car expls) ls))
                           ((y l2) (Expr* (cdr expls) ls)))
               (values (cons x y) (union l1 l2))))))
    
    (define (Expr exp ls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((x l1) (Expr x ls))
                                     ((y l2) (Expr y ls))
                                     ((z l3) (Expr z ls)))
                         (values `(if ,x ,y ,z) (union (union l1 l2) l3))))
        ((begin ,x ...) (let*-values (((x l1) (Expr* x ls)))
                          (values `(begin ,x ... ) l1)))
        ((let ((,x ,y) ...) ,z) (let*-values (((y l1) (Expr* y ls))
                                              ((z l2) (Expr z ls)))
                                  (values `(let ,(map (lambda(x y) `(,x ,y)) x y) ,z) (difference (union l1 l2) x))))
        ((lambda (,y ...) ,z) )
        ((letrec (,x ...) ,y) (let*-values (((x l1) (Exp* x ls))
                                            ((y l2) (Expr y ls)))
                                (values `(letrec (,x ...) ,y) (difference (union l1 l2) (map car x)))))
        ((quote ,x) (values exp ls))
        ((,x ,y ...) (guard (prim? x)) (let-values (((y l1) (Expr* y ls)))
                                         (values `(,x ,y ...) l1)))
        ((,x ...) (let-values (((x l1) (Expr* x ls)))
                    (values `(,x ...) l1)))
        (,x (guard (uvar? x)) (values x  `(,x)))
        (,else (values else '()))))
           
    (define (Program exp)                   ;get-trace-define
      (let-values (((exp ls) (Expr exp '())))
        exp))

    (define (remove-anonymous-lambda exp)                   ;get-trace-define
      (Program exp))
    
    (remove-anonymous-lambda program)))
