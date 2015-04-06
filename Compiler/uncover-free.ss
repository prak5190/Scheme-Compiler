(library (Compiler uncover-free)
  (export
   uncover-free
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (uncover-free program)        
    (define (Exp* expls ls)
      (cond
       ((null? expls) (values expls ls))
       (else (let*-values (((x ls) (Exp (car expls) ls))
                           ((y ls) (Exp* (cdr expls) ls)))
               (values (cons x y) ls)))))

    (define (Exp exp ls)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,z)) (let-values (((z ls) (Expr z ls)))
                                     (values `(,x (lambda (,y ...) (free ,(difference ls y) ,z))) (difference ls y))))))
    
    (define (Expr* expls ls)
      (cond
       ((null? expls) (values '() ls))
       (else (let*-values (((x ls) (Expr (car expls) ls))
                           ((y ls) (Expr* (cdr expls) ls)))
               (values (cons x y) ls)))))
    
    (define (Expr exp ls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((x ls) (Expr x ls))
                                     ((y ls) (Expr y ls))
                                     ((z ls) (Expr z ls)))
                         (values `(if ,x ,y ,z) ls)))
        ((begin ,x ...) (let*-values (((x ls) (Expr* x ls)))
                          (values `(begin ,x ... ) ls)))
        ((let ((,x ,y) ...) ,z) (let*-values (((y ls) (Expr* y ls))
                                              ((z ls) (Expr z ls)))
                                  (values `(let ,(map (lambda(x y) `(,x ,y)) x y) ,z) (difference ls x))))
        ((letrec (,x ...) ,y) (let*-values (((x ls) (Exp* x ls))
                                            ((y ls) (Expr y ls)))
                                (values `(letrec (,x ...) ,y) ls)))
        ((quote ,x) (values exp ls))
        ((,x ,y ...) (guard (prim? x)) (let-values (((y ls) (Expr* y ls)))
                                         (values `(,x ,y ...) ls)))
        ((,x ...) (let-values (((x ls) (Expr* x ls)))
                    (values `(,x ...) ls)))
        (,x (guard (uvar? x)) (values x (cons x ls)))
        (,else (values else ls))))
           
    (define (Program exp)                   ;get-trace-define
      (let-values (((exp ls) (Expr exp '())))
        exp))

    (define (uncover-free exp)                   ;get-trace-define
      (Program exp))
    
    (uncover-free program)))
