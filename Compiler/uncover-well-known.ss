(library (Compiler uncover-well-known)
  (export
   uncover-well-known
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (uncover-well-known program)        
    (define (Exp* expls ls)
      (cond
       ((null? expls) (values expls ls))
       (else (let*-values (((x ls) (Exp (car expls) ls))
                           ((y ls) (Exp* (cdr expls) ls)))
               (values (cons x y) ls)))))
    
    (define (Exp exp ls)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) (bind-free ,bf ,z))) (let-values (((z ls) (Expr z ls)))                                                     
                                                     (values `(,x (lambda (,y ...) (bind-free ,bf ,z))) ls)))))
    
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
                                  (values `(let ,(map (lambda(x y) `(,x ,y)) x y) ,z) ls)))
        ((letrec (,x ...) (closures ,lls ,y)) (let ((vars (fold-left (lambda(s x) (cons (car x) s)) '() lls)))
                                                (let*-values (((x ls) (Exp* x ls))
                                                              ((y ls) (Expr y (union vars ls))))
                                                  (let ((ls (difference ls vars))
                                                        (wls (intersection ls vars)))
                                                    (values `(letrec (,x ...) (closures ,lls (well-known ,wls ,y))) ls)))))
        ((quote ,x) (values exp ls))
        ((,x ,y ...) (guard (prim? x)) (let-values (((y ls) (Expr* y ls)))
                                         (values `(,x ,y ...) ls)))
        ;; Dont run Expr on the ones called as direct procedure so that it doesn't get deleted from list because of this
        ((,x ,y ,z ...) (guard (and (label? x) (uvar? y))) (let*-values (((z ls) (Expr* z ls)))
                                                             (values `(,x ,y ,z ...) ls)))
        ;; This probably will never happen
        ((,x ...) (let-values (((x ls) (Expr* x ls)))
                    (values `(,x ...) ls)))
        ;; Delete all found vars from the list
        (,x (guard (uvar? x)) (values x (difference ls `(,x))))
        (,else (values else ls))))
           
    (define (Program exp)                   ;get-trace-define
      (let-values (((exp ls) (Expr exp '())))
        exp))

    (define (uncover-well-known exp)                   ;get-trace-define
      (Program exp))
    
    (uncover-well-known program)))
