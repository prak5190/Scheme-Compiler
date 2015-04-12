(library (Compiler introduce-procedure-primitives)
  (export
   introduce-procedure-primitives
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (introduce-procedure-primitives program)
    
    (define (Exp* expls)
      (map Exp expls))    
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) (bind-free ,ls ,z)))
           `(,(unique-label x) (lambda (,y ...) ,(Expr z ls))))))
    
    (define (Expr* exp ls)
      (map (lambda(x) (Expr x ls)) exp))

    (define (Closure* ls)
      (map (lambda(x)
             `(,(car x) (make-procedure ,(cadr x) (quote ,(length (cdr (cdr x))))))) ls))
    
    (define (get-procedure-set proc xls ls)
      (let loop ((xls xls) (i 0))
        (if (null? xls)
            `()            
            (let ((x (car xls)))              
              (cons `(procedure-set! ,proc (quote ,i) ,(Expr x ls)) (loop (cdr xls) (add1 i)))))))
    
    (define (get-proc-set* lls ls)
      (fold-left (lambda(s x)
                   (append s (get-procedure-set (car x) (cdr (cdr x)) ls))) '() lls))
    
    (define (Expr exp ls)
      (match exp
        ((if ,x ,y ,z) (let* ((x (Expr x ls))
                              (y (Expr y ls))
                              (z (Expr z ls)))
                         `(if ,x ,y ,z)))
        ((begin ,x ...) `(begin ,(Expr* x ls)... ))
        ((let ((,x ,y) ...) ,z) `(let ,(map (lambda(x y) `(,x ,y)) x (Expr* y ls)) ,(Expr z ls)))
        ((letrec (,x ...) (closures ,lls ,y)) (let* ((x (Exp* x))
                                                    (make-proc-ls (Closure* lls))
                                                    (proc-set-ls (get-proc-set* lls ls)))
                                               `(letrec (,x ...) (let ,make-proc-ls
                                                                   ,(make-begin (append proc-set-ls `(,(Expr y ls))))))))
        ((quote ,x)  exp)
        ((,x ,y ...) (guard (prim? x)) `(,x ,(Expr* y ls) ...))
        ((,x ,y ,z ...) (guard (and (uvar? x) (eqv? x y)))
         (let ((x (Expr x ls)))
           `((procedure-code ,x) ,x ,(Expr* z ls) ...)))
        ((,x  ...) (Expr* x ls))
        (,x (guard (uvar? x)) (cond
                               ((null? ls) x)
                               ((memq x (cdr ls)) => (lambda(r) `(procedure-ref ,(car ls) (quote ,(sub1 (- (length ls) (length r)))))))
                               (else x)))
        (,else else)))
           
    (define (Program exp)                   ;get-trace-define
      (Expr exp '()))

    (define (introduce-procedure-primitives exp)                   ;get-trace-define
      (Program exp))
    
    (introduce-procedure-primitives program)))
