(library (Compiler optimize-known-call)
  (export
   optimize-known-call
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (optimize-known-call program)
    
    (define (Exp* expls ls)
      (map (lambda(x)
             (Exp x ls)) expls))
    
    (define (Exp exp ls)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) (bind-free ,fls ,z)))
         `(,x (lambda (,y ...) (bind-free ,fls ,(Expr z ls)))))))
    
    (define (Expr* exp ls)
      (map (lambda(x) (Expr x ls)) exp))

        
    (define (Expr exp ls)
      (match exp
        ((if ,x ,y ,z) (let* ((x (Expr x ls))
                              (y (Expr y ls))
                              (z (Expr z ls)))
                         `(if ,x ,y ,z)))
        ((begin ,x ...) `(begin ,(Expr* x ls)... ))
        ((let ((,x ,y) ...) ,z) `(let ,(map (lambda(x y) `(,x ,y)) x (Expr* y ls)) ,(Expr z ls)))
        ((letrec (,x ...) (closures ,lls ,y)) (let* ((ls (append lls ls))
                                                     (x (Exp* x ls))
                                                     (y (Expr y ls)))
                                                `(letrec (,x ...) (closures ,lls ,y))))
        ((quote ,x)  exp)
        ((,x ,y ...) (guard (prim? x)) `(,x ,(Expr* y ls) ...))
        ((,x ,y ,z ...) (guard (and (uvar? x) (eqv? x y)))
         (let ((z (Expr z ls)))
           (cond
            ((assq x ls) => (lambda (r) (let ((x (cadr r)))
                                          `(,x ,y ,z ...))))
            (else `(,x ,y ,z ...)))))
        ((,x  ...) (Expr* x ls))
        (,x (guard (uvar? x)) exp)
        (,else else)))
           
    (define (Program exp)                   ;get-trace-define
      (Expr exp '()))

    (define (optimize-known-call exp)                   ;get-trace-define
      (Program exp))
    
    (optimize-known-call program)))
