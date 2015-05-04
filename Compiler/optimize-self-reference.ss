(library (Compiler optimize-self-reference)
  (export
   optimize-self-reference
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler measurements)
    (Compiler common))
  
  (define-who (optimize-self-reference program)    
    (define (Exp* expls ls)
      (map (lambda (x) (Exp x ls)) expls))
    
    (define (Exp exp ls)                   ;get-trace-define
      (match exp
        ((,x (lambda (,cp ,y ...) (bind-free ,bf ,z))) (let ((bf (filter (lambda(y) (not (eqv? x (unique-label y)))) bf)))
                                                              `(,x (lambda (,cp ,y ...) (bind-free ,bf ,(Expr z `((,x ,cp))))))))))
    
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
        ((letrec (,x ...) (closures ,lls ,y))
         (let* ((lls (map (lambda(x)
                            (cons (car x) (cons (cadr x)
                                                (filter (lambda(y) (not (eqv? y (car x)))) (cddr x))))) lls))
                (x (map (lambda(x) (Exp x ls)) x)))                                                   
           `(letrec (,x ...) (closures ,(Expr* lls ls) ,[Expr y ls]))))
        ((quote ,x)  exp)
        ((,x ,y ...) (guard (prim? x)) `(,x ,(Expr* y ls) ...))
        ((,x ,y ,z ...) (guard (and (uvar? x) (eqv? x y))) (let ((y1 (Expr y ls))
                                                                 (z (Expr* z ls)))
                                                             (if (eqv? y1 y)
                                                                 `(,x ,y1 ,z ...)
                                                                 `(,(unique-label x) ,y1  ,z ...))))
        ((,x ,z ...) (guard (and (uvar? x))) `(,x ,(Expr* z ls) ...))
        ((,x  ...) (Expr* x ls))
        (,x (guard (uvar? x)) (cond
                               ((assq (unique-label x) ls) => (lambda(r) (cadr r)))
                               (else x)))
        (,else else)))
           
    (define (Program exp)                   ;get-trace-define
      (Expr exp '()))

    (define (optimize-self-reference exp)                   ;get-trace-define
      (Program exp))
    
    (begin
      (optimize-self-reference program)
      (analyze-closure-size program))))
