(library (Compiler optimize-direct-call)
  (export
   optimize-direct-call
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (optimize-direct-call program)        
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,[Expr -> z])) `(,x (lambda (,y ...) ,z)))))
        
    (define (Expr exp)
      (match exp
        ((if ,[Expr -> x] ,[Expr -> y] ,[Expr -> z]) `(if ,x ,y ,z))
        ((begin ,[Expr -> x] ...) `(begin ,x ...))
        ((let ((,x ,[Expr -> y]) ...) ,[Expr -> z]) (let ((ls (map (lambda(x y) `(,x ,y)) x y)))
                                           `(let ,ls ,z)))
        ((letrec (,[Exp -> x] ...) ,[Expr -> y]) `(letrec (,x ...) ,y))        
        (((lambda (,x ...) ,[Expr -> z]) ,y ...) (if (eqv? (length x) (length y))
                                                     (let ((ls (map (lambda(x y) `(,x ,y)) x y)))
                                                       `(let ,ls ,z))))
        ((lambda (,x ...) ,[Expr -> z]) `(lambda (,x ...) ,z))
        ((quote ,x) exp)
        ((,x ,[Expr -> y] ...) (guard (prim? x)) `(,x ,y ...))
        ((,[Expr -> x] ...) `(,x ...))
        (,x (guard (uvar? x)) x)
        (,else else)))
    
    (define (Program exp)                   ;get-trace-define
      (Expr exp))

    (define (optimize-direct-call exp)                   ;get-trace-define
      (Program exp))
    
    (optimize-direct-call program)))
