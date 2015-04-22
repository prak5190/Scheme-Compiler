(library (Compiler purify-letrec)
  (export
   purify-letrec
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (purify-letrec program)        
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) (assigned ,ls ,[Expr -> z]))) `(,x (lambda (,y ...) (assigned ,ls ,z))))))

    (define (is-lambda e)
      (match
        
    
    (define (Expr exp)
      (match exp
        ((if ,[Expr -> x] ,[Expr -> y] ,[Expr -> z]) `(if ,x ,y ,z))
        ((begin ,[Expr -> x] ...) `(begin ,x ...))
        ((let ((,x ,[Expr -> y]) ...) (assigned ,ls ,[Expr -> z])) (let ((exls (map (lambda(x y) `(,x ,y)) x y)))
                                                                     `(let ,exls (assigned ,ls ,z))))
        ((letrec (,[Exp -> x] ...) (assigned ,ls ,[Expr -> y])) `(letrec (,x ...) (assigned ,ls ,y)))
        ((lambda (,x ...) (assigned ,ls ,[Expr -> z])) `(lambda (,x ...) (assigned ,ls ,z)))
        ((quote ,x) exp)
        ((,x ,[Expr -> y] ...) (guard (prim? x)) `(,x ,y ...))
        ((,[Expr -> x] ...) `(,x ...))
        (,x (guard (uvar? x)) x)
        (,else else)))
    
    (define (Program exp)                   ;get-trace-define
      (Expr exp))

    (define (purify-letrec exp)                   ;get-trace-define
      (Program exp))
    
    (purify-letrec program)))
