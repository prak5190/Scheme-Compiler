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
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,[Expr -> z])) `(,x (lambda (,y ...) ,z)))))

    ;; A special form so that lambdas bound to let are not changed
    (define (NExpr exp)
      (match exp               
        ((lambda (,x ...) ,[Expr -> z]) `(lambda (,x ...) ,z))        
        (,else (Expr else))))
    
    (define (Expr exp)
      (match exp
        ((if ,[Expr -> x] ,[Expr -> y] ,[Expr -> z]) `(if ,x ,y ,z))
        ((begin ,[Expr -> x] ...) `(begin ,x ...))
        ((let ((,x ,[NExpr -> y]) ...) ,[Expr -> z]) (let ((ls (map (lambda(x y) `(,x ,y)) x y)))
                                           `(let ,ls ,z)))
        ((letrec (,[Exp -> x] ...) ,[Expr -> y]) `(letrec (,x ...) ,y))        
        ((lambda (,x ...) ,[Expr -> z]) (let ((uniq (unique-name 'anon)))                                          
                                          `(letrec ((,uniq (lambda (,x ...) ,z)))
                                             ,uniq)))
        ((quote ,x) exp)
        ((,x ,[Expr -> y] ...) (guard (prim? x)) `(,x ,y ...))
        ((,[Expr -> x] ...) `(,x ...))
        (,x (guard (uvar? x)) x)
        (,else else)))
    
    (define (Program exp)                   ;get-trace-define
      (Expr exp))

    (define (remove-anonymous-lambda exp)                   ;get-trace-define
      (unique-name-count 700)
      (Program exp))
    
    (remove-anonymous-lambda program)))
