(library (Compiler sanitize-binding-forms)
  (export
   sanitize-binding-forms
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (sanitize-binding-forms program)        
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,[Expr -> z])) `(,x (lambda (,y ...) ,z)))))

    (define (is-lambda-exp? x)
      (match x
        ((lambda (,x ...) ,z) #t)
        (,else #f)))
    
    (define (Expr exp)
      (match exp
        ((if ,[Expr -> x] ,[Expr -> y] ,[Expr -> z]) `(if ,x ,y ,z))
        ((begin ,[Expr -> x] ...) `(begin ,x ...))
        ((let ((,x ,[Expr -> y]) ...) ,[Expr -> z]) (let-values (((recls letls) (partition
                                                                                 (lambda(x)                        
                                                                                   (is-lambda-exp? (cadr x)))
                                                                                 (map (lambda(x y) `(,x  ,y)) x y))))
                                                      `(letrec ,recls (let ,letls ,z))))
        ((letrec (,[Exp -> x] ...) ,[Expr -> y]) `(letrec (,x ...) ,y))        
        ((lambda (,x ...) ,[Expr -> z]) `(lambda (,x ...) ,z))
        ((quote ,x) exp)
        ((,x ,[Expr -> y] ...) (guard (prim? x)) `(,x ,y ...))
        ((,[Expr -> x] ...) `(,x ...))
        (,x (guard (uvar? x)) x)
        (,else else)))
    
    (define (Program exp)                   ;get-trace-define
      (Expr exp))

    (define (sanitize-binding-forms exp)                   ;get-trace-define
      ;; (unique-name-count 700)
      (Program exp))
    
    (sanitize-binding-forms program)))
