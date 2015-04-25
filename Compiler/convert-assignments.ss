(library (Compiler convert-assignments)
  (export
   convert-assignments
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (convert-assignments program)
    (define (Exp exp ls)                   ;get-trace-define
      (match exp
        ((,y (lambda (,x ...) (assigned ,als ,z))) (let* ((new-x (map (lambda(x) (unique-name (string->symbol (extract-root x)))) x))
                                                          (cons-ls (map (lambda (x y) `(,x (cons ,y (void)))) x new-x))
                                                          (z (Expr z (union als ls))))
                                                     `(,y (lambda (,new-x ...) (let ,cons-ls  ,z)))))))
    
    (define (Expr exp ls)
      (let* ((OExpr Expr)
             (Expr (lambda(x) (Expr x ls))))
        (match exp
          ((if ,[Expr -> x] ,[Expr -> y] ,[Expr -> z]) `(if ,x ,y ,z))
          ((begin ,[Expr -> x] ...) `(begin ,x ...))
          ((let ((,x ,y) ...) (assigned ,als ,z)) (let* ((y (map (lambda(y) (Expr y)) y))
                                                         (tx-ls (map (lambda(y x) `(,(unique-name (string->symbol (extract-root x))) y)) y x))
                                                         (cons-ls (map (lambda (x y) `(,x (cons ,y (void)))) x (map car tx-ls)))
                                                         (z (OExpr z (union als ls))))
                                                    `(let ,tx-ls (let ,cons-ls ,z))))
          ((letrec ((,x ,[Expr -> z]) ...) ,[Expr -> y]) (let ((rls (map (lambda(x z) `(,x ,z)) x z)))
                                                           `(letrec ,rls ,y)))
          ((lambda (,x ...) (assigned ,als ,z)) (let* ((new-x (map (lambda(x) (unique-name (string->symbol (extract-root x)))) x))
                                                       (cons-ls (map (lambda (x y) `(,x (cons ,y (void)))) x new-x))
                                                       (z (OExpr z (union als ls))))
                                                  `(lambda (,new-x ...) (let ,cons-ls  ,z))))
          ((quote ,x) exp)
          ((set! ,[Expr -> y] ...) `(set-car! ,y ...))
          ((,x ,[Expr -> y] ...) (guard (prim? x)) `(,x ,y ...))
          ((,[Expr -> x] ...) `(,x ...))
          (,x (guard (uvar? x)) (if (memq x ls)
                                    `(car ,x)
                                    x))
          (,else else))))
    
    (define (Program exp)                   ;get-trace-define
      (Expr exp '()))

    (define (convert-assignments exp)                   ;get-trace-define
      (Program exp))
    
    (convert-assignments program)))
