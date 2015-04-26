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

    (define (is-lambda exp)
      (match exp
        ((,x (lambda(,y ...) ,z)) #t)
        (,else #f)))
    
    (define (is-simple exp ls)
      (match exp
        ((,x (lambda(,y ...) ,z)) #f)
        ((,x ,z) (Expr-ls z ls))
        (,else #f)))
    
    (define (Exp xls yls body als)                   ;get-trace-define
      (let* ((expls (map (lambda(x y) `(,x ,y)) xls yls)))
        (let*-values (((simp expls) (partition (lambda(x) (and (not (memq (car x) als)) (is-simple x xls))) expls))
                      ((lamb complex) (partition (lambda(x) (and (not (memq (car x) als)) (is-lambda x))) expls)))
          (if (null? complex)
              `(let ,simp (assigned () (letrec ,lamb
                                         ,body)))
              (let* ((comp-init (map (lambda(x) `(,(car x) (void))) complex))
                     (comp-ass (intersection (map car complex) als))
                     (als (difference als comp-ass))
                     (comp-temp (map (lambda(x) `(,(unique-name 't) ,(cadr x))) complex))
                     (comp-set-ls (map (lambda (x y) `(set! ,x ,y)) (map car complex) (map car comp-temp))))
                `(let ,simp
                   (assigned ()
                             (let ,comp-init
                               (assigned ,comp-ass
                                         (letrec ,lamb
                                           (let ,comp-temp
                                             (assigned ,als
                                                       ,(make-begin (append comp-set-ls `(,body) ))))))))))))))
    
    

    (define (Expr-ls exp ls)
      (let* ((Expr (lambda(x) (Expr-ls x ls)))
             (notin? (lambda(x) (not (memq x ls)))))
        (match exp
          ((if ,x ,y ,z) (andmap Expr `(,x ,y ,z)))
          ((begin ,x ...) (andmap Expr x))
          ((let ((,x ,y) ...) (assigned ,ls ,z)) (andmap Expr (cons z y)))
          ((letrec ((,x ,y) ...) (assigned ,ls ,[Expr -> z])) (and z (andmap Expr y)))
          ((lambda (,x ...) (assigned ,ls ,[Expr -> z])) z)
          ((quote ,x) #t)
          ((,x ,y ...) (guard (prim? x)) (andmap Expr y))
          ((,x ...) (andmap Expr x))
          (,x (guard (uvar? x)) (notin? x))
          (,else #t))))
    
    (define (Expr exp)
      (match exp
        ((if ,[Expr -> x] ,[Expr -> y] ,[Expr -> z]) `(if ,x ,y ,z))
        ((begin ,[Expr -> x] ...) `(begin ,x ...))
        ((let ((,x ,[Expr -> y]) ...) (assigned ,ls ,[Expr -> z])) (let ((exls (map (lambda(x y) `(,x ,y)) x y)))
                                                                     `(let ,exls (assigned ,ls ,z))))
        ((letrec ((,x ,[Expr -> z]) ...) (assigned ,ls ,[Expr -> y])) (Exp x z y ls))
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
