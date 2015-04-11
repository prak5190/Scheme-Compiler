(library (Compiler convert-closures)
  (export
   convert-closures
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (convert-closures program)
    (define (Exp* expls)
      (cond
       ((null? expls) (values '() '()))
       (else (let*-values (((x l1) (Exp (car expls)))
                           ((y l2) (Exp* (cdr expls))))
               (values (cons x y) (cons l1 l2))))))
    
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) (free ,fr ,[Expr -> z])))
         (let ((cp-uniq (unique-name 'cp)))           
               (values `(,x (lambda (,cp-uniq ,y ...) (bind-free ,(cons cp-uniq fr) ,z)))
                       (append `(,x ,(unique-label x)) fr))))))
    
    
    
    (define (Expr exp)
      (match exp
        ((if ,x ,y ,z) (let* ((x (Expr x))
                              (y (Expr y))
                              (z (Expr z)))
                         `(if ,x ,y ,z)))
        ((begin ,[Expr -> x] ...) `(begin ,x ... ))
        ((let ((,x ,[Expr -> y]) ...) ,[Expr -> z]) `(let ,(map (lambda(x y) `(,x ,y)) x y) ,z))
        ((letrec (,x ...) ,[Expr -> y]) (let*-values (((x ls) (Exp* x)))
                                          `(letrec (,x ...) (closure ,ls ,y))))
        ((quote ,x)  exp)
        ((,x ,[Expr -> y] ...) (guard (prim? x)) `(,x ,y ...))
        ((,x ,[Expr -> y] ...) (guard (uvar? x))  `(,x ,x ,y ...))
        ((,x ,[Expr -> y] ...)  (let ((cp.uniq (unique-name 'cp)))
                                  `(let ((,cp.uniq ,x))
                                     (,cp.uniq ,cp.uniq ,y ...))))
        (,x (guard (uvar? x)) x)
        (,else else)))
           
    (define (Program exp)                   ;get-trace-define
      (Expr exp))

    (define (convert-closures exp)                   ;get-trace-define
      ;; Need to determine what the count should be
      (unique-name-count 900)
      (Program exp))
    
    (convert-closures program)))
