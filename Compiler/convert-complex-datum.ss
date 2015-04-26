(library (Compiler convert-complex-datum)
  (export
   convert-complex-datum
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (convert-complex-datum program)        
    (define (Exp* expls)
      (cond
       ((null? expls) (values expls '()))
       (else (let*-values (((x l1) (Exp (car expls)))
                           ((y l2) (Exp* (cdr expls))))
               (values (cons x y) (append l1 l2))))))

    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x ,z) (let-values (((z ls) (Expr z)))
                   (values `(,x ,z) ls)))))

    (define (Vector y)
      (let* ((x (length y))
                          (n (unique-name 'tmp))                     
                          (ls (map (lambda(x y)
                                     `(vector-set! ,n ,(Quote x) ,(Quote y))) (iota x) y))
                          (val `(let ((,n (make-vector (quote ,x))))
                                  ,(cons 'begin (append ls `(,n))))))
                     val))
    (define (Quote exp)
      (match exp
        (#(,y ...) (Vector y))
        ((,[Quote -> x] . ,[Quote -> y]) `(cons ,x ,y))        
        (,x `(quote ,x))))
    
    (define (Expr* expls)
      (cond
       ((null? expls) (values expls '()))
       (else (let*-values (((x l1) (Expr (car expls)))
                           ((y l2) (Expr* (cdr expls))))
               (values (cons x y) (append l1 l2))))))

    (define (Expr exp)
      (match exp
        ((if ,x ,y ,z) (let*-values (((x l1) (Expr x))
                                     ((y l2) (Expr y))
                                     ((z l3) (Expr z)))                                                       
                         (values `(if ,x ,y ,z) (append l1 (append l2 l3)))))
        ((begin ,x ...) (let-values (((x ls) (Expr* x)))
                          (values `(begin ,x ...) ls)))
        ((let ((,x ,y) ...) ,z) (let*-values (((y l1) (Expr* y))
                                              ((z l2) (Expr z)))
                                  (values `(let ,(map (lambda(x y) `(,x ,y)) x y) ,z) (append l1 l2))))
        ((letrec (,x ...) ,y) (let*-values (((x l1) (Exp* x))
                                            ((y l2) (Expr y)))
                                (values `(letrec (,x ...) ,y) (append l1 l2))))
        ((lambda (,x ...) ,z) (let-values (((z ls) (Expr z)))
                                (values `(lambda (,x ...) ,z) ls)))
        ((quote #(,y ...)) (let* ((n1 (unique-name 'tmp))                                  
                                  (val (Vector y)))
                             (values n1 `((,n1 ,val)))))
        ((quote ,x) (guard (not (list? x))) (values exp '()))
        ((quote ,x) (let ((n (unique-name 'tmp)))                  
                      (values n `((,n ,(Quote x))))))        
        ((,x ,y ...) (guard (prim? x)) (let-values (((y ls) (Expr* y)))
                                                   (values `(,x ,y ...) ls)))
        ((,x ...) (let-values (((x ls) (Expr* x)))
                    (values `(,x ...) ls)))
        (,x (guard (uvar? x)) (values exp '()))
        (,else (values exp '()))))
    
    (define (Program exp)                   ;get-trace-define
      ;; (unique-name-count 800)      
      (let-values (((exp ls) (Expr exp)))
        (if (null? ls)
            exp
            `(let ,ls ,exp))))

    (define (convert-complex-datum exp)                   ;get-trace-define      
      (Program exp))
    
    (convert-complex-datum program)))
