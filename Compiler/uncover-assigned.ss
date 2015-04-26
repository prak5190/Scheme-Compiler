(library (Compiler uncover-assigned)
  (export
   uncover-assigned
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (uncover-assigned program)        
    (define (Exp* expls yls)
      (cond
       ((null? expls) (values expls '()))
       (else (let*-values (((x l1) (Exp (car expls) yls))
                           ((y l2) (Exp* (cdr expls) yls)))
               (values (cons x y) (union l1 l2))))))

    (define (Exp exp y)                   ;get-trace-define
      (match exp
        ((,x ,z) (let-values (((z ls) (Expr z)))
                   (let* ((as-ls (intersection y ls))
                          (rem-ls (difference ls as-ls)))
                     (values `(,x ,z) ls))))))
    

    
    (define (Expr* expls)
      (cond
       ((null? expls) (values expls '()))
       (else (let*-values (((x l1) (Expr (car expls)))
                           ((y l2) (Expr* (cdr expls))))
               (values (cons x y) (union l1 l2))))))
    
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
                                  (let* ((un-ls (union l1 l2))
                                         (as-ls (intersection x (union l1 l2)))
                                         (rem-ls (difference un-ls as-ls)))
                                    (values `(let ,(map (lambda(x y) `(,x ,y)) x y) (assigned ,as-ls ,z)) rem-ls))))
        ((letrec (,x ...) ,y) (let*-values (((x l1) (Exp* x (map car x)))
                                            ((y l2) (Expr y)))
                                (let* ((un-ls (union l1 l2))
                                       (as-ls (intersection (map car x) (union l1 l2)))
                                       (rem-ls (difference un-ls as-ls)))
                                  (values `(letrec (,x ...) (assigned ,as-ls ,y)) rem-ls))))
        ((lambda (,x ...) ,z) (let-values (((z ls) (Expr z)))
                                (let* ((as-ls (intersection x ls))
                                       (rem-ls (difference ls as-ls)))
                                  (values `(lambda (,x ...) (assigned ,as-ls ,z)) rem-ls))))
        ((quote ,x) (values exp '()))
        ((set! ,x ,y) (let-values (((y ls) (Expr y)))
                        (values `(set! ,x ,y) (union `(,x) ls))))
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

    (define (uncover-assigned exp)                   ;get-trace-define      
      (Program exp))
    
    (uncover-assigned program)))
