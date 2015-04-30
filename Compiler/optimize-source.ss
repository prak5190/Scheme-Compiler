(library (Compiler optimize-source)
  (export
   optimize-source
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (optimize-source program)        
    (define (Exp* expls ls)
      (cond
       ((null? expls) (values expls '()))
       (else (let*-values (((x l1) (Exp (car expls) ls))
                           ((y l2) (Exp* (cdr expls) ls)))
               (values `(nop) '())))))
    
    (define (Exp exp ls)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,z)) (let-values (((z l1) (Expr z ls)))
                                     (values `(,x (lambda (,y ...) ,z)) l1)))))
    (define (Expr* expls ls)
      (cond
       ((null? expls) (values '() '()))
       (else (let*-values (((x l1) (Expr (car expls) ls))
                           ((y l2) (Expr* (cdr expls) ls)))
               (values (cons x y) (append l1 l2))))))

    (define (is-constant? x)
      (match x
        ((quote ()) #t)
        ((quote ,x)  (guard (number? x)) #t)
        (,else #f)))
    (define (is-constant-or-bool? x)
      (match x
        ((quote ()) #t)
        ;; Even number can work since normalize-context would transform it to a true exp anyway
        ((quote ,x)  (guard (or (boolean? x) (number? x))) #t)
        (,else #f)))
    (define (get-unquoted exp)
      (match exp
        ((quote ()) ''())
        ((quote ,x) x)
        (,e e)))
      
    (define (is-foldable-prim? x)
      (or (memq x '(+ - *))
          (assq x pred-prim)))

    (define (apply-op exp)
      (match exp
        ((,o ,[get-unquoted -> x] ...) (let ((val (eval `(,o ,x ...))))
                                      (if (or (fixnum? val) (boolean? val))
                                          `',val
                                          exp)))))
    
    (define (Expr exp ls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((x ls1) (Expr x ls))
                                     ((y ls2) (Expr y ls))
                                     ((z ls3) (Expr z ls)))
                         (let ((ls (append ls1 (append ls2 ls3))))
                           (if (is-constant-or-bool? x)
                               (if (get-unquoted x) (values y ls) (values z ls))
                               (values `(if ,x ,y ,z) ls)))))
        ((begin ,x ...) (let*-values (((x l1) (Expr* x ls)))
                          (values `(begin ,x ... ) l1)))
        ((let ((,x ,y) ...) ,z) (let*-values (((y l2) (Expr* y ls))
                                              ((letls l1) (partition (lambda(x)
                                                                       (not (is-constant-or-bool? (cadr x))))
                                                                     (map (lambda(x y) `(,x ,y)) x y)))
                                              ((z l3) (Expr z (append l1 ls))))
                                  (let* ((letls (filter (lambda(x) (memq (car x) ls)) letls)))
                                    (if (null? letls)
                                        (values z (append l2 (append l1 l3)))
                                        (values `(let ,letls ,z) (append l2 (append l1 l3)))))))
        ((letrec (,x ...) ,y) (let*-values (((x l1) (Exp* x ls))
                                            ((y l2) (Expr y ls)))
                                (values y (append l1 l2))))
        ((quote ,x) (values exp '()))
        ((,x ,y ...) (guard (prim? x)) (let-values (((y l1) (Expr* y ls)))
                                         (if (and (is-foldable-prim? x)
                                                  (andmap is-constant? y))
                                             (values (apply-op `(,x ,y ...)) l1)
                                             (values `(,x ,y ...) l1))))
        ((,x ...) (let-values (((x l1) (Expr* x ls)))
                    (values `(,x ...) l1)))
        (,x (guard (uvar? x)) (cond
                               ((assq x ls) => (lambda(r) (values (cadr r) `(,x))))
                               (else (values x `(,x)))))
        (,else (values else '()))))
           
    (define (Program exp)                   ;get-trace-define
      (let-values (((exp ls) (Expr exp '())))
        exp))

    (define (optimize-source exp)                   ;get-trace-define
      (Program exp))
    
    (optimize-source program)))
