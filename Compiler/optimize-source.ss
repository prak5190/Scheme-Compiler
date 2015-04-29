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
       ((null? expls) (values expls ls))
       (else (let*-values (((x ls) (Exp (car expls) ls))
                           ((y ls) (Exp* (cdr expls) (cons x ls))))
               (values `(nop) ls)))))
    
    (define (Exp exp ls)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,z)) (let-values (((z ls) (Expr z ls)))
                                     (values `(,x (lambda (,y ...) ,z)) ls)))))
    (define (Expr* expls ls)
      (cond
       ((null? expls) (values '() ls))
       (else (let*-values (((x ls) (Expr (car expls) ls))
                           ((y ls) (Expr* (cdr expls) ls)))
               (values (cons x y) ls)))))

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
        ((if ,x ,y ,z) (let*-values (((x ls) (Expr x ls))
                                     ((y ls) (Expr y ls))
                                     ((z ls) (Expr z ls)))
                         (if (is-constant-or-bool? x)
                             (if (get-unquoted x) (values y ls) (values z ls))
                             (values `(if ,x ,y ,z) ls))))        
        ((begin ,x ...) (let*-values (((x ls) (Expr* x ls)))
                          (values `(begin ,x ... ) ls)))
        ((let ((,x ,y) ...) ,z) (let*-values (((y ls) (Expr* y ls))
                                              ((letls l1) (partition (lambda(x)
                                                                       (not (is-constant-or-bool? (cadr x))))
                                                                     (map (lambda(x y) `(,x ,y)) x y)))
                                              ((z ls) (Expr z (append l1 ls))))
                                  (let* ((letls (filter (lambda(x) (memq (car x) ls)) letls)))
                                    (if (null? letls)
                                        (values z (append l1 ls))                                      
                                        (values `(let ,letls ,z) (append l1 ls))))))
        ((letrec (,x ...) ,y) (let*-values (((x ls) (Exp* x ls))
                                            ((y ls) (Expr y ls)))
                                (values y ls)))
        ((quote ,x) (values exp ls))
        ((,x ,y ...) (guard (prim? x)) (let-values (((y ls) (Expr* y ls)))
                                         (if (and (is-foldable-prim? x)
                                                  (andmap is-constant? y))
                                             (values (apply-op `(,x ,y ...)) ls)
                                             (values `(,x ,y ...) ls))))
        ((,x ...) (let-values (((x ls) (Expr* x ls)))
                    (values `(,x ...) ls)))
        (,x (guard (uvar? x)) (cond
                               ((assq x ls) => (lambda(r) (values (cadr r) (cons x ls))))
                               (else (values x (cons x ls)))))
        (,else (values else ls))))
           
    (define (Program exp)                   ;get-trace-define
      (let-values (((exp ls) (Expr exp '())))
        exp))

    (define (optimize-source exp)                   ;get-trace-define
      (Program exp))
    
    (optimize-source program)))
