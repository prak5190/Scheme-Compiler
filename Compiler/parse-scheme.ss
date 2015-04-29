(library (Compiler parse-scheme)
  (export
   parse-scheme
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (parse-scheme program)    
    (define (check-unique x* exp)
      (let f ((x* x*))
        (if (null? x*)
            #t
            (let ((x (car x*))
                  (x* (cdr x*)))
              (if (memq x x*)
                  (error who "Duplicate var ~s declared  in Expr ~s" x exp)
                  (f x*))))))
      
    (define (Datum exp)
      (match exp
        (,x (guard (integer? x)) (if (fixnum? x)
                                     `,x
                                     (error who "~s is not in fixnum range" x)))
        (#t '#t)
        (#f '#f)
        (() (quote ()))
        (,x (guard (symbol? x)) x)
        ((,[Datum -> x] . ,[Datum -> y]) `(,x . ,y))
        (else (error who "Invalid exp 11111" exp))))
              
              
    (define (Env-Prim exp env ls)
      (let ((Expr (lambda(xs) (Expr xs env ls))))
        (match exp
          ((,x ,[Expr -> y] ...) (guard (prim? x)) (if (eqv? (length y) (get-prim-arg-length x))
                                                       `(,x ,y ...)
                                                       (error who "Incorrect number of arguments for ~s in Expr ~s" x exp)))
          (,else (error who "Invalid exp " exp)))))
    
    ;; The mother func for main language primitives
    (define (Init-Env exp env ls)
      (let ((Expr (lambda(x) (Expr x env ls)))
            (ExprLs (lambda(x l) (Expr x env (append l ls)))))
        (match exp
          ((quote ,[Datum -> x])  `(quote ,x))          
          ((quote ,x ...) (error who "Invalid quote ~s" exp))
          ((if ,[Expr -> x] ,[Expr -> y]) `(if ,x ,y (void)))
          ((if ,[Expr -> x] ,[Expr -> y] ,[Expr -> z]) `(if ,x ,y ,z))
          ((if ,x ...) (error who "Incorrect Number of args to if in expression ~s" exp))
          ((let ((,x ,[Expr -> y]) ...) ,z ...) (begin
                                                  (check-unique x exp)
                                                  (let* ((xls (map (lambda(x) `(,x ,(unique-name x))) x))
                                                         (letls (map (lambda(x y) `(,(cadr x) ,y)) xls y))
                                                         (z (make-begin (map (lambda(z) (ExprLs z xls)) z))))
                                                    `(let ,letls ,z))))
          ((let ,x ...) (error who "Malformed let expression ~s" exp))
          ((letrec ((,x ,y) ...) ,z ...) (begin
                                           (check-unique x exp)
                                           (let* ((xls (map (lambda(x) `(,x ,(unique-name x))) x))
                                                  (y (map (lambda(y) (ExprLs y xls)) y))
                                                  (letls (map (lambda(x y) `(,(cadr x) ,y)) xls y))
                                                  (z (make-begin (map (lambda(z) (ExprLs z xls)) z))))
                                             `(let ,letls ,z))))
          ((letrec ,x ...) (error who "Malformed letrec expression ~s" exp))
          ((lambda (,x ...) ,z ...) (begin
                                      (check-unique x exp)
                                      (let* ((xls (map (lambda(x) `(,x ,(unique-name x))) x))                                           
                                             (z (make-begin (map (lambda(z) (ExprLs z xls)) z))))
                                        `(lambda ,x ,z))))
          ((lambda ,x ...) (error who "Invalid lambda expression ~s" exp))
          ((begin ,[Expr -> x] ... ,[Expr -> y]) `(begin ,x ... ,y))
          ((begin ,x ...) (error who "Malformed begin expression ~s" exp))
          ((set! ,x ,y) (guard (symbol? x)) `(set! ,(Expr x) ,(Expr y)))
          ((set! ,x ...) (error who "Invalid set! expression ~s" exp))
          ((not ,[Expr -> x]) `(if ,x '#f '#t))
          ((and ,[Expr -> x*] ...) (if (null? x*)
                                       '(quote #t)
                                       (let f ([x* x*])
                                         (let ([x (car x*)] [x* (cdr x*)])
                                           (if (null? x*)
                                               x
                                               `(if ,x ,(f x*) (quote #f)))))))
          ((or ,[Expr -> x*] ...) (if (null? x*)
                                      '(quote #f)
                                      (let f ([x* x*])
                                        (let ([x (car x*)] [x* (cdr x*)])
                                          (if (null? x*)
                                               x
                                               (let ((tmp (unique-name 't)))
                                                 `(let ((,tmp ,x))
                                                    (if ,tmp ,tmp ,(f x*)))))))))
          (,else (error who "Invalid expression ~s" exp)))))
    
          
    (define (Expr exp env ls)
      (match exp
        (,x (guard (integer? x)) (if (fixnum? x)
                                     `',x
                                     (error who "~s is not in fixnum range" x)))
        (#t `'#t)
        (#f `'#f)
        (() (quote ()))
        (,x (guard (symbol? x)) (cond
                                 ((assq x ls) => (lambda(r) (cadr r)))
                                 (else (error who "Unbound variable ~s" x))))
        ((,x ,y ...) (cond
                      ;; If present in list then just use that
                      ((assq x ls) (map (lambda(x) (Expr x env ls)) (cons x y)))
                      ;; Environment meant for primitives and macros which can actually transform code
                      ((assq x env) => (lambda(r) ((cadr r) exp env ls)))
                      (else (map (lambda(x) (Expr x env ls)) (cons x y)))))))
    
    
    (define (Program exp)                   ;get-trace-define
      (let* ((main-prims '(let if begin letrec and or quote set!))             
             (env (append (map (lambda(x) `(,x ,Init-Env)) main-prims)
                          (map (lambda(x) `(,x ,Env-Prim)) (append (map car pred-prim)
                                                                   (append (map car value-prim)
                                                                           (map car effect-prim)))))))
        (Expr exp env '())))

    (trace-define (parse-scheme exp)                   ;get-trace-define      
      (Program exp))
    
    (parse-scheme program)))
