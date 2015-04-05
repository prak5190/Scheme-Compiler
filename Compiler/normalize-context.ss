(library (Compiler normalize-context)
  (export
   normalize-context
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (normalize-context program)
    (define (constant x)
      (match x
        ((quote ,x) x)
        (,x #f)))

    (define (normalize-if-cond x)
      (if (constant x)
          (if (eq? (constant x) '#f) `(false) `(true))
          `(if (eq? ,x '#f) (false) (true))))
    
    (define (Value* expls)
      (cond
       ((null? expls) expls)
       (else (let ((x (Value (car expls))))
               (append `(,x) (Value* (cdr expls)))))))
    
    (define (Value exp)
      (match exp
        ((if ,x ,y ,z) (let* ((x (Pred x))
                              (y (Value y))
                              (z (Value z)))
                         `(if ,x ,y ,z)))
        ((begin ,x ... ,y) (let ((x (Effect* x)))
                             `(begin ,x ... ,(Value y))))
        ((let ((,x ,y) ...) ,z) (let* ((ls (map (lambda(x y) `(,x ,y)) x (Value* y))))
                                  `(let ,ls ,(Value z))))
        ((quote ,x) exp)
        ((,x ,[Value -> y] ...) (guard (or (value-prim? x) (label? x) (uvar? x))) `(,x ,y ...))
        ((,x ,[Value -> y] ...) (guard (pred-prim? x)) `(if (,x ,y ...) '#t '#f))                
        ((,x ,[Value -> y] ...) (guard (effect-prim? x)) `(begin (,x ,y ...) (nop)))
        ((,x ...) (Value* x))
        (,x (guard (or (uvar? x) (label? x))) x)
        (,else `(nop))))

    (define (Pred exp)
      (match exp
        ((if ,x ,y ,z) (let* ((x (Pred x))
                              (y (Pred y))
                              (z (Pred z)))                         
                         `(if ,x ,y ,z)))
        ((begin ,x ... ,y) (let ((x (Effect* x)))
                             `(begin ,x ... ,(Pred y))))
        ((let ((,x ,y) ...) ,z) (let* ((ls (map (lambda(x y) `(,x ,y)) x (Value* y))))
                                  `(let ,ls ,(Pred z))))
        ((quote ,x) (if (eq? x '#f) `(false) `(true)))
        ((,x ,[Value -> y] ...) (guard (pred-prim? x)) `(,x ,y ...))
        ((,x ,[Value -> y] ...) (guard (or (value-prim? x) (label? x) (uvar? x))) `(if (eq? (,x ,y ...) '#f) (false) (true)))
        ((,x ,[Value -> y] ...) (guard (effect-prim? x)) `(begin (,x ,y ...) (true)))
        ((,[Value -> x] ...) `(if (eq? (,x ...) '#f) (false) (true)))
        (,x (guard (var? x)) x)
        (,else  `(nop))))
    
    (define (Effect* expls)
      (cond
       ((null? expls) expls)
       (else (let ((x (Effect (car expls))))
               (append `(,x) (Effect* (cdr expls)))))))    
    
    (define (Effect exp)
      (match exp
        ((if ,x ,y ,z) (let* ((x (Pred x))
                              (y (Effect y))
                              (z (Effect z)))
                         `(if ,x ,y ,z)))
        ((begin ,x ...) (let ((x (Effect* x)))
                          `(begin ,x ...)))
        ((let ((,x ,y) ...) ,z) (let* ((ls (map (lambda(x y) `(,x ,y)) x (Value* y))))
                                  `(let ,ls ,(Effect z))))
        ((quote ,x) `(nop))
        ((,x ,[Value -> y] ...) (guard (effect-prim? x)) `(,x ,y ...))
        ((,x ,[Value -> y] ...) (guard (or (uvar? x) (label? x))) `(,x ,y ...))
        ;; Ignore x in both case 
        ((,x ,[Effect -> y] ...) (guard (value-prim? x)) (make-nopless-begin y))
        ((,x ,[Effect -> y] ...) (guard (pred-prim? x)) (make-nopless-begin y))        
        ((,x ...) (Value* x))
        (,x (guard (var? x)) `(nop))
        (,else  `(nop))))

    ;; Everything starts off in value context
    (define (Expr exp)
      (Value exp))
    
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,z)) (let ((z (Expr z)))
                                     `(,x (lambda (,y ...) ,z))))))
    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,z) (let ((z (Expr z)))
                                         `(letrec (,x ...) ,z)))))

    (define (normalize-context exp)                   ;get-trace-define
      (Program exp))
    
    (begin
      (normalize-context program))))
