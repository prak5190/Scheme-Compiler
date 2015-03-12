(library (Compiler remove-let)
  (export
   remove-let
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (remove-let program)    
    (define (Value* expls)
      (map Value expls))
    
    (define (Value exp)
      (match exp
        ((alloc ,v) `(alloc ,(Value v)))
        ((if ,x ,y ,z) (let* ((x (Pred x))
                              (y (Value y))
                              (z (Value z)))
                         `(if ,x ,y ,z)))
        ((begin ,x ... ,y) (let ((x (Effect* x)))
                             (make-begin `(,x ... ,(Value y)))))
        ((let (,x ...) ,y) (let* ((uvars (map car x))
                                  (values (map cadr x))
                                  (ls (map (lambda(x y) `(set! ,x ,(Value y))) uvars values)))
                             (make-begin `(,ls ... ,(Value y)))))
        ((,x ,y ,z) (guard (binop? x)) (let* ((y (Value y))
                                              (z (Value z)))
                                         `(,x ,y ,z)))
        ((,x ...) (Value* x))
        (,else else)))
    
    (define (Pred exp)
      (match exp
        ((if ,x ,y ,z) (let* ((x (Pred x))
                              (y (Pred y))
                              (z (Pred z)))
                         `(if ,x ,y ,z)))
        ((begin ,x ... ,y) (let ((x (Effect* x)))
                             (make-begin `(,x ... ,(Pred y)))))
        ((let (,x ...) ,y) (let* ((uvars (map car x))
                                  (values (map cadr x))
                                  (ls (map (lambda(x y) `(set! ,x ,(Value y))) uvars values)))
                             (make-begin `(,ls ... ,(Pred y)))))
        ((,x ,y ,z) (guard (relop? x)) (let* ((y (Value y))
                                              (z (Value z)))
                                         `(,x ,y ,z)))
        (,else  else)))


    (define (Effect* expls)
      (map Effect expls))
    
    (define (Effect exp)
      (match exp
        ((if ,x ,y ,z) (let* ((x (Pred x))
                              (y (Effect y))
                              (z (Effect z)))
                         `(if ,x ,y ,z)))
        ((begin ,x ... ,y) (let ((x (Effect* x)))
                             (make-begin `(,x ... ,(Tail y)))))
        ((let (,x ...) ,y) (let* ((uvars (map car x))
                                  (values (map cadr x))
                                  (ls (map (lambda(x y) `(set! ,x ,(Value y))) uvars values)))
                             (make-begin `(,ls ... ,(Effect y)))))
        ((mset! ,x ,y ,z) (let* ((x (Value x))
                                 (y (Value y))
                                 (z (Value z)))
                            `(mset! ,x ,y ,z)))
        ((,x ...) (map Value x))
        (,else  exp)))
    
    (define (Tail exp)
      (match exp
        ((alloc ,v) `(alloc ,(Value v)))
        ((if ,x ,y ,z) (let* ((x (Pred x))
                              (y (Tail y))
                              (z (Tail z)))
                         `(if ,x ,y ,z)))
        ((begin ,x ... ,y) (let ((x (Effect* x)))
                             (make-begin `(,x ... ,(Tail y)))))
        ((let (,x ...) ,y) (let* ((uvars (map car x))
                                  (values (map cadr x))
                                  (ls (map (lambda(x y) `(set! ,x ,(Value y))) uvars values)))
                             (make-begin `(,ls ... ,(Tail y)))))
        ((,x ,y ,z) (guard (binop? x)) (let* ((y (Value y))
                                              (z (Value z)))
                                         `(,x ,y ,z)))
        ((,x ...) (map Value x))
        (,x (guard (triv? x)) x)))

    (define (Body exp)
      (match exp
        ((locals ,ls ,y) `(locals ,ls ,(Tail y)))))
    
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,b)) `(,x (lambda (,y ...) ,(Body b))))))
    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))

    (define (remove-let exp)                   ;get-trace-define
      (Program exp))    
    (remove-let program)))
