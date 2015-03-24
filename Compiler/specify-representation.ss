(library (Compiler specify-representation)
  (export
   specify-representation
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (specify-representation program)
    (define (Value* expls)
      (cond
       ((null? expls) expls)
       (else (let ((x (Value (car expls))))
               (append `(,x) (Value* (cdr expls)))))))
    
    (define (Value exp)
      (match exp
        ((alloc ,v) `(alloc ,(Value v)))
        ((if ,x ,y ,z) (let* ((x (Pred x))
                              (y (Value y))
                              (z (Value z)))
                         `(if ,x ,y ,z)))
        ((begin ,x ... ,y) (let ((x (Effect* x)))
                             `(begin ,x ... ,(Value y))))
        ((let (,x ...) ,y) (let* ((x (Value* x)))
                             `(let (,x ...) ,(Value y))))
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
                           `(begin ,x ... ,(Pred y))))
        ((let (,x ...) ,y) (let* ((x (Value* x)))
                             `(let (,x ...) ,(Pred y))))
        ((,x ,y ,z) (guard (relop? x)) (let* ((y (Value y))
                                              (z (Value z)))
                                         `(,x ,y ,z)))
        (,else  else)))

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
        ((begin ,x ... ,y) (let ((x (Effect* x)))
                             `(begin ,x ... ,(Effect y))))
        ((mset! ,x ,y ,z) (let* ((x (Value x))
                                 (y (Value y))
                                 (z (Value z)))
                            `(mset! ,x ,y ,z)))
        ((let (,x ...) ,y) (let* ((x (Value* x)))
                             `(let (,x ...) ,(Effect y))))
        ((,x ...) (Value* x))
        (,else  else)))
    
    (define (Tail exp)
      (match exp
        ((alloc ,v) `(alloc ,(Value v)))
        ((if ,x ,y ,z) (let* ((x (Pred x))
                              (y (Tail y))
                              (z (Tail z)))
                         `(if ,x ,y ,z)))
        ((begin ,x ... ,y) (let ((x (Effect* x)))
                             `(begin ,x ... ,(Tail y))))
        ((let (,x ...) ,y) (let* ((x (Value* x)))
                             `(let (,x ...) ,(Tail y))))
        ((,x ,y ,z) (guard (binop? x)) (let* ((y (Value y))
                                              (z (Value z)))
                                         `(,x ,y ,z)))        
        ((,x ...) (Value* x))
        (,x (guard (triv? x)) (Value x))))
    
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,z)) (let ((z (Tail z)))
                                      `(,x (lambda (,y ...) (locals ,z)))))))
    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,z) (let ((z (Tail z)))
                                         `(letrec (,x ...) ,z)))))

    (define (specify-representation exp)                   ;get-trace-define
      (Program exp))    
    (specify-representation program)))
