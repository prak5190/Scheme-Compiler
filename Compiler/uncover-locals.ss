(library (Compiler uncover-locals)
  (export
   uncover-locals
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (uncover-locals program)
    ;; Doesn't modify expression , so all methods return list of locals    
    (define (Value* expls ls)
      (cond
       ((null? expls) ls)
       (else (let ((ls (Value (car expls) ls)))
               (Value* (cdr expls) ls)))))
    (define (Value exp ls)
      (match exp
        ((alloc ,v) (Value v ls))
        ((if ,x ,y ,z) (let* ((ls (Pred x ls))
                              (ls (Value y ls))
                              (ls (Value z ls)))
                         ls))
        ((begin ,x ... ,y) (let ((ls (Effect* x ls)))
                           (Value y ls)))
        ((let (,x ...) ,y) (let* ((uvars (map car x))
                                  (values (map cdr x))
                                  (ls (Value* values (append uvars ls))))
                             (Value y (map car x))))
        ((,x ,y ,z) (guard (binop? x)) (let* ((ls (Value y ls))
                                              (ls (Value z ls)))
                                         ls))
        ((,x ...) (Value* x ls))
        (,else  ls)))
    
    (define (Pred exp ls)
      (match exp
        ((if ,x ,y ,z) (let* ((ls (Pred x ls))
                              (ls (Pred y ls))
                              (ls (Pred z ls)))
                         ls))
        ((begin ,x ... ,y) (let ((ls (Effect* x ls)))
                           (Pred y ls)))
        ((let (,x ...) ,y) (let* ((uvars (map car x))
                                  (values (map cdr x))
                                  (ls (Value* values (append uvars ls))))
                             (Pred y (map car x))))
        ((,x ,y ,z) (guard (relop? x)) (let* ((ls (Value y ls))
                                              (ls (Value z ls)))
                                         ls))
        (,else  ls)))

    (define (Effect* expls ls)
      (cond
       ((null? expls) ls)
       (else (let ((ls (Effect (car expls) ls)))
               (Effect* (cdr expls) ls)))))
    
    (define (Effect exp ls)
      (match exp
        ((if ,x ,y ,z) (let* ((ls (Pred x ls))
                              (ls (Effect y ls))
                              (ls (Effect z ls)))
                         ls))
        ((begin ,x ... ,y) (let ((ls (Effect* x ls)))
                           (Effect y ls)))
        ((mset! ,x ,y ,z) (let* ((ls (Value x ls))
                                 (ls (Value y ls))
                                 (ls (Value z ls)))
                            ls))
        ((let (,x ...) ,y) (let* ((uvars (map car x))
                                  (values (map cdr x))
                                  (ls (Value* values (append uvars ls))))
                             (Effect y (map car x))))
        ((,x ...) (Value* x ls))
        (,else  ls)))
    
    (define (Tail exp ls)
      (match exp
        ((alloc ,v) (Value v ls))
        ((if ,x ,y ,z) (let* ((ls (Pred x ls))
                              (ls (Tail y ls))
                              (ls (Tail z ls)))
                         ls))
        ((begin ,x ... ,y) (let ((ls (Effect* x ls)))
                           (Tail y ls)))
        ((let (,x ...) ,y) (let* ((uvars (map car x))
                                  (values (map cdr x))
                                  (ls (Value* values (append uvars ls))))
                             (Tail y (map car x))))
        ((,x ,y ,z) (guard (binop? x)) (let* ((ls (Value y ls))
                                              (ls (Value z ls)))
                                         ls))        
        ((,x ...) (Value* x ls))
        (,x (guard (triv? x)) ls)))

    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,z)) (let ((ls (Tail z '())))
                                      `(,x (lambda (,y ...) (locals ,ls ,z)))))))
    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,z) (let ((ls (Tail z '())))
                                         `(letrec (,x ...) (locals ,ls ,z))))))

    (define (uncover-locals exp)                   ;get-trace-define
      (Program exp))    
    (uncover-locals program)))
