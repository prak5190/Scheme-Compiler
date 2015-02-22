(library (Compiler remove-complex-opera*)
  (export
   remove-complex-opera*
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (remove-complex-opera* program)
    ;; (get-unique-name ls)

    
    (define (Value exp ls)
      (match exp
        ((if ,x ,y ,z) (let ((n (get-unique-name ls)))                         
                         (values `(set! ,n (if ,x ,(Tail y ls) ,(Tail z ls))) `(,n))))
        ((begin ,x ... ,z) (let ((n (get-unique-name ls)))                         
                         (values `(set! ,n (begin ,x ... ,z)) `(,n))))
        ((,x ,y ,z) (guard (binop? x)) (let ((n (get-unique-name ls)))
                                         (values `(set! ,n (,x ,y ,z)) `(,n))))
        (,x (guard triv? x) (values exp '()))))

    (define (Value* exp ls)
      (cond
       ((null? exp) (values '() '()))
       (else (let*-values (((exls l) (Value (car exp) ls))
                           ((expls l2) (Value* (cdr exp) (append l ls))))
               (values (cons exls expls) (append l l2))))))                                           

    ;; Returns expression and list of vars to be added 
    (define (Tail exp ls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((e1 l1) (Tail y ls))
                                    ((e2 l2) (Tail z (append ls l1))))
                         (values `(if ,x ,e1 ,e2) (append l2 l1))))        
        ((,x ,y ,z) (guard (binop? x)) (values exp '()))
        ((begin ,x ... ,y) (let-values (((y l1) (Tail y ls)))
                             (values `(begin ,x ... ,y) l1)))
        ((,x ...) (let-values (((exp l1) (Value* x ls)))
                    (values (append exp l1) l1)))                   
        (,x (guard triv? x) (values exp '()))))
    
    (define (Body exp)
      (match exp
        ((locals (,x ...) ,y)  (let-values (((y ls) (Tail y x)))
                                 `(locals ,(append x ls) ,y)))))

    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,tail)) `(,x (lambda (,y ...) ,(Body tail))))))

    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))

    (define (remove-complex-opera* exp)                   ;get-trace-define
      (Program exp))
    
    (remove-complex-opera* program)))
