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
    
    ;; Returns set! exp list , list to be added and the var to be used
    (define (Value exp ls)
      (match exp
        ((if ,x ,y ,z) (let ((n (get-unique-name ls)))
                         (let*-values (((x l) (Pred x ls))
                                       ((e1 l1 y) (Value y (append ls l)))
                                       ((e2 l2 z) (Value z (append ls (append l1 l)))))                                                      
                           (values `((set! ,n (if ,x ,(make-begin (append e1 `(,y))) ,(make-begin (append e2 `(,z)))))) `(,n ,l ... ,l1 ... ,l2 ...) n))))
        ((begin ,x ... ,z) (let ((n (get-unique-name ls)))
                         (values `((set! ,n (begin ,x ... ,z))) `(,n) n)))
        ((,x ,y ,z) (guard (binop? x)) (let ((n (get-unique-name ls)))
                                         (let*-values (((e1 l1 y) (Value y ls))
                                                       ((e2 l2 z) (Value z (append ls l1))))
                                           (values `(,e1 ... ,e2 ... (set! ,n (,x ,y ,z))) `(,n ,l1 ... ,l2 ...) n))))
        ((,x ,y ...)  (let ((n (get-unique-name ls)))
                        (let*-values (((pre ls valls) (Value* y ls)))                             
                          (values `(,pre ... (set! ,n (,x ,valls ...))) (cons n ls) n))))
        (,x (guard triv? x) (values '() '() x))))

    (define (Value* exp ls)
      (cond
       ((or (not (pair? exp)) (null? exp)) (values '() '() '()))
       (else (let*-values (((exls l x) (Value (car exp) ls))
                           ((expls l2 xls) (Value* (cdr exp) (append l ls))))
               (values (append exls expls) (append l l2) (cons x xls))))))

    (define (Effect exp ls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((e l) (Pred x ls))
                                     ((e1 l1) (Effect y (append l ls)))
                                     ((e2 l2) (Effect z (append ls (append l1 l)))))
                         (values `((if ,e ,(make-begin e1) ,(make-begin e2))) `(,l1 ... ,l2 ...))))
        ((begin ,x ... ,y) (let*-values (((x l) (Effect* x ls))
                                        ((y l1) (Effect y (append l ls))))                             
                             (values `((begin ,x ... ,y)) (append l1 l))))
        ((,x ...)  (let-values (((pre l ex) (Value exp ls)))
                     (values (append pre `(,ex)) l)))
        ((set! ,x ,y) (let*-values (((e l y) (Value y ls)))
                        (values `(,e ... (set! ,x ,y)) l)))
        (,x (values `(,x) '()))))
    
    ;; Returns modified list and list to be added 
    (define (Effect* exp ls)
      (cond
       ((null? exp) (values '() '()))
       (else (let*-values (((x l) (Effect (car exp) ls))
                           ((y l1) (Effect* (cdr exp) (append ls l))))
               (values (append x y) (append l l1))))))
        
        
    ;; Returns exp to be prepended ,list to be added to locals and var to be used
    (define (Pred exp ls)
      (match exp
        ((,x ,y ,z) (guard (relop? x))  (let*-values (((e1 l1 y) (Value y ls))
                                                      ((e2 l2 z) (Value z (append ls l1))))
                                          (values (make-begin `(,e1 ... ,e2 ...  (,x ,y ,z))) `(,l1 ... ,l2 ...))))
        ((if ,x ,y ,z) (let*-values (((e l) (Pred x ls))
                                     ((e1 l1) (Pred y (append l ls)))
                                     ((e2 l2) (Pred z (append ls (append l1 l)))))
                                          (values (make-begin `((if ,e ,e1 ,e2))) `(,l1 ... ,l2 ...))))
        ((begin ,x ... ,y) (let*-values (((x l) (Effect* x ls))
                                        ((y l1) (Pred y (append l ls))))
                             (values `(begin ,x ... ,y) (append l1 l))))
        ((,x ...)  (let-values (((pre l ex) (Value exp ls)))
                     (values (make-begin (append pre `(,ex))) l)))
        (,x (values x '())))) 
        
        
    ;; Returns expression and list of vars to be added 
    (define (Tail exp ls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((x l) (Pred x ls))
                                     ((e1 l1) (Tail y (append ls l)))
                                     ((e2 l2) (Tail z (append ls (append l l1)))))
                         (values `(if ,x ,e1 ,e2) `(,l ... ,l1 ... ,l2 ...))))        
        ((,x ,y ,z) (guard (binop? x)) (let*-values (((e1 l1 y) (Value y ls))
                                                     ((e2 l2 z) (Value z (append ls l1))))
                                         (values (make-begin (append (append e1 e2) `((,x ,y ,z)))) (append l1 l2))))                    
        ((begin ,x ... ,y) (let*-values (((x l) (Effect* x ls))
                                        ((y l1) (Tail y (append l ls))))
                             (values `(begin ,x ... ,y) (append l1 l))))
        ((,x ...)  (let-values (((pre l ex) (Value exp ls)))
                     (values (make-begin (append pre `(,ex))) l)))
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
