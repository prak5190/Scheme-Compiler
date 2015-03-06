(library (Compiler impose-calling-conventions)
  (export
   impose-calling-conventions
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (impose-calling-conventions program)
    ;; (get-unique-name ls)   
    (define (add-begin ex)
      (match ex
        ((,x ,y ,z) (guard (relop? x)) ex)
        ((,x) x)
        ((if ,x ,y ,z) `(if ,x ,y ,z))
        ((begin ,x ...) `(begin ,x ...))
        ((,x ,y ...) `(begin ,x ,y ...))))

    ;; Assign proc vals , Returns list of set! exps and set of params not assigned
    (define (assign-val-reg params expls regls l1)     
      (cond
       ((or (null? regls) (null? params)) (values expls params l1))
       (else (let* ((r (car regls))
                    (exp `(set! ,r ,(car params))))
               (assign-val-reg (cdr params) (append expls `(,exp)) (cdr regls) (cons r l1))))))
    
    (define (assign-val-frames params expls ind l2)
      (cond
       ((null? params) (values expls params l2))
       (else (let* ((f (index->frame-var ind))
                    (exp `(set! ,f ,(car params))))
               (assign-val-frames (cdr params) (append expls `(,exp)) (add1 ind) (cons f l2))))))
    
    (define (assign-val-vars params ls expls fls)
      (cond
       ((null? params) (values expls params fls))
       (else (let* ((n (get-unique-name-p (append ls fls) 'nfv))
                    (exp `(set! ,n ,(car params))))
               (assign-val-vars (cdr params) ls (append expls `(,exp)) (cons n fls))))))
    
    ;; Returns list of set! exps , set of params not assigned
    (define (assign-params-reg params expls regls)   
      (cond
       ((or (null? regls) (null? params)) (values expls params))
       (else (let ((exp `(set! ,(car params) ,(car regls))))
               (assign-params-reg (cdr params) (append expls `(,exp)) (cdr regls))))))

    (define (assign-params-frames params expls ind)
      (cond
       ((null? params) (values expls params))
       (else (let ((exp `(set! ,(car params) ,(index->frame-var ind))))
               (assign-params-frames (cdr params) (append expls `(,exp)) (add1 ind))))))

    ;; Returns a list of exps
    (define (substitute-proc-vars params rp)
      (match params
        ((alloc ,x)  `((set! ,return-value-register ,params)
                         (,rp ,frame-pointer-register ,return-value-register)))
        ((mref ,x ,y)  `((set! ,return-value-register ,params)
                         (,rp ,frame-pointer-register ,return-value-register)))
        ((,x ,y ,z) (guard (binop? x)) `((set! ,return-value-register ,params)
                                         (,rp ,frame-pointer-register ,return-value-register)))
        ((,x ,y ...) (guard (triv? x)) (let*-values
                                           (((exp1 params l1) (assign-val-reg y '() parameter-registers '()))
                                            ((exp2 params l2) (assign-val-frames params '() 0 '())))
                                         (let ((rpset `(set! ,return-address-register ,rp)))
                                           `(,exp2 ... ,exp1 ... ,rpset
                                                   (,x ,return-address-register ,frame-pointer-register ,(reverse l1) ... ,(reverse l2) ...)))))
        (,x (guard (triv? x)) `((set! ,return-value-register ,x)
                                (,rp ,frame-pointer-register ,return-value-register)))))
    
    ;; Return pre , value and fls 
    (define (Value exp ls fls)
      (match exp
        ((alloc ,x) (values exp fls))   
        ((mref ,x ,y) (values exp fls))
        ((,x ,y ...) (guard (triv? x))
         (let ((n (get-unique-label-p ls 'rp)))
           (let*-values
               (((exp1 params l1) (assign-val-reg y '() parameter-registers '()))
                ((exp2 params l2) (assign-val-vars params ls '() '())))
             (let ((rpset `(set! ,return-address-register ,n)))
               (values `(return-point ,n (begin
                                           ,exp2 ... ,exp1 ... ,rpset
                                           (,x ,return-address-register ,frame-pointer-register ,(reverse l1) ... ,(reverse l2) ...)))
                       `(,(reverse l2) ,fls ...))))))
        (,else (values exp fls))))
      
    (define (Pred exp ls fls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((x fls) (Pred x ls fls))
                                     ((y fls) (Pred y ls fls))
                                     ((z fals) (Pred z ls fls)))
                         (values `(if ,x ,y ,z) fls)))
        ((begin ,x ... ,y) (let*-values (((x fls) (Effect* x ls fls))
                                         ((y fls) (Pred y ls fls)))
                             (values `(begin ,x ... ,y) fls)))
        (,else (values exp fls))))

    (define (Effect* exp ls fls)
      (cond
       ((null? exp) (values '() fls))
       (else (let*-values (((exl fls) (Effect (car exp) ls fls))
                           ((ex2 fls) (Effect* (cdr exp) ls fls)))
               (values (cons exl ex2) fls)))))
    
    (define (Effect exp ls fls)
      (match exp       
        ((if ,x ,y ,z) (let*-values (((x fls) (Pred x ls fls))
                                     ((y ls) (Effect y ls fls))
                                     ((z ls) (Effect z ls fls)))
                         (values `(if ,x ,y ,z) fls)))
        ((begin ,x ...) (let*-values (((x ls) (Effect* x ls fls)))
                          (values `(begin ,x ...) fls)))
        ((set! ,u (,x ,y ...)) (guard (not (binop? x))) (let*-values (((exp fls) (Value `(,x ,y ...) ls fls)))
                                                          (values (make-begin `(,exp (set! ,u ,return-value-register))) fls)))
        ((set! ,x ,y) (values exp fls))
        ((mset! ,x ,y) (values exp fls))        
        ;; Apparently this case is not poosible
        ;; ((,x ,y ,z) (guard (binop? x)) (let*-values (((pre1 e1 fls) (Value y ls fls))
        ;;                                              ((pre2 e2 fls) (Value z ls fls)))
        ;;                                  (values (append (append e1 e2) `((,x ,e1 ,e2))) fls)))
        ((,x ,y ...) (guard (triv? x)) (Value exp ls fls))                     

        (,else (values exp fls))))
    
    ;;parameter-registers, frame-pointer-register,
    ;;return-value-register, and return-address-register.
    ;; Returns -> exp , list of frame vars
    (define (Tail exp ls prep-exp rp fls)      
      (match exp
        ((if ,x ,y ,z) (let*-values (((x fls) (Pred x ls fls))
                                     ((y fls) (Tail y ls '() rp fls))
                                     ((z fls) (Tail z ls '() rp fls)))
                         (values (make-begin `(,prep-exp ... (if ,x ,y ,z))) fls)))
        ;((mref ,x ,y) (values exp ls))
        ((begin ,x ... ,y) (let*-values (((x fls) (Effect* x ls fls))
                                         ((y fls) (Tail y ls '() rp fls)))
                             (values `(begin ,prep-exp ... ,x ... ,y) fls)))
        ((mref ,x ,y) (values `(begin ,prep-exp ... ,(substitute-proc-vars exp rp) ...) fls))
        ((,x ,y ,z) (guard (binop? x)) (values `(begin ,prep-exp ... ,(substitute-proc-vars exp rp) ...) fls))
        (,x (guard triv? x) (values `(begin ,prep-exp ... ,(substitute-proc-vars exp rp) ...) fls))
        ((,x ,y ...) (guard (triv? x)) (values `(begin ,prep-exp ... ,(substitute-proc-vars exp rp) ...) fls))))
  
    ;;frame-pointer-register, return-address-register, and return-value-register    
    (define (Body exp params)
      (match exp
        ((locals (,x ...) ,y)
         (let*-values (((exp1 params1) (assign-params-reg params '() parameter-registers))
                       ((exp2 params2) (assign-params-frames params1 '() 0)))
           (let* ((rp (get-unique-name-p x 'rp))
                  (rpset `(set! ,rp ,return-address-register))
                  (fexp (cons rpset (append exp1 exp2)))
                  (ls `(,x ... ,rp ,params ...)))
             (let-values (((y fls) (Tail y ls fexp rp '())))
               `(locals (,ls ... ,(fold-right append '() fls) ...) (new-frames ,fls ,y))))))))
    
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,z ...) ,tail)) `(,x (lambda () ,(Body tail z))))))

    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y '())))))

    (define (impose-calling-conventions exp)                   ;get-trace-define
      (Program exp))
    
    (impose-calling-conventions program)))
