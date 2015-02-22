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
        ((,x ,y ,z) (guard (binop? x)) `((set! ,return-value-register ,params)
                                         (,rp ,frame-pointer-register ,return-value-register)))
        ((,x ,y ...) (let*-values
                         (((exp1 params l1) (assign-val-reg params '() parameter-registers '()))
                          ((exp2 params l2) (assign-val-frames params '() 0 '())))
                       (let ((rpset `(set! ,return-value-register ,rp)))
                         `(,exp2 ... ,exp1 ... ,rpset
                                 (,x ,return-value-register ,frame-pointer-register ,l1 ... ,l2 ...)))))
        (,x (guard (triv? x)) `((set! ,return-value-register ,x)
                                (,rp ,frame-pointer-register ,return-value-register)))))
          
      
    ;;parameter-registers, frame-pointer-register,
    ;;return-value-register, and return-address-register. 
    (define (Tail exp ls prep-exp rp)      
          (match exp
            ((if ,x ,y ,z) `(begin ,prep-exp ... (if ,x ,(Tail y ls '() rp) ,(Tail z ls '() rp)))) 
            ((,x ,y ,z) (guard (binop? x)) `(begin ,prep-exp ... ,(substitute-proc-vars exp rp) ...))
            ((begin ,x ... ,y) `(begin ,prep-exp ... ,x ... ,(Tail y ls '() rp)))
            ((,x ,y ...)  `(begin ,prep-exp ... ,(substitute-proc-vars exp rp) ...))
            (,x (guard triv? x) `(begin ,prep-exp ... ,(substitute-proc-vars exp rp) ...))))
  
    ;;frame-pointer-register, return-address-register, and return-value-register    
    (define (Body exp params)
      (match exp
        ((locals (,x ...) ,y)
         (let*-values (((exp1 params1) (assign-params-reg params '() parameter-registers))
                       ((exp2 params2) (assign-params-frames params1 '() 0)))
           (let* ((rp (get-unique-name-p x 'rp))
                  (rpset `(set! ,rp ,return-address-register))
                  (fexp (cons rpset (append exp1 exp2))))
             (let ((y (Tail y x fexp rp)))
               `(locals (,x ... ,rp ,params ...) ,y)))))))
    
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,z ...) ,tail)) `(,x (lambda () ,(Body tail z))))))

    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y '())))))

    (define (impose-calling-conventions exp)                   ;get-trace-define
      (Program exp))
    
    (impose-calling-conventions program)))
