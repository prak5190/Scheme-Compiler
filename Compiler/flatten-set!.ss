(library (Compiler flatten-set!)
  (export
   flatten-set!
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (flatten-set! program)
    ;; (get-unique-name ls)   
    (define (add-begin ex)
      (match ex
        ((,x ,y ,z) (guard (relop? x)) ex)
        ((,x) x)
        ((if ,x ,y ,z) `(if ,x ,y ,z))
        ((begin ,x ...) `(begin ,x ...))
        ((,x ,y ...) `(begin ,x ,y ...))))

         ;; (let ((n (get-unique-name ls)))

    ;; This is called for set! only - so it will always add a set! when removing if and begin
    ;; Returns list of exp ending with set! and  list of vars
    (define (Value xorig exp ls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((x ls) (Pred x ls))                                     
                                     ((yls ls) (Value xorig y ls))
                                     ((zls ls) (Value xorig z ls)))
                         (values `((if ,x ,(make-begin yls) ,(make-begin zls))) ls)))
        ((begin ,x ... ,z) (let*-values (((expls ls) (Effect* x ls))
                                         ((exp2 ls) (Value xorig z ls)))
                             (values `((begin ,expls ... ,exp2 ...)) ls)))
        ((,x ,y ,z) (guard (binop? x)) (values `((set! ,xorig (,x ,y ,z))) ls))
        (,x (guard triv? x) (values `((set! ,xorig ,x)) ls))))
      
    ;;  Returns exp list and list of variable
    (define (Effect exp ls)
      (match exp       
        ((set! ,x ,y) (Value x y ls))
        ((if ,x ,y ,z) (let*-values (((x ls) (Pred x ls))
                                     ((yls ls) (Effect y ls))
                                     ((zls ls) (Effect z ls)))
                         (values `((if ,x ,y ,z)) ls)))
        (,else (values `(,exp) ls))))
    
    ;;  Returns exp list and list of vars 
    (define (Effect* exp ls)
      (cond
       ((null? exp) (values '() ls))
       (else (let*-values (((exl ls) (Effect (car exp) ls))
                           ((ex2 ls) (Effect* (cdr exp) ls)))
               (values (append exl ex2) ls)))))

               ;;   (((exl1 ls) (Effect (car exp) ls))
               ;;            ((exl2 ls) (Effect* (cdr exp) ls)))
               ;; (values `((append exl1 exl2)) ls)))))
    ;; Returns exp and list of vars 
    (define (Pred exp ls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((x ls) (Pred x ls))
                                     ((y ls) (Pred y ls))
                                     ((z ls) (Pred z ls)))
                         (values `(if ,x ,y ,z) ls)))
        ((begin ,x ... ,y) (let*-values (((x ls) (Effect* x ls))
                                        ((y ls) (Pred y ls)))
                             (values `(begin ,x ... ,y) ls)))
        (,else (values exp ls))))
    ;; Return exp and list of vars
    (define (Tail exp ls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((x ls) (Pred x ls))
                                     ((y ls) (Tail y ls))
                                     ((z ls) (Tail z ls)))
                         (values `(if ,x ,y ,z) ls)))        
        ((,x ,y ,z) (guard (binop? x)) (values exp ls))
        ((begin ,x ... ,y) (let*-values (((x ls) (Effect* x ls))
                                        ((y ls) (Tail y ls)))
                             (values `(begin ,x ... ,y) ls)))
        ((,x ...) (values exp ls))                   
        (,x (guard triv? x) (values exp ls))))
    
    (define (Body exp)
      (match exp
        ((locals (,x ...) ,y)  (let-values (((y x) (Tail y x)))
                                 `(locals (,x ...)  ,y)))))
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,tail)) `(,x (lambda (,y ...) ,(Body tail))))))

    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))

    (define (flatten-set! exp)                   ;get-trace-define
      (Program exp))
    
    (flatten-set! program)))
