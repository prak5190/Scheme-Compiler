;#!chezscheme
(library (Compiler verify-scheme)
  (export
    verify-scheme  
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers))
  ;;;; TODO Need to add machine constraints error 
  ;; If it is a binary operator or not
  (define (binop? exp)                   ;get-trace-define
    (define binops '(+ - * logand logor sra))
    (and (memq exp binops) #t))
  
  ;; If it is a relational operator or not
  (define (relop? exp)                   ;get-trace-define
    (define relops '(< > = <= >=))
    (and (memq exp relops) #t))

  (define (Register? x)
    (or (register? x) (uvar? x)))
  
  ;; A variable is a either a register or a frame variable 
  (define (var? exp)                   ;get-trace-define
                (or (register? exp) (frame-var? exp) (uvar? exp)))  
  ;; extract-suffix name -> use this to enforce unique name
  ;; Using define-who macro 
  (define-who (verify-scheme program)
    ;; An exp is divided into Program, Body,Tail, Effect, Var, Triv
    ;; Writing a function for each part
    ;; Trivial is Var | int | label  -- No int? so putting int64?

    (define (Loc x)
      (if (or (register? x) (frame-var? x))
          x
          (errorf who "Not a Loc ~s" x)))
    
    (define (Var x)
      (if (or (uvar? x) (Loc x))
          x
          (errorf who "Not a Var ~s" x)))
    
    ;; making triv? into Triv which will now take list of label and uvar to check for unbounded vars
    ;; Returns true or false - throws error for unbounded variable    
    (define (Triv exp labells uvarls)                   ;get-trace-define      
      (if (or (int64? exp)
          (if (label? exp)
              (if (assv exp labells)
                  #t
                  (errorf who "unbound label name ~s" exp))
              #f)
          (if (uvar? exp)
              (cond
               ((memq exp uvarls)  #t)
               (else (errorf who "unbound uvar name ~s" exp)))
              #f)
          ;; unbound vars err out before var?
          (var? exp))
          exp
          (errorf who "Not a Triv ~s" exp)))

    ;;    (define (verify-labels ls)

    ;; Validate letrec label exp :   [label (lambda() Tail)]
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ;; Someone has to tell me why when i do extract suffix and then do eqv? on result, it always gives me false , I got  1 == 1 as #f 
        ;; Hackish way to ensure that the extracted suffix is treated normally 
        ((,x (lambda () ,tail)) (guard (label? x))         
         `((,x . ,tail) . ,(string->number (extract-suffix x))))
         (,else (errorf who "invalid letrec exp ~s" exp))))

    ;; Validate Body locate exp
    (define (BodyExp exp)                   ;get-trace-define
      (match exp
        (,x (guard (uvar? x)) x)
        (,else (errorf who "invalid body exp ~s" exp))))        
    
    ;; Validate Effect
    (define (Effect exp ls locls)                   ;get-trace-define
      (match exp        
        ;; (set! Var1 int64)
        ;; (set! Var1 Var2)
        [(nop) #t]
        [(if ,x ,y ,z) (Pred x ls locls) (Effect y ls locls) (Effect z ls locls)]
        [(begin ,x ... ,y) (for-each (lambda(x) (Effect x ls locls)) x) (Effect y ls locls)]
        [(set! ,v (,b ,t1 ,t2)) (guard (and (var? v) (Triv v ls locls) (binop? b)
                                            (Triv t1 ls locls) (Triv t2 ls locls)))
         (if (and 
              (not (and (label? t1) (label? t2)))
              (not (and (frame-var? t1) (frame-var? t2)))
              (if (eqv? b '*) (Register? v) #t)
              (if (int64? t2) (int32? t2) #t)
              (if (eqv? b 'sra) (or (int32? t1) (uint6? t2)) #t)
              (eqv? v t1))
             exp
             (errorf who "Violates Machine constraints : ~s" exp))]
        [(set! ,v ,t) (guard (and (var? v) (Triv v ls locls)
                                  (Triv t ls locls)))
         (if (and
              (not (and (frame-var? v) (frame-var? t)))
              (if (and (int64? t) (not (int32? t))) (Register? v) #t)
              (if (label? t) (Register? v) #t))
             exp
             (errorf who "Violates Machine constraints : ~s" exp))]       
      [,x (errorf who "invalid effect: ~s" x)]))

    
    ;; Validate Pred
    (define (Pred exp ls locls)
      (match exp
        ((true) #t)
        ((false) #t)
        ((if ,x ,y ,z) (Pred x ls locls) (Pred y ls locls) (Pred z ls locls))
        ((begin ,x ... ,p)
         (for-each (lambda(x) (Effect x ls locls)) x)
         (Pred p ls locls))
        ((,x ,y ,z) (guard (relop? x)
                           (Triv y ls locls) (Triv z ls locls)
                           (not (label? y)) (not (label? z))
                           (not (int32? y))
                           (let ((y y)
                                 (z z))
                             (and                              
                              (if (int64? z) (int32? z) #t)                              
                              (not (and (frame-var? y) (frame-var? z)))))) #t)
        (,x (errorf who "invalid Pred: ~s" x))))
    
    ;; Validate Body
    (define (Body exp ls)
      (match exp
        ((locals (,[BodyExp -> x] ...) ,y)
         (checkAllUnique x)
         (Tail y ls x))
        (,x (errorf who "invalid Body: ~s" x))))

       
    ;; Validate Tail
    (define (Tail exp ls locls)                   ;get-trace-define
      (match exp
        ((begin ,x ... ,t)
         (for-each (lambda(x) (Effect x ls locls)) x)
         (Tail t ls locls))
        ((if ,x ,y ,z) (Pred x ls locls) (Tail y ls locls) (Tail z ls locls))
        ((,x ,y ...) (guard (and (Triv x ls locls) (not (int64? x)) (map Loc y))) #t)
        (,else (errorf who "invalid Tail ~s" exp))))

    
    ;; Iterate through labels and validate 
    (define (Label ls)
      (map (lambda(x) (Body (cdr x) ls)) ls))
    
    ;; Checks if all elements are unique ,if not unique then throws error
    (define (checkAllUnique ls)
      (cond
       ((null? ls) #t)
       ((not (memq (car ls) (cdr ls))) (checkAllUnique (cdr ls)))
       (else (errorf who "Label suffix numbers repeat with number ~s " (car ls)))))

    ;; Validate Program
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y)
         (checkAllUnique (map (lambda(x) (cdr x)) x))
         (Label (map (lambda(x) (car x)) x))
         (Body y (map (lambda(x) (car x)) (append x '()))))
        (,else (errorf who "invalid Program ~s" exp))))              

    (define (validate exp)                   ;get-trace-define
      (Program exp))
    ;; Quick fix -- TODO - restructure code so that validate returns the full exp
    ;; Ignore -- This is becoming useful now    
    (and (validate program) program)))
