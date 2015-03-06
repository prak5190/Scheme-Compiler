;#!chezscheme
(library (Compiler finalize-locations)
  (export
   finalize-locations
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))

  ;; A variable is a either a register or a frame variable 
  (define (var? exp)                   ;get-trace-define
                (or (register? exp) (frame-var? exp) (uvar? exp)))  
  
  (define-who (finalize-locations program)
    ;; An exp is divided into Program, Body,Tail, Effect, Var, Triv , Label
    ;; Writing a function for each part
    ;; Trivial is Var | int | label  -- No int? so putting int64?
 
    ;; Validate letrec label exp :   [label (lambda() Tail)]
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x  (lambda() ,(Body tail))))))

    ;; Validate Body locate exp
    (define (BodyExp exp)                   ;get-trace-define
      (match exp
        ((,x ,y) (guard (and (or (register? y) (frame-var? y)) (uvar? x)))
         `((,x . ,y) . ,(string->number (extract-suffix x))))
        (,else (errorf who "invalid body exp ~s" exp))))
    
    ;; Find location and substitute if uvar otherwise return oldval
    ;; Assumes assq will always find which is preset in Triv
    (define (substituteLocation x ls)
      (if (uvar? x) 
          (cdr (assq x ls))
          x))
    
    ;; Validate Effect
    (define (Effect exp locls)                   ;get-trace-define
      (let ((q (lambda(x) (substituteLocation x locls))))
        (match exp        
          [(nop) exp]
          [(if ,x ,y ,z) `(if ,(Pred x locls) ,(Effect y locls) ,(Effect z locls))]
          [(begin ,x ... ,y) `(begin ,(map (lambda(x) (Effect x locls)) x) ... ,(Effect y locls))]
          [(return-point ,x ,y) `(return-point ,x ,(Effect y locls))]
          [(mset! ,[q -> v] ...) `(mset! ,v ...)]
          [(set! ,v (,[q -> x] ...)) `(set! ,(q v) (,x ...))]
          ;; [(set! ,v (,b ,t1 ,t2)) `(set! ,(q v) (,b ,(q t1) ,(q t2)))]
          [(set! ,v ,t) (let ((v (q v)) (t (q t)))
                          (if (eqv? v t)
                              '(nop)
                              `(set! ,v ,t)))]
          ((,x) `(,(substituteLocation x locls))))))
  
    ;; Validate Pred
    (define (Pred exp locls)
      (match exp
        ((true) exp)
        ((false) exp)
        ((if ,x ,y ,z) `(if ,(Pred x locls) ,(Pred y locls) ,(Pred z locls)))
        ((begin ,x ... ,p)
         `(begin ,(map (lambda(x) (Effect x locls)) x) ... ,(Pred p locls)))
        ((,x ,y ,z) `(,x ,(substituteLocation y locls) ,(substituteLocation z locls)))))
    
    ;; Validate Body
    (define (Body exp)
      (match exp
        ((locate (,[BodyExp -> x] ...) ,y)
         (Tail y (map (lambda(x) (car x)) x)))))
    
    ;; Validate Tail
    (define (Tail exp locls)                   ;get-trace-define
      (let ((q (lambda(x) (substituteLocation x locls))))
        (match exp
          ((begin ,x ... ,t)
           (append (cons `begin (map (lambda(x) (Effect x locls)) x)) `(,(Tail t locls))))
          ((if ,x ,y ,z) `(if ,(Pred x locls) ,(Tail y locls) ,(Tail z locls)))
          ((mref ,[q -> x] ,[q -> y]) (values))
          ((,x ,y ,z) (guard (relop? x)) `(,x ,(Tail y locls) ,(Tail z locls)))        
          ((,x) `(,(substituteLocation x locls))))))

    
    ;; Iterate through labels and validate 
    (define (Label ls)
      (map (lambda(x) (Body (cdr x))) ls))  

    ;; Validate Program
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,x ...) ,y)         
         `(letrec ,(map Exp x) ,(Body y))
;         (verifyLabels (map (lambda(x) (car x)) x))
;         (Body y (map (lambda(x) (car x)) x))
         )
        (,else (errorf who "invalid Program ~s" exp))))              

    (define (finalize-locations exp)                   ;get-trace-define
      (Program exp))
    
    (finalize-locations program)))
