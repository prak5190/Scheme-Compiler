(library (Compiler finalize-frame-locations)
  (export
   finalize-frame-locations
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (finalize-frame-locations program)
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x  (lambda() ,(Body tail))))))  
    
    ;; Find location and substitute if uvar otherwise return oldval
    ;; Assumes assq will always find which is preset in Triv
    (define (substituteLocation x ls)
      (if (uvar? x)
          (let ((v (assq x ls)))
            (if v (cadr v) x))
          x))
    
    ;; Validate Effect
    (define (Effect exp locls)                   ;get-trace-define
      (let ((q (lambda(x) (substituteLocation x locls))))
        (match exp        
          [(nop) exp]
          [(if ,x ,y ,z) `(if ,(Pred x locls) ,(Effect y locls) ,(Effect z locls))]
          [(begin ,x ... ,y) `(begin ,(map (lambda(x) (Effect x locls)) x) ... ,(Effect y locls))]
          [(set! ,v (,b ,t1 ,t2)) `(set! ,(q v) (,b ,(q t1) ,(q t2)))]
          [(set! ,v ,t) (let ((v (q v)) (t (q t)))
                          (if (eqv? v t)
                              '(nop)
                              `(set! ,v ,t)))])))
  
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
        ((locals ,uv (ulocals ,uv2 (locate (,x ...) (frame-conflict ,cg ,y))))
         (let ((fls (filter (lambda(x) (frame-var? (cadr x))) x)))
           `(locals ,uv (ulocals ,uv2 (locate (,x ...) (frame-conflict ,cg ,(Tail y fls)))))))        
        ((locate (,x ...) ,y) exp)))
    
    ;; Validate Tail
    (define (Tail exp locls)                   ;get-trace-define
      (match exp
        ((begin ,x ... ,t)
         (append (cons `begin (map (lambda(x) (Effect x locls)) x)) `(,(Tail t locls))))
        ((if ,x ,y ,z) `(if ,(Pred x locls) ,(Tail y locls) ,(Tail z locls)))
        ((,x ,y ,z) (guard (relop? x)) `(,x ,(Tail y locls) ,(Tail z locls)))
        ((,x ...) exp)))    
    
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
