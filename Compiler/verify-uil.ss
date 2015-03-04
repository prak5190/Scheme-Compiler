(library (Compiler verify-uil)
  (export
   verify-uil
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (verify-uil program)  
    (define (Value exp)
      (match exp
        ((if ,[Pred -> x] ,[Effect -> y] ,[Effect -> z]) exp)
        ((begin ,[Effect -> x] ... ,[Value -> y]) exp)
        ((alloc ,[Value -> x]) exp)
        ((mref ,[Value -> x]) exp)
        ((,x ,y ,z) (guard (binop? x)) (let ((y (Value y))
                                             (z (Value z))) exp))
        ((,[Value -> x] ,[Value -> y] ...) (guard (triv? x)) exp)        
        (,x (guard triv? x) exp)
        (,x (error who "invalid Value ~a" x))))

    (define (Effect exp)
      (match exp
        ((nop) exp)
        ((if ,[Pred -> x] ,[Effect -> y] ,[Effect -> z]) exp)
        ((begin ,[Effect -> x] ... ,[Effect -> y]) exp)
        ((mset! ,[Value -> x] ,[Value -> y] ,[Value -> z]) exp)
        ((set! ,x ,[Value -> y]) (guard (uvar? x)) exp)
        (,x (error who "invalid Effect ~a" x))))    
        
        
    (define (Pred exp)
      (match exp
        ((true) exp)
        ((false) exp)
        ((if ,[Pred -> x] ,[Pred -> y] ,[Pred -> z]) exp)
        ((begin ,[Effect -> x] ... ,[Pred -> y]) exp)
        ((,x ,y ,z) (guard (relop? x))  (let ((y (Value y))
                                              (z (Value z))) exp))
        (,x (error who "invalid Pred ~a" x))))
                
    (define (Tail exp)
      (match exp
        ((if ,[Pred -> x] ,[Tail -> y] ,[Tail -> z]) exp)
        ((begin ,[Effect -> x] ... ,[Tail -> y]) exp)
        ((alloc ,[Value -> x]) exp)
        ((mref ,[Value -> x]) exp)
        ((,x ,y ,z) (guard (binop? x)) (let ((y (Value y))
                                             (z (Value z))) exp))
        ((,[Value -> x] ,[Value -> y] ...) (guard (triv? x)) exp)
        (,x (guard triv? x) exp)
        (,x (error who "invalid Tail ~a" x))))
    
    (define (Body exp)
      (match exp
        ((locals (,x ...) ,y)  (let ((y (Tail y))) exp))
        (,x (error who "invalid Locals ~a" x))))

    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,[Body -> tail])) exp)
        (,x (error who "invalid lambda ~a" x))))

    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y '())))))

    (define (verify-uil exp)                   ;get-trace-define
      (Program exp))
    
    (verify-uil program)))
