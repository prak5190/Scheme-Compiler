(library (Compiler expose-allocation-pointer)
  (export
   expose-allocation-pointer
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (expose-allocation-pointer program)    
    ;; Return pre , value and fls 
    (define (Value exp)
      (match exp
        ((if ,x ,y ,z) `(if ,(Pred x) ,(Value y) ,(Value z)))
        ((begin ,[Effect -> x] ... ,[Value -> y]) `(begin ,x ... ... ,y))
        ((mref ,[Value -> x] ,[Value -> y]) `(mref ,x ,y))
        ((alloc ,[Value -> x]) `(set! ,allocation-pointer-register (+ ,allocation-pointer-register ,x)))
        (,else else)))
      
    (define (Pred exp)
      (match exp
        ((if ,[Pred -> x] ,[Pred -> y] ,[Pred -> z]) `(if ,x ,y ,z))
        ((begin ,[Effect -> x] ... ,[Pred -> y]) `(begin ,x ... ... ,y))
        (,else else)))
    
    (define (Effect exp)
      (match exp       
        ((if ,[Pred -> x] ,[Effect -> y] ,[Effect -> z]) `((if ,x ,(make-begin y) ,(make-begin z))))
        ((begin ,[Effect -> x] ... ,[Effect -> y]) `((begin ,x ... ... ,y ...)))
        ((set! ,x (alloc ,y)) `((set! ,x ,allocation-pointer-register)
                                (set! ,allocation-pointer-register (+ ,allocation-pointer-register ,y))))
        ((set! ,x ,y) `((set! ,x ,y)))
        ((,x ,[Value -> y] ...) `((,x ,y ...)))))
    
    (define (Tail exp)      
      (match exp
        ((if ,x ,y ,z) `(if ,(Pred x) ,(Tail y) ,(Tail z)))
        ((begin ,[Effect -> x] ... ,[Tail -> y]) `(begin ,x ... ... ,y))
        ((,x ,y ...) exp)))
  
    ;;frame-pointer-register, return-address-register, and return-value-register    
    (define (Body exp)
      (match exp
        ((locals (,x ...) (new-frames ,nf ,y)) `(locals (,x ...) (new-frames ,nf ,(Tail y))))))
    
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ... ) ,tail)) `(,x (lambda (,y ...) ,(Body tail))))))

    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))

    (define (expose-allocation-pointer exp)                   ;get-trace-define
      (Program exp))
    
    (expose-allocation-pointer program)))
