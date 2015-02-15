(library (Compiler expose-frame-var)
  (export
   expose-frame-var
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers))   
  
  (define-who (expose-frame-var program)

    ;; Validate letrec label exp :   [label (lambda() Tail)]
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,(Tail -> y)))  `(,x (lambda () ,y)))))

    (define (convertFrameVar x)
      (if (frame-var? x) (make-disp-opnd 'rbp (* 8 (frame-var->index x))) x))
    
    (define (Pred exp)
      (match exp
        ((true) exp)
        ((false) exp)
        ((if ,x ,y ,z) `(if ,(Pred x) ,(Pred y) ,(Pred z)))
        ((begin ,x ... ,p)
         `(begin ,(map (lambda(x) (Effect x)) x) ... ,(Pred p)))
        ((,x ,y ,z) `(,x ,(convertFrameVar y) ,(convertFrameVar z)))))
    
    ;; Parse and convert frameVar
    (define (Effect exp)                   ;get-trace-define
      (match exp
        [(nop) '(nop)]
        [(begin ,[Effect -> x] ... ,[Effect -> t]) `(begin ,x ... ,t)]
        [(if ,x ,y ,z) `(if ,(Pred x) ,(Effect y) ,(Effect z))]
        [(set! ,[convertFrameVar -> v] (,b ,[convertFrameVar -> t1] ,[convertFrameVar -> t2]))
         `(set! ,v (,b ,t1 ,t2))]
        [(set! ,[convertFrameVar -> v] ,[convertFrameVar -> t]) `(set! ,v ,t)]))
    
    ;; Validate Tail
    (define (Tail exp)                   ;get-trace-define
      (match exp
        [(if ,x ,y ,z) `(if ,(Pred x) ,(Tail y) ,(Tail z))]
        ((begin ,[Effect -> x] ... ,[Tail -> t]) `(begin ,x ... ,t))
        ((,x)  (if (frame-var? x) `(,(convertFrameVar x)) `(,x)))))
    
    ;; Validate Program
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,(Tail -> y)) `(letrec (,x ...) ,y))))              

    (define (exposeFramevar exp)                   ;get-trace-define
      (Program exp))
    (exposeFramevar program))
)
