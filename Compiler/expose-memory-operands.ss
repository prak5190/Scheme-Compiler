(library (Compiler expose-memory-operands)
  (export
   expose-memory-operands
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers))   
  
  (define-who (expose-memory-operands program)
    (define fp-offset 0)
    ;; Validate letrec label exp :   [label (lambda() Tail)]
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,(Tail -> y)))  `(,x (lambda () ,y)))))

    (define (convertFrameVar x)
      (if (frame-var? x) (make-disp-opnd frame-pointer-register
                                         (+ (ash (frame-var->index x) word-shift) fp-offset)) x)) 
    (define (expose-memory x offset)
      (if (and (register? x) (register? offset))
          (make-index-opnd x offset)
          (if (and (int32? x) (not (int64? offset)))
              (make-disp-opnd offset x)
              (make-disp-opnd x offset))))

    ;; make-index-opnd      
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
        [(return-point ,x ,y) `(return-point ,x ,(Effect y))]
        [(set! ,fp (+ ,fp ,off)) (guard (eqv? fp frame-pointer-register)) (set! fp-offset (+ fp-offset off)) exp]
        [(set! ,fp (- ,fp ,off)) (guard (eqv? fp frame-pointer-register)) (set! fp-offset (- fp-offset off)) exp]
        [(set! ,[convertFrameVar -> x] (mref ,y ,off)) `(set! ,x ,(expose-memory y off))]
        [(mset! ,x ,off ,[convertFrameVar -> v]) `(set! ,(expose-memory x off) ,v)]
        [(set! ,[convertFrameVar -> v] (,b ,[convertFrameVar -> t1] ,[convertFrameVar -> t2]))
         `(set! ,v (,b ,t1 ,t2))]
        [(set! ,[convertFrameVar -> v] ,[convertFrameVar -> t]) `(set! ,v ,t)]
        [(,x)  `(,(convertFrameVar x))]))
    
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
