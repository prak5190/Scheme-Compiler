(library (Compiler remove-let)
  (export
   remove-let
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (remove-let program)
    ;; Doesn't modify expression , so all methods return list of locals
    (define (Tail exp ls)
      (match exp
        ((alloc ,v) (values exp ls))
;        ((if ,[(Pred exp ls)-> x] ,[ ->] ,[]) (
        ))

    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) ,tail)) `(,x (lambda (,y ...) ,(Tail tail y))))))
    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Tail y '())))))

    (define (remove-let exp)                   ;get-trace-define
      (Program exp))    
    (remove-let program)))
