(library (Compiler uncover-locals)
  (export
   uncover-locals
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-who (uncover-locals program)
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

    (define (uncover-locals exp)                   ;get-trace-define
      (Program exp))    
    (uncover-locals program)))
