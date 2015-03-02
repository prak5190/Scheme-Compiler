(library (Compiler discard-call-live)
  (export
   discard-call-live
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
   
  ;; Using define-who macro 
  (define-who (discard-call-live program)
    
    (define (Effect exp)
      (match exp       
        ((if ,x ,y ,z) (let ((x (Pred x))
                             (y (Effect y))
                             (z (Effect z)))
                         `(if ,x ,y ,z)))
        ((begin ,x ...) (let*((x (map (lambda(x) (Effect x)) x)))
                          `(begin ,x ...)))
        ((return-point ,x ,y)  `(return-point ,x ,(Effect y)))
        ((,x ,y ...) (guard (triv? x)) `(,x))
        (,else exp)))
    
    (define (Pred exp)
      (match exp
        ((if ,x ,y ,z) (let ((x (Pred x))
                             (y (Pred y))
                             (z (Pred z)))
                         `(if ,x ,y ,z)))
        ((begin ,x ... ,y) (let*((x (map (lambda(x) (Effect x)) x))
                                 (y (Pred y)))
                             `(begin ,x ... ,y)))
        (,else exp)))
    
    (define (Tail exp)                   ;get-trace-define
      (match exp
        ((begin ,[Effect -> x] ... ,t) `(begin ,x ... ,(Tail t)))
        ((if ,x ,y ,z) `(if ,(Pred x) ,(Tail y) ,(Tail z)))
        ((,x ,y ...) `(,x))))
    
    (define (Body exp)
      (match exp
        ((locate (,x ...) ,y) `(locate (,x ...) ,(Tail y)))))


    ;; Validate letrec label exp :   [label (lambda() Tail)]
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x (lambda () ,(Body tail))))))

    ;; Validate Program
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))
    (Program program)))
