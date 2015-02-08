(library (Compiler discard-call-live)
  (export
   discard-call-live
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
  
  ;; A variable is a either a register or a frame variable 
  (define (var? exp)                   ;get-trace-define
                (or (register? exp) (frame-var? exp) (uvar? exp)))  
  ;; extract-suffix name -> use this to enforce unique name
  ;; Using define-who macro 
  (define-who (discard-call-live program)
    (define (Tail exp)                   ;get-trace-define
      (match exp
        ((begin ,x ... ,t) `(begin ,x ... ,(Tail t)))
        ((if ,x ,y ,z) `(if ,x ,(Tail y) ,(Tail z)))
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
