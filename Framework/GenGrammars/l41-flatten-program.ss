;; Automatically generated file -- DO NOT MODIFY
(library (Framework GenGrammars l41-flatten-program)
  (export verify-grammar:l41-flatten-program)
  (import (chezscheme) (Framework match) (Framework prims))
  (define (any . nested-bool-ls)
    (letrec ([helper (lambda (x)
                       (cond
                         [(not x) #f]
                         [(null? x) #f]
                         [(pair? x) (or (helper (car x)) (helper (cdr x)))]
                         [else x]))])
      (helper nested-bool-ls)))
  (define verify-grammar:l41-flatten-program
    (lambda (x)
      (define Statement
        (lambda (x)
          (match x
            [,e (guard (not [Label e])) #f]
            [(set! . ,bod)
             (and (match (cons 'set! bod)
                    [(set! ,(Loc -> x1) ,(Triv -> x2)) (any x2 x1)]
                    [,e (invalid-expr 'set! e)])
                  (match (cons 'set! bod)
                    [(set! ,(Loc -> x1)
                       (,(Binop -> x2) ,(Triv -> x3) ,(Triv -> x4)))
                     (any x4 x3 x2 x1)]
                    [,e (invalid-expr 'set! e)]))]
            [(if . ,bod)
             (and (match (cons 'if bod)
                    [(if (,(Relop -> x1) ,(Triv -> x2) ,(Triv -> x3))
                         (jump ,(Label -> x4)))
                     (any x4 x3 x2 x1)]
                    [,e (invalid-expr 'if e)])
                  (match (cons 'if bod)
                    [(if (not (,(Relop -> x1) ,(Triv -> x2) ,(Triv -> x3)))
                         (jump ,(Label -> x4)))
                     (any x4 x3 x2 x1)]
                    [,e (invalid-expr 'if e)]))]
            [(jump ,(Triv -> x1)) (any x1)]
            [,e (invalid-expr 'Statement e)])))
      (define Triv
        (lambda (x)
          (match x
            [,e (guard (not [Loc e])) #f]
            [,e (guard (not [Integer e])) #f]
            [,e (guard (not [Label e])) #f]
            [,e (invalid-expr 'Triv e)])))
      (define Loc
        (lambda (x)
          (match x
            [,e (guard (not [Reg e])) #f]
            [,e (guard (not [Disp e])) #f]
            [,e (invalid-expr 'Loc e)])))
      (define Conflict
        (lambda (x)
          (match x
            [,e (guard (not [Reg e])) #f]
            [,e (guard (not [UVar e])) #f]
            [,e (invalid-expr 'Conflict e)])))
      (define Prog
        (lambda (x)
          (match x
            [(code ,(Statement -> x1) ... ,(Statement -> x2))
             (any x2 x1)]
            [,e (invalid-expr 'Prog e)])))
      (let ([res (Prog x)])
        (if res
            (errorf 'verify-grammar:l41-flatten-program "~a" res)
            x)))))
