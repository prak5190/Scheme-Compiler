(library (Compiler flatten-program)
  (export flatten-program)
  (import (chezscheme)
          (Framework helpers)
	  (Framework GenGrammars l41-flatten-program)
          (Framework match))

(define-who flatten-program
  (define Effect
    (lambda (ef)
      (match ef
        [(set! ,var ,rhs) `((set! ,var ,rhs))]
        [,ef (error who "invalid syntax for Effect ~s" ef)])))
  (define Tail
    (lambda (tail)
      (match tail
        [(,t) `((jump ,t))]
        [(begin ,[Effect -> code**] ... ,[code*])
         `(,code** ... ... ,code* ...)]
        [,tail (error who "invalid syntax for Tail ~s" tail)])))
  (lambda (program)
    (verify-grammar:l41-flatten-program 
     (match program
       [(letrec ([,label* (lambda () ,[Tail -> code**])] ...) ,[Tail -> code*])
	(let ([code** (map cons label* code**)])
	  `(code ,code* ... ,code** ... ...))]
       [,program (error who "invalid syntax for Program: ~s" program)]))))

)