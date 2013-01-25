(library (Compiler discard-call-live)
  (export discard-call-live)
  (import (chezscheme)
          (Framework helpers)
          (Framework match))

(define-who discard-call-live
  (define Tail
    (lambda (tail)
      (match tail
        [(begin ,ef* ... ,[tail]) `(begin ,ef* ... ,tail)]
        [(if ,test ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(,t ,live* ...) `(,t)]
        [,tail (error who "invalid Tail ~s" tail)])))
  (define Body
    (lambda (bd)
      (match bd
        [(locate ([,uvar* ,loc*] ...) ,[Tail -> tail])
         `(locate ([,uvar* ,loc*] ...) ,tail)]
        [,bd (error who "invalid Body ~s" bd)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))  
  
)