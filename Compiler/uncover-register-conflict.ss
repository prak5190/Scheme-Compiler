(library (Compiler uncover-register-conflict)
  (export uncover-register-conflict)
  (import
    (chezscheme)
    (Compiler helpers)
    (Framework helpers)
    (Framework match))
  
;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2010

(define-who uncover-register-conflict
  (define Body
    (lambda (x)
      (match x
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (locate (,home* ...)
               (frame-conflict ,fv-ct ,tail))))
         ;; setup the conflict table ct for storing conflicts
         (let ([ct (do-uncover-conflict tail `(,local* ... ,ulocal* ...)
                     who register?)])
           `(locals (,local* ...)
              (ulocals (,ulocal* ...)
                (locate (,home* ...)
                  (frame-conflict ,fv-ct
                    (register-conflict ,ct ,tail))))))]
        [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))

)
