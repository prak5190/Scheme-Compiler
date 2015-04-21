(library (Compiler introduce-allocation-forms)
  (export
   introduce-allocation-forms
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
    
  (define-who (introduce-allocation-forms program)
    (define (Body exp)
      (match exp
        ((locals (,x ...) ,y)  `(locals (,x ...)
                                        (ulocals()
                                                (locate ()
                                                        ,y))))))
    
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x (lambda () ,(Body tail))))))

    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))

    (define (introduce-allocation-forms exp)                   ;get-trace-define
      (Program exp))
    
    (introduce-allocation-forms program)))
