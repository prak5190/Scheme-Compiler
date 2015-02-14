(library (Compiler finalize-frame-locations)
  (export
   finalize-frame-locations
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
    
  (define-who (finalize-frame-locations program)
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

    (define (finalize-frame-locations exp)                   ;get-trace-define
      (Program exp))
    
    (finalize-frame-locations program)))
