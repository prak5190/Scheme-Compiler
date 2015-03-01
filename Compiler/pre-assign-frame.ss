(library (Compiler pre-assign-frame)
  (export
   pre-assign-frame
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
    
  (define-who (pre-assign-frame program)
    (define (frame-var-or-uvar? x)
      (or (frame-var? x) (uvar? x)))
    
    (define (Body exp)
      (match exp
        ((locals (,x ...) ,y)  `(locals (,x ...)                   
                                        (frame-conflict cg ,y)))))

    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x (lambda () ,(Body tail))))))

    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))

    (define (pre-assign-frame exp)                   ;get-trace-define
      (Program exp))
    
    (pre-assign-frame program)))
