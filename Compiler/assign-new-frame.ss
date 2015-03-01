(library (Compiler assign-new-frame)
  (export
   assign-new-frame
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
    
  (define-who (assign-new-frame program)
    (define (frame-var-or-uvar? x)
      (or (frame-var? x) (uvar? x)))
    
    (define (Body exp)
      (match exp
        ((locals (,x ...) ,y)  `(locals (,x ...)                   
                                        (frame-conflict da ,y)))))

    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x (lambda () ,(Body tail))))))

    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))

    (define (assign-new-frame exp)                   ;get-trace-define
      (Program exp))
    
    (assign-new-frame program)))
