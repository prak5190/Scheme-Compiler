(library (Compiler uncover-register-conflict)
  (export
   uncover-register-conflict
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:    
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  ;; A variable is a either a register or a frame variable 
  (define (var? exp)                   ;get-trace-define
                (or (register? exp) (frame-var? exp) (uvar? exp)))  
  ;; extract-suffix name -> use this to enforce unique name
  ;; Using define-who macro 
  (define-who (uncover-register-conflict program)
    ;; difference intersection union
    
    ;; An exp is divided into Program, Body,Tail, Effect, Var, Triv
    ;; Writing a function for each part    
    (define (register-or-uvar? x)
      (or (register? x) (uvar? x)))      
    ;; Validate Body
    (define (Body exp)
      (match exp
        ((locals (,x ...) (ulocals ,ul (locate ,z (frame-conflict ,fc ,y))))
         `(locals (,x ...)
                  (ulocals ,ul (locate ,z (frame-conflict ,fc 
                                                          (register-conflict ,(get-conflict y x register-or-uvar?) ,y))))))
        ((locate (,x ...) ,y) exp)))
      
    ;; Validate letrec label exp :   [label (lambda() Tail)]
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x (lambda () ,(Body tail))))))

    ;; Validate Program
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))

    (define (uncover-register-conflict exp)                   ;get-trace-define
      (Program exp))
    ;; Quick fix -- TODO - restructure code so that validate returns the full exp
    ;; Ignore -- This is becoming useful now    
    (uncover-register-conflict program)))
