(library (Compiler flatten-program)
  (export
   flatten-program
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers))   

  (define-who (flatten-program program)
    ;; An exp is divided into Program, Tail, Effect, Var, Triv
    ;; Writing a function for each part

    ;; Validate letrec label exp :   [label (lambda() Tail)]
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,y))  `(,x . ,y))))

    ;; Replace function with jump statements
    (define (replaceFunc k i) (cons (car k) (Tail (cdr k) i)))
    
    ;; Validate Tail
    (define (Tail exp i)                   ;get-trace-define
      (match exp
        ((if ,x (,y) (,z)) (cond
                            ((eqv? i y) `((if (not ,x) (jump ,z)) (jump ,y)))
                            (else  `((if ,x (jump ,y))  (jump ,z)))))                               
        ((begin ,x ... ,t) (append x (Tail t i)))
        ((,x)  `((jump ,x)))))
    
    ;; Validate Program
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x]  ...) ,y) (let (( k (fold-right (lambda(a s)                                                      
                                                      `(,(car a) . ,(append (replaceFunc a (car s)) (cdr s)))) '(_) x)))
                                          (append `(code ,(Tail y (car k)) ...) (cdr k)))))) 
                                            
    (define (flatten exp)                   ;get-trace-define
      (Program exp))
    (flatten program))  
)
