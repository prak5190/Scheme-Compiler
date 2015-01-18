;#!chezscheme
(library (Compiler passes)
  (export
    verify-scheme
    generate-x86-64
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers))
  
  ;; If it is a binary operator or not
  (define (binop? exp)
    (define binops '(+ - *))
    (and (memq exp binops) #t))
  
  ;; Uses register? from helpers.ss - only register names are allowed as var
  (define (var? exp)
    (register? exp))

  ;; Using define-who macro 
  (define-who (verify-scheme program)
    ;; Validate individual sexp
    ;; Return exp or throws error
    (define (Effect exp)
      (match exp        
        ;; (set! Var1 int64)
        ;; (set! Var1 Var2)
        [(set! ,v ,t) (guard (and (var? v) (or (var? t) (int64? t)))) exp]
        ;; (set! Var1 (Binop Var1 int32 ))
        ;; (set! Var1 (Binop Var1 Var2))
        [(set! ,v (,b ,t1 ,t2)) (guard (and (var? v) (binop? b) (var? t1) (eqv? v t1)
                                            (or (int32? t2) (var? t2)))) exp]
        [,x (errorf who "invalid effect: ~s" x)]))
    
    (define (validate exp)
      (match exp
        ;;(begin Statement*)
        [(begin ,x) (Effect x)]
        [(begin ,x ,y) (Effect x) (validate `(begin ,y))]
        [(begin ,x ,y ...) (Effect x) (validate `(begin ,y ...))]
        [,else (errorf who "invalid exp: ~s" exp)]
        ))
    (validate program))
  ;; Generate x86-64 Code
  (define-who (generate-x86-64 exp)
    (define (binop->instr binop)
      (match binop 
        [+ 'addq]
        [- 'subq]
        [* 'imulq]        
        [,x (errorf who "unexpected binop ~s" x)]))
    (define (convertStatement code)
      (match code
        ;; (set! Var1 (Binop Var1 int32 ))
        ;; (set! Var1 (Binop Var1 Var2))
        [(set! ,dst (,b ,t1 ,src)) (emit (binop->instr b) (rand->x86-64-arg src) (rand->x86-64-arg dst))]
        ;; (set! Var1 (Binop Var1 int32 ))
        ;; (set! Var1 (Binop Var1 Var2))
        [(set! ,dst ,src) (emit 'movq (rand->x86-64-arg src) (rand->x86-64-arg dst))]
        [,else (errorf who "unexpected expression ~S" else) else]))
    
    (define (convert exp)
        (match exp         
          ;;(begin Statement*)
          [(,code ,code* ...) (convertStatement code) (convert code*)]          
          ;; [(begin ,x) (convertStatement x) (emit 'ret)]
          ;; [(begin ,x ,y) (convertStatement x) (convert `(begin ,y))]
          ;; [(begin ,x ,y ...) (convertStatement x) (convert `(begin ,y ...))]
          [,else (errorf who "unexpected statemen222222t ~S" else) else]
          ))    
    (emit-program (convertStatement exp))))




