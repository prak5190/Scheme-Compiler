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
        [(begin ,x ,y ...) (Effect x) (validate `(begin ,y ...))]
        [(begin ,x ,y) (Effect x) (validate `(begin ,y))]
        [(begin ,x) (Effect x)]
        [(begin) #f]
        ))
    
    (validate program)
    ;; #| Tail : exp --> void
    ;; | Tail takes an expression and throws an error
    ;; | unless the expression qualifies as a tail.
    ;; |
    ;; | Tail --> (,Triv)
    ;; |       |  (begin ,Effect* ,Tail)
    ;; |#
    ;; (define (Tail env)
    ;;   (lambda (exp)
    ;;     (match exp
    ;;       [(begin ,[Effect -> e*] ... ,[(Tail env) -> t]) exp]
    ;;       [(,t) (guard
    ;;              (if (label? t) (and (member t env) #t))
    ;;              (not (integer? t)) ; architectural nuance.  Jump must be to label, not address.
    ;;              )
    ;;        (values)]
    ;;       [,x (errorf who "invalid tail: ~s" x)]
    ;;       )
    ;;     )
    ;;   )

    ;; #| This block acts as the heartbeat of verify-scheme
    ;; | by doing the work naturally expected to be within
    ;; | some helper 'verify-program'.
    ;; |
    ;; | Program --> (letrec ([<label> (lambda () ,Tail)]*) ,Tail)
    ;; |#
    ;; (match program
    ;;   [(letrec ([,lbl (lambda () ,t*)] ...) ,t)
    ;;    (let ([env (cons 'r15 lbl)])
    ;;      (for-each (Tail env) t*)
    ;;      ((Tail env) t)
    ;;      (if (set? (map string->number (map extract-suffix lbl)))
    ;;          program
    ;;          (errorf who "Label suffixes must be unique: ~s" lbl)))]
    ;;   [,x (errorf who "invalid syntax for Program: expected (letrec ([<label> (lambda () ,Tail)]*) ,Tail) but received ~s" program)]
      )
  (define-who (generate-x86-64 program)
    'blahfasjdhajusd)
  (verify-scheme  '(begin (set! r11 5) (set! rax r11)))
  )



