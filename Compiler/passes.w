#!chezscheme
(library (p423 compiler passes)
  (export
    verify-scheme
    finalize-locations
    expose-frame-var
    expose-basic-blocks
    flatten-program
    generate-x86-64)
  (import
    (chezscheme)
    (p423 compiler helpers)
    (p423 compiler match))

;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2010

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; Grammar assignment 3:
;;;
;;; Program --> (letrec ([<label> (lambda () <Body>)]*) <Body>)
;;; Body    --> (locate ([uvar <Loc>]*) <Tail>)
;;; Tail    --> (<Triv>)
;;;          |  (begin <Effect>* <Tail>)
;;;          |  (if <Pred> <Tail> <Tail>)
;;; Pred    --> (true)
;;;          |  (false)
;;;          |  (relop <Triv> <Triv>)
;;;          |  (begin <Effect>* <Pred>)
;;;          |  (if <Pred> <Pred> <Pred>)
;;; Effect  --> (nop)
;;;          |  (set! <Var> <Triv>)
;;;          |  (set! <Var> (<binop> <Triv> <Triv>)
;;;          |  (begin <Effect>+)
;;;          |  (if <Pred> <Effect> <Effect>)
;;; Var     --> uvar
;;;          |  Loc
;;; Loc     --> register
;;;          |  frame-var
;;; Triv    --> Var
;;;          |  int
;;;          |  label
;;;
;;; Where uvar is symbol.n where (n >= 0)
;;;       binop is +, -, *, logand, logor, or sra
;;;       relop is <, <=, =, >=, or >
;;;       register is rax, rcx, rdx, rbx, rbp, rdi, rsi, r8,
;;;                   r9, r10, r11, r12, r13, r14, or r15
;;;       label is symbol$n where (n >= 0)
;;;       frame-var is fvn where (n >= 0)
;;;
;;; If the value is a valid program, verify scheme returns the value
;;; unchanged; otherwise, it signals an error.

(define-who verify-scheme
  (define verify-x-list
    (lambda (x* x? what)
      (let loop ([x* x*] [idx* '()])
        (unless (null? x*)
          (let ([x (car x*)] [x* (cdr x*)])
            (unless (x? x)
              (error who "invalid ~s ~s found" what x))
            (let ([idx (extract-suffix x)])
              (when (member idx idx*)
                (error who "non-unique ~s suffix ~s found" what idx))
              (loop x* (cons idx idx*))))))))
  (define Var
    (lambda (env)
      (lambda (var)
        (unless (or (register? var) (frame-var? var) (uvar? var))
          (error who "invalid variable ~s" var))
        (when (uvar? var)
          (unless (assq var env)
            (error who "unbound uvar ~s" var)))
        var)))
  (define Loc
    (lambda (loc)
      (unless (or (register? loc) (frame-var? loc))
        (error who "invalid Loc ~s" loc))
      loc))
  (define Var->Loc
    (lambda (v env)
      (if (uvar? v) (cdr (assq v env)) v)))
  (define Triv
    (lambda (label* env)
      (lambda (t)
        (unless (or (register? t) (frame-var? t) (label? t) (uvar? t)
                    (and (integer? t) (exact? t)))
          (error who "invalid Triv ~s" t))
        (when (uvar? t)
          (unless (assq t env)
            (error who "unbound uvar ~s" t)))
        (when (label? t)
          (unless (memq t label*)
            (error who "unbound label ~s" t)))
        t)))
  (define Pred
    (lambda (label* env)
      (lambda (pr)
        (match pr
          [(true) (void)]
          [(false) (void)]
          [(begin ,[(Effect label* env) -> ef*] ... ,[pr]) (void)]
          [(if ,[test] ,[conseq] ,[altern]) (void)]
          [(,relop ,[(Triv label* env) -> x] ,[(Triv label* env) -> y])
           (unless (memq relop '(= < <= > >=))
             (error who "invalid predicate operator ~s" relop))
           (let ([x (Var->Loc x env)] [y (Var->Loc y env)])
             (unless (or (and (register? x)
                              (or (register? y)
                                  (frame-var? y)
                                  (int32? y)))
                         (and (frame-var? x)
                              (or (register? y)
                                  (int32? y))))
               (error who "~s violates machine constraints"
                      `(,relop ,x ,y))))]
          [,pr (error who "invalid Pred ~s" pr)]))))
  (define Effect
    (lambda (label* env)
      (lambda (ef)
        (match ef
          [(nop) (void)]
          [(set! ,[(Var env) -> x]
             (,binop ,[(Triv label* env) -> y] ,[(Triv label* env) -> z]))
           (unless (and (eq? y x)
                        (let ([x (Var->Loc x env)] [z (Var->Loc z env)])
                          (case binop
                            [(+ - logand logor)
                             (or (and (register? x)
                                      (or (register? z)
                                          (frame-var? z)
                                          (int32? z)))
                                 (and (frame-var? x)
                                      (or (register? z)
                                          (int32? z))))]
                            [(*)
                             (and (register? x)
                                  (or (register? z)
                                      (frame-var? z)
                                      (int32? z)))]
                            [(sra)
                             (and (or (register? x) (frame-var? x))
                                  (uint6? z))]
                            [else
                             (error who "invalid binary operator ~s" binop)])))
             (error who "~s violates machine constraints"
                    `(set! ,x (,binop ,y ,z))))]
          [(set! ,[(Var env) -> x] ,[(Triv label* env) -> y])
           (let ([x (Var->Loc x env)] [y (Var->Loc y env)])
             (unless (or (and (register? x)
                              (or (register? y)
                                  (frame-var? y)
                                  (int64? y)
                                  (label? y)))
                         (and (frame-var? x)
                              (or (register? y)
                                  (int32? y))))
               (error who "~s violates machine constraints" `(set! ,x ,y))))]
          [(begin ,[ef] ,[ef*] ...) (void)]
          [(if ,[(Pred label* env) -> test] ,[conseq] ,[altern]) (void)]
          [,ef (error who "invalid Effect ~s" ef)]))))
  (define Tail
    (lambda (label* env)
      (lambda (tail)
        (match tail
          [(begin ,[(Effect label* env) -> ef*] ... ,tail)
           ((Tail label* env) tail)]
          [(if ,[(Pred label* env) -> test] ,[conseq] ,[altern]) (void)]
          [(,[(Triv label* env) -> t])
           (when (integer? t)
             (error who "~s violates machine constraints" `(,t)))]
          [,tail (error who "invalid Tail ~s" tail)]))))
  (define Body
    (lambda (label*)
      (lambda (bd)
        (match bd
          [(locate ([,uvar* ,[Loc -> loc*]] ...) ,tail)
           (verify-x-list uvar* uvar? 'uvar)
           ((Tail label* (map cons uvar* loc*)) tail)]
          [,bd (error who "invalid Body ~s" bd)]))))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,bd*)] ...) ,bd)
       (verify-x-list label* label? 'label)
       (for-each (Body label*) bd*)
       ((Body label*) bd)]
      [,x (error who "invalid Program ~s" x)])
    x))

(define-who finalize-locations
  (define Var
    (lambda (env)
      (lambda (v)
        (if (uvar? v) (cdr (assq v env)) v))))
  (define Triv
    (lambda (env)
      (lambda (t)
        (if (uvar? t) (cdr (assq t env)) t))))
  (define Pred
    (lambda (env)
      (lambda (pr)
        (match pr
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[(Effect env) -> ef*] ... ,[pr]) `(begin ,ef* ... ,pr)]
          [(,relop ,[(Triv env) -> x] ,[(Triv env) -> y]) `(,relop ,x ,y)]
          [,pr (error who "invalid Pred ~s" pr)]))))
  (define Effect
    (lambda (env)
      (lambda (ef)
        (match ef
          [(nop) '(nop)]
          [(set! ,[(Var env) -> x]
             (,binop ,[(Triv env) -> y] ,[(Triv env) -> z]))
           `(set! ,x (,binop ,y ,z))]
          [(set! ,[(Var env) -> x] ,[(Triv env) -> y]) 
           (if (eq? x y) '(nop) `(set! ,x ,y))]
          [(begin ,[ef] ,[ef*] ...) `(begin ,ef ,ef* ...)]
          [(if ,[(Pred env) -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [,ef (error who "invalid Effect ~s" ef)]))))
  (define Tail
    (lambda (env)
      (lambda (tail)
        (match tail
          [(begin ,[(Effect env) -> ef*] ... ,[tail]) `(begin ,ef* ... ,tail)]
          [(if ,[(Pred env) -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(,[(Triv env) -> t]) `(,t)]
          [,tail (error who "invalid Tail ~s" tail)]))))
  (define Body
    (lambda (bd)
      (match bd
        [(locate ([,uvar* ,loc*] ...) ,[(Tail (map cons uvar* loc*)) -> tail])
         tail]
        [,bd (error who "invalid Body ~s" bd)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> bd*])] ...) ,[Body -> bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))

;;; expose-frame-var traverses the scheme source in the same grammar
;;; accepted by verify-scheme and changes frame-vars in the form
;;; fv0, fv1, etc. into explicit integer offsets from the register
;;; pointing to the frame-pointer register. To accomplish this,
;;; expose-frame-var makes use of make-disp-opnd which creates a
;;; displacement operand record expressing a register and fixed
;;; number displacment each displacement is the original frame var
;;; number multiplied by the word size (8 for 64-bit target machine)
;;; to get the byte offset.
;;; (i.e. fv0 => (make-disp-opnd frame-pointer-register 0)
;;;       fv1 => (make-disp-opnd frame-pointer-register 8)
;;;       fv2 => (make-disp-opnd frame-pointer-register 16)
;;;       fv3 => (make-disp-opnd frame-pointer-register 24)
;;;       ... well you get the idea.)
;;;
;;; Note: we use shift left by word-shift (3 for 64-bit target
;;; machine) to calculate the multiplication.

(define-who expose-frame-var
  (define Triv
    (lambda (t)
      (if (frame-var? t)
          (make-disp-opnd frame-pointer-register
            (ash (frame-var->index t) word-shift))
          t)))
  (define Pred
    (lambda (pr)
      (match pr
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[test])
         `(begin ,ef* ... ,test)]
        [(,relop ,[Triv -> tr1] ,[Triv -> tr2]) `(,relop ,tr1 ,tr2)]
        [,pr (error who "invalid Pred ~s" pr)])))
  (define Effect
    (lambda (st)
      (match st
        [(nop) '(nop)]
        [(set! ,[Triv -> var] (,binop ,[Triv -> t1] ,[Triv -> t2]))
         `(set! ,var (,binop ,t1 ,t2))]
        [(set! ,[Triv -> var] ,[Triv -> t])
         `(set! ,var ,t)]
        [(begin ,[ef] ,[ef*] ...) `(begin ,ef ,ef* ...)]
        [(if ,[Pred -> test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [,st (error who "invalid syntax for Effect ~s" st)])))
  (define Tail
    (lambda (tail)
      (match tail
        [(,[Triv -> t]) `(,t)]
        [(begin ,[Effect -> ef*] ... ,[tail])
         `(begin ,ef* ... ,tail)]
        [(if ,[Pred -> test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [,tail (error who "invalid syntax for Tail ~s" tail)])))
  (lambda (program)
    (match program
      [(letrec ([,label* (lambda () ,[Tail -> tail*])] ...) ,[Tail -> tail])
       `(letrec ([,label* (lambda () ,tail*)] ...) ,tail)]
      [,program (error who "invalid syntax for Program: ~s" program)])))

(define-who expose-basic-blocks
  (define (Tail x)
    (match x
      [(if ,pred ,[conseq cb*] ,[altern ab*])
       (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
         (let-values ([(pred pb*) (Pred pred clab alab)])
           (values pred
             `(,pb* ...
               [,clab (lambda () ,conseq)]
               ,cb* ...
               [,alab (lambda () ,altern)]
               ,ab* ...))))]
      [(begin ,effect* ... ,[tail tb*])
       (let-values ([(x xb*) (Effect* effect* `(,tail))])
         (values x `(,xb* ... ,tb* ...)))]
      [(,triv) (values `(,triv) '())]
      [,x (error who "invalid Tail ~s" x)]))
  (define (Pred x tlab flab)
    (match x
      [(true) (values `(,tlab) '())]
      [(false) (values `(,flab) '())]
      [(if ,pred ,[conseq cb*] ,[altern ab*])
       (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
         (let-values ([(pred pb*) (Pred pred clab alab)])
           (values pred
             `(,pb* ...
               [,clab (lambda () ,conseq)]
               ,cb* ...
               [,alab (lambda () ,altern)]
               ,ab* ...))))]
      [(begin ,effect* ... ,[pred pb*])
       (let-values ([(x xb*) (Effect* effect* `(,pred))])
         (values x `(,xb* ... ,pb* ...)))]
      [(,relop ,triv1 ,triv2)
       (values `(if (,relop ,triv1 ,triv2) (,tlab) (,flab)) '())]
      [,x (error who "invalid Pred ~s" x)]))
  (define (Effect* x* rest*)
    (match x*
      [() (values (make-begin rest*) '())]
      [(,x* ... ,x) (Effect x* x rest*)]))
  (define (Effect x* x rest*)
    (match x
      [(nop) (Effect* x* rest*)]
      [(set! ,lhs ,rhs) (Effect* x* `((set! ,lhs ,rhs) ,rest* ...))]
      [(if ,pred ,conseq ,altern)
       (let ([clab (unique-label 'c)]
             [alab (unique-label 'a)]
             [jlab (unique-label 'j)])
         (let-values ([(conseq cb*) (Effect '() conseq `((,jlab)))]
                      [(altern ab*) (Effect '() altern `((,jlab)))]
                      [(pred pb*) (Pred pred clab alab)])
           (let-values ([(x xb*) (Effect* x* `(,pred))])
             (values x
               `(,xb* ...
                 ,pb* ...
                 [,clab (lambda () ,conseq)]
                 ,cb* ...
                 [,alab (lambda () ,altern)]
                 ,ab* ...
                 [,jlab (lambda () ,(make-begin rest*))])))))]
      [(begin ,effect* ...) (Effect* `(,x* ... ,effect* ...) rest*)]
      [,x (error who "invalid Effect ~s" x)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Tail -> tail* b**])] ...) ,[Tail -> tail b*])
       `(letrec ([,label* (lambda () ,tail*)] ... ,b** ... ... ,b* ...) ,tail)]
      [,x (error who "invalid Program ~s" x)])))

; alternative with special-casing obviated by optimize-jumps
#;(define-who expose-basic-blocks
  (define (Tail x)
    (match x
     ; special-case cases where conseq and/or altern are jumps to labels
      [(if ,pred (,clab) (,alab))
       (guard (label? clab) (label? alab))
       (Pred pred clab alab)]
      [(if ,pred ,[conseq cb*] (,alab))
       (guard (label? alab))
       (let ([clab (unique-label 'c)])
         (let-values ([(tail xb*) (Pred pred clab alab)])
           (values tail
             `(,xb* ...
               [,clab (lambda () ,conseq)]
               ,cb* ...))))]
      [(if ,pred (,clab) ,[altern ab*])
       (guard (label? clab))
       (let ([alab (unique-label 'a)])
         (let-values ([(tail xb*) (Pred pred clab alab)])
           (values tail
             `(,xb* ...
               [,alab (lambda () ,altern)]
               ,ab* ...))))]
      [(if ,pred ,[conseq cb*] ,[altern ab*])
       (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
         (let-values ([(pred pb*) (Pred pred clab alab)])
           (values pred
             `(,pb* ...
               [,clab (lambda () ,conseq)]
               ,cb* ...
               [,alab (lambda () ,altern)]
               ,ab* ...))))]
      [(begin ,effect* ... ,[tail tb*])
       (let-values ([(x xb*) (Effect* effect* `(,tail))])
         (values x `(,xb* ... ,tb* ...)))]
      [(,triv) (values `(,triv) '())]
      [,x (error who "invalid Tail ~s" x)]))
  (define (Pred x tlab flab)
    (match x
      [(true) (values `(,tlab) '())]
      [(false) (values `(,flab) '())]
     ; (not (not x))
      [(if ,x (true) (false)) (Pred x tlab flab)]
     ; (not x)
      [(if ,x (false) (true)) (Pred x flab tlab)]
     ; (or (not x) y)
      [(if ,pred ,[conseq cb*] (true))
       (let ([clab (unique-label 'c)])
         (let-values ([(expr xb*) (Pred pred clab tlab)])
           (values expr
             `(,xb* ...
               [,clab (lambda () ,conseq)]
               ,cb* ...))))]
     ; (and x y)
      [(if ,pred ,[conseq cb*] (false))
       (let ([clab (unique-label 'c)])
         (let-values ([(expr xb*) (Pred pred clab flab)])
           (values expr
             `(,xb* ...
               [,clab (lambda () ,conseq)]
               ,cb* ...))))]
     ; (or x y)
      [(if ,pred (true) ,[altern ab*])
       (let ([alab (unique-label 'a)])
         (let-values ([(expr xb*) (Pred pred tlab alab)])
           (values expr
             `(,xb* ...
               [,alab (lambda () ,altern)]
               ,ab* ...))))]
     ; (and (not x) y)
      [(if ,pred (false) ,[altern ab*])
       (let ([alab (unique-label 'a)])
         (let-values ([(expr xb*) (Pred pred flab alab)])
           (values expr
             `(,xb* ...
               [,alab (lambda () ,altern)]
               ,ab* ...))))]
      [(if ,pred ,[conseq cb*] ,[altern ab*])
       (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
         (let-values ([(pred pb*) (Pred pred clab alab)])
           (values pred
             `(,pb* ...
               [,clab (lambda () ,conseq)]
               ,cb* ...
               [,alab (lambda () ,altern)]
               ,ab* ...))))]
      [(begin ,effect* ... ,[pred pb*])
       (let-values ([(x xb*) (Effect* effect* `(,pred))])
         (values x `(,xb* ... ,pb* ...)))]
      [(,relop ,triv1 ,triv2)
       (values `(if (,relop ,triv1 ,triv2) (,tlab) (,flab)) '())]
      [,x (error who "invalid Pred ~s" x)]))
  (define (Effect* x* rest*)
    (match x*
      [() (values (make-begin rest*) '())]
      [(,x* ... ,x) (Effect x* x rest*)]))
  (define (Effect x* x rest*)
    (match x
      [(nop) (Effect* x* rest*)]
      [(set! ,lhs ,rhs) (Effect* x* `((set! ,lhs ,rhs) ,rest* ...))]
     ; (if x y)
      [(if ,pred ,conseq (nop))
       (let ([clab (unique-label 'c)] [jlab (unique-label 'j)])
         (let-values ([(conseq cb*) (Effect '() conseq `((,jlab)))]
                      [(expr xb*) (Pred pred clab jlab)])
           (let-values ([(expr eb*) (Effect* x* `(,expr))])
             (values expr
               `(,eb* ...
                 ,xb* ...
                 [,clab (lambda () ,conseq)]
                 [,jlab (lambda () ,(make-begin rest*))]
                 ,cb* ...)))))]
     ; (if (not x) y)
      [(if ,pred (nop) ,altern)
       (let ([alab (unique-label 'a)] [jlab (unique-label 'j)])
         (let-values ([(altern ab*) (Effect '() altern `((,jlab)))]
                      [(expr xb*) (Pred pred jlab alab)])
           (let-values ([(expr eb*) (Effect* x* `(,expr))])
             (values expr
               `(,eb* ...
                 ,xb* ...
                 [,alab (lambda () ,altern)]
                 [,jlab (lambda () ,(make-begin rest*))]
                 ,ab* ...)))))]
      [(if ,pred ,conseq ,altern)
       (let ([clab (unique-label 'c)]
             [alab (unique-label 'a)]
             [jlab (unique-label 'j)])
         (let-values ([(conseq cb*) (Effect '() conseq `((,jlab)))]
                      [(altern ab*) (Effect '() altern `((,jlab)))]
                      [(pred pb*) (Pred pred clab alab)])
           (let-values ([(x xb*) (Effect* x* `(,pred))])
             (values x
               `(,xb* ...
                 ,pb* ...
                 [,clab (lambda () ,conseq)]
                 ,cb* ...
                 [,alab (lambda () ,altern)]
                 ,ab* ...
                 [,jlab (lambda () ,(make-begin rest*))])))))]
      [(begin ,effect* ...) (Effect* `(,x* ... ,effect* ...) rest*)]
      [,x (error who "invalid Effect ~s" x)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Tail -> tail* b**])] ...) ,[Tail -> tail b*])
       `(letrec ([,label* (lambda () ,tail*)] ... ,b** ... ... ,b* ...) ,tail)]
      [,x (error who "invalid Program ~s" x)])))

; alternative: collects bindings via side effects
#;(define-who expose-basic-blocks
  (lambda (x)
    (define b* '())
    (define (Tail x)
      (match x
        [(if ,pred ,[conseq] ,[altern])
         (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
           (set! b* `(,b* ...
                      [,clab (lambda () ,conseq)]
                      [,alab (lambda () ,altern)]))
           (Pred pred clab alab))]
        [(begin ,effect* ... ,[tail]) (Effect* effect* `(,tail))]
        [(,triv) `(,triv)]
        [,x (error who "invalid Tail ~s" x)]))
    (define (Pred x tlab flab)
      (match x
        [(true) `(,tlab)]
        [(false) `(,flab)]
        [(if ,pred ,[conseq] ,[altern])
         (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
           (set! b* `(,b* ...
                      [,clab (lambda () ,conseq)]
                      [,alab (lambda () ,altern)]))
           (Pred pred clab alab))]
        [(begin ,effect* ... ,[expr]) (Effect* effect* `(,expr))]
        [(,relop ,triv1 ,triv2) `(if (,relop ,triv1 ,triv2) (,tlab) (,flab))]
        [,x (error who "invalid Pred ~s" x)]))
    (define (Effect* x* rest*)
      (match x*
        [() (make-begin rest*)]
        [(,x* ... ,x) (Effect x* x rest*)]))
    (define (Effect x* x rest*)
      (match x
        [(nop) (Effect* x* rest*)]
        [(set! ,lhs ,rhs) (Effect* x* `((set! ,lhs ,rhs) ,rest* ...))]
        [(if ,pred ,conseq ,altern)
         (let ([clab (unique-label 'c)]
               [alab (unique-label 'a)]
               [jlab (unique-label 'j)])
           (let ([conseq (Effect '() conseq `((,jlab)))]
                 [altern (Effect '() altern `((,jlab)))]
                 [pred (Pred pred clab alab)])
             (set! b* `(,b* ...
                        [,clab (lambda () ,conseq)]
                        [,alab (lambda () ,altern)]
                        [,jlab (lambda () ,(make-begin rest*))]))
             (Effect* x* `(,pred))))]
        [(begin ,effect* ...) (Effect* `(,x* ... ,effect* ...) rest*)]
        [,x (error who "invalid Effect ~s" x)]))
    (match x
      [(letrec ([,label* (lambda () ,[Tail -> tail*])] ...) ,[Tail -> tail])
       `(letrec ([,label* (lambda () ,tail*)] ... ,b* ...) ,tail)]
      [,x (error who "invalid Program ~s" x)])))

; alternative: threads b*
#;(define-who expose-basic-blocks
  (define (Tail x b*)
    (match x
      [(if ,pred ,[conseq b*] ,altern)
       (let-values ([(altern b*) (Tail altern b*)])
         (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
           (Pred pred clab alab
             `(,b* ...
               [,clab (lambda () ,conseq)]
               [,alab (lambda () ,altern)]))))]
      [(begin ,effect* ... ,[tail b*]) (Effect* effect* `(,tail) b*)]
      [(,triv) (values `(,triv) b*)]
      [,x (error who "invalid Tail ~s" x)]))
  (define (Pred x tlab flab b*)
    (match x
      [(true) (values `(,tlab) b*)]
      [(false) (values `(,flab) b*)]
      [(if ,pred ,[conseq b*] ,altern)
       (let-values ([(altern b*) (Pred altern tlab flab b*)])
         (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
           (Pred pred clab alab
             `(,b* ...
               [,clab (lambda () ,conseq)]
               [,alab (lambda () ,altern)]))))]
      [(begin ,effect* ... ,[pred b*]) (Effect* effect* `(,pred) b*)]
      [(,relop ,t1 ,t2) (values `(if (,relop ,t1 ,t2) (,tlab) (,flab)) b*)]
      [,x (error who "invalid Pred ~s" x)]))
  (define (Effect* x* rest* b*)
    (match x*
      [() (values (make-begin rest*) b*)]
      [(,x* ... ,x) (Effect x* x rest* b*)]))
  (define (Effect x* x rest* b*)
    (match x
      [(nop) (Effect* x* rest* b*)]
      [(set! ,lhs ,rhs) (Effect* x* `((set! ,lhs ,rhs) ,rest* ...) b*)]
      [(if ,pred ,conseq ,altern)
       (let ([clab (unique-label 'c)]
             [alab (unique-label 'a)]
             [jlab (unique-label 'j)])
         (let*-values ([(conseq b*) (Effect '() conseq `((,jlab)) b*)]
                       [(altern b*) (Effect '() altern `((,jlab)) b*)]
                       [(pred b*) (Pred pred clab alab b*)])
           (Effect* x* `(,pred)
             `(,b* ...
               [,clab (lambda () ,conseq)]
               [,alab (lambda () ,altern)]
               [,jlab (lambda () ,(make-begin rest*))]))))]
      [(begin ,effect* ...) (Effect* `(,x* ... ,effect* ...) rest* b*)]
      [,x (error who "invalid Effect ~s" x)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
       (let-values ([(tail* b*)
                     (let f ([tail* tail*] [b* '()])
                       (if (null? tail*)
                           (values '() b*)
                           (let-values ([(tail b*) (Tail (car tail*) b*)])
                             (let-values ([(tail* b*) (f (cdr tail*) b*)])
                               (values (cons tail tail*) b*)))))])
         (let-values ([(tail b*) (Tail tail b*)])
           `(letrec ([,label* (lambda () ,tail*)] ... ,b* ...) ,tail)))]
      [,x (error who "invalid Program ~s" x)])))

(define-who flatten-program
  (define Effect
    (lambda (ef)
      (match ef
        [(set! ,var ,rhs) `((set! ,var ,rhs))]
        [,ef (error who "invalid Effect ~s" ef)])))
  (define Tail
    (lambda (tail next-label)
      (match tail
        [(,t) (if (eq? t next-label) '() `((jump ,t)))]
        [(if ,test (,tlab) (,flab))
         (cond
           [(eq? flab next-label) `((if ,test (jump ,tlab)))]
           [(eq? tlab next-label) `((if (not ,test) (jump ,flab)))]
           [else `((if ,test (jump ,tlab)) (jump ,flab))])]
        [(begin ,[Effect -> ef-code**] ... ,[tail-code*])
         `(,ef-code** ... ... ,tail-code* ...)]
        [,tail (error who "invalid Tail ~s" tail)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
       `(code
          ,@(let f ([tail tail] [label* label*] [tail* tail*])
              (if (null? tail*)
                  (Tail tail #f)
                  `(,(Tail tail (car label*)) ...
                    ,(car label*)
                    ,(f (car tail*) (cdr label*) (cdr tail*)) ...))))]
      [,x (error who "invalid Program ~s" x)])))

(define-who generate-x86-64
  (define prim->opcode
    (lambda (prim)
      (cdr (assq prim
             '((+ . addq) (- . subq) (* . imulq)
               (logand . andq) (logor . orq) (sra . sarq))))))
  (define relop->opcode
    (lambda (relop not?)
      (cdr (assq relop (if not?
                            '((= . jne) (< . jge) (<= . jg) (> . jle) (>= . jl))
                            '((= . je) (< . jl) (<= . jle) (> . jg) (>= . jge)))))))
  (define Code
    (lambda (ef)
      (match ef
        [,lab (guard (label? lab)) (emit-label lab)]
        [(jump ,rand) (emit-jump 'jmp rand)]
        [(set! ,rand1 ,lab)
         (guard (label? lab))
         (emit 'leaq lab rand1)]
        [(set! ,rand1 (,prim ,rand1 ,rand2))
         (emit (prim->opcode prim) rand2 rand1)]
        [(set! ,rand1 ,rand2) (emit 'movq rand2 rand1)]
        [(if (not (,relop ,rand1 ,rand2)) (jump ,lab))
         (emit 'cmpq rand2 rand1)
         (emit-jump (relop->opcode relop #t) lab)]
        [(if (,relop ,rand1 ,rand2) (jump ,lab))
         (emit 'cmpq rand2 rand1)
         (emit-jump (relop->opcode relop #f) lab)]
        [,ef (error who "invalid Code syntax ~s" ef)])))
  (lambda (x)
    (match x
      [(code ,code* ...) (emit-program (for-each Code code*))]
      [,x (error who "invalid Program syntax ~s" x)])))

)
