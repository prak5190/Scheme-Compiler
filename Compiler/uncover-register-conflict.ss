(library (Compiler uncover-register-conflict)
  (export
   uncover-register-conflict
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers))
  ;;;; TODO Need to add machine constraints error 
  ;; If it is a binary operator or not
  (define (binop? exp)                   ;get-trace-define
    (define binops '(+ - * logand logor sra))
    (and (memq exp binops) #t))

  (define (Register? x)
    (or (register? x) (uvar? x)))
  ;; If it is a relational operator or not
  (define (relop? exp)                   ;get-trace-define
    (define relops '(< > = <= >=))
    (and (memq exp relops) #t))
  
  ;; A variable is a either a register or a frame variable 
  (define (var? exp)                   ;get-trace-define
                (or (register? exp) (frame-var? exp) (uvar? exp)))  
  ;; extract-suffix name -> use this to enforce unique name
  ;; Using define-who macro 
  (define-who (uncover-register-conflict program)
    ;; difference intersection union
    
    ;; An exp is divided into Program, Body,Tail, Effect, Var, Triv
    ;; Writing a function for each part
    ;; Trivial is Var | int | label  -- No int? so putting int64?  
    (define (Triv exp labells uvarls)                   ;get-trace-define      
      (or (int64? exp)
          (label? exp)
          (uvar? exp)              
          (var? exp)))
                     
    ;; Combine two conflict graph
    (define (combine-cg x y ig)
      (cond
       ((null? x)
        ;; Hacky code caused due to cons in else assq part
        (if (null? y)
                      y
                      (if (memq (caar y) ig) (combine-cg x (cdr y) ig))))
       ((null? y) x)
       (else (cond
              ;; Ignore items in y present in ignore list
              ((memq (caar y) ig) (combine-cg x (cdr y) ig))
              ((assq (caar x) y) => (lambda(l)
                                      (cons
                                       (cons (car l) (box (union (unbox (cdar x))
                                                                 (unbox (cdr l)))))
                                       (combine-cg (cdr x) y (cons (caar x) ig)))))
              (else (cons (car x) (combine-cg (cdr x) y ig)))))))

    (define (add-conflict v ls cg)
      (if (uvar? v)
          (let* ((x (cdr (assq v cg)))
                 (b (unbox x)))
            (set-box! x (union b ls))
            cg)
          cg))

    
    ;; Validate Pred
    (define (Pred exp ls cg)
      (match exp
        ((true) (values ls cg))
        ((false) (values ls cg))
        ((if ,x ,y ,z) (let*-values (((l1 g1) (Pred z ls cg))
                                     ((l2 g2) (Pred y ls cg)))
                         (Pred x (union l1 l2) (combine-cg g1 g2 '()))))
        ((begin ,x ... ,p) (let-values (((l g) (Pred p ls cg)))
                             (Effect* x l g)))                                      
        ((,x ,y ,z) (let* ((l (if (Register? y) (union `(,y) ls) ls))
                           (l (if (Register? z) (union `(,z) l) l)))
                        (values l cg)))))

    (define (Effect exp ls cg)                   ;get-trace-define
      (match exp
        [(nop) (values ls cg)]
        [(if ,x ,y ,z) (let*-values (((l1 g1) (Effect z ls cg))
                                     ((l2 g2) (Effect y ls cg)))
                         (Pred x (union l1 l2) (combine-cg g1 g2 '())))]
        [(begin ,x ...) (Effect* x ls cg)]
        [(set! ,v (,b ,t1 ,t2)) (let* ((l (difference ls `(,v)))
                                       (g (add-conflict v l cg))
                                       (l (if (Register? t1) (union `(,t1) l) l))
                                       (l (if (Register? t2) (union `(,t2) l) l)))
                                  (values l g))]
        [(set! ,v ,t) (let* ((l (difference ls `(,v)))
                             (g (add-conflict v l cg))
                             (l (if (Register? t) (union `(,t) l) l)))                             
                        (values l g))]))
    
    (define (Effect* ex ls cg)
      (match ex
        ((,x ... ,y) (let-values (((l g) (Effect y ls cg)))
                       (Effect* x l g)))
        (,else (values ls cg))))
    
    ;; Validate Tail
    (define (Tail exp ls cg)                   ;get-trace-define
      (match exp
        ((begin ,x ... ,t) (let*-values
                               (((ls cg) (Tail t ls cg)))
                             (Effect* x ls cg)))
        ((if ,x ,y ,z) (let*-values (((l1 cg1) (Tail y ls cg))
                                     ((l2 cg2) (Tail z ls cg)))
                         (Pred x (union l1 l2) (combine-cg cg1 cg2 '()))))
        ((,x ,y ...) (values (filter Register? (union ls (cons x y))) cg))))

    (define (init-cg ls)
      (map (lambda(x) `(,x . ,(box '()))) ls))
    (define (unbox-cg cg)
      (map (lambda(x) `(,(car x) . ,(unbox (cdr x)))) cg))
      
    ;; Validate Body
    (define (Body exp)
      (match exp
        ((locals (,x ...) ,y) (let*-values (((ls cg) (Tail y '() (init-cg x))))
                                `(locals (,x ...)                   
                                         (register-conflict ,(unbox-cg cg) ,y))))))

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
