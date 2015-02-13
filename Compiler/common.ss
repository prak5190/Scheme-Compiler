(library (Compiler common)
  (export
   get-conflict
   is-int64?
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers))

  
  ;; A variable is a either a register or a frame variable 
  (define (var? exp)                   ;get-trace-define
                (or (register? exp) (frame-var? exp) (uvar? exp)))
  
  ;; Matches only 64 bit and not 32 bit
  (define (is-int64? exp)
    (and (int64? exp) (not (int32? exp))))
  
  (define-who (get-conflict program list cgvar?)        
    ;; An exp is divided into Program, Body,Tail, Effect, Var, Triv
    ;; Writing a function for each part                         
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

    (define (add-conflict-others ls v cg)      
      (cond
       ((null? ls) cg)
       ((uvar? (car ls)) (let* ((x (cdr (assq (car ls) cg)))
                                (b (unbox x)))
                           (set-box! x (union b `(,v)))
                           (add-conflict-others (cdr ls) v cg)))
       (else (add-conflict-others (cdr ls) v cg))))
                                   
    (define (add-conflict v ls cg)
      (if (uvar? v)
          (let* ((x (cdr (assq v cg)))
                 (b (unbox x)))
            (add-conflict-others ls v cg)
            (set-box! x (union b ls))
            cg)
          (add-conflict-others ls v cg)))

    
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
        ((,x ,y ,z) (let* ((l (if (cgvar? y) (union `(,y) ls) ls))
                           (l (if (cgvar? z) (union `(,z) l) l)))
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
                                       (l (if (cgvar? t1) (union `(,t1) l) l))
                                       (l (if (cgvar? t2) (union `(,t2) l) l)))
                                  (values l g))]
        [(set! ,v ,t) (let* ((l (difference ls `(,v)))
                             (g (add-conflict v l cg))
                             (l (if (cgvar? t) (union `(,t) l) l)))                             
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
        ((,x ,y ...) (values (filter cgvar? (union ls (cons x y))) cg))))
    
    (define (init-cg ls)
      (map (lambda(x) `(,x . ,(box '()))) ls))
    (define (unbox-cg cg)
      (map (lambda(x) `(,(car x) . ,(unbox (cdr x)))) cg))
    (let-values
        (((ls cg) (Tail program '() (init-cg list))))
      (unbox-cg cg))))
