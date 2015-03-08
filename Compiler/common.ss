(library (Compiler common)
  (export
   get-conflict
   is-int64?
   triv?
   binop?
   relop?
   get-unique-name
   get-unique-name-p
   get-unique-label-p
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers))

  (define (binop? exp)                   ;get-trace-define
    (define binops '(+ - * logand logor sra))
    (and (memq exp binops) #t))
  
  ;; If it is a relational operator or not
  (define (relop? exp)                   ;get-trace-define
    (define relops '(< > = <= >=))
    (and (memq exp relops) #t))
  ;; A variable is a either a register or a frame variable 
  (define (var? exp)                   ;get-trace-define
                (or (register? exp) (frame-var? exp) (uvar? exp)))
  
  ;; Matches only 64 bit and not 32 bit
  (define (is-int64? exp)
    (or (and (int64? exp) (not (int32? exp))) (label? exp)))

  (define (triv? exp)
    (or (uvar? exp) (int64? exp) (label? exp)))

  
  (define (labelLs->suffx ls)
    (map (lambda(x) (string->number (extract-suffix x))) ls))

  ;; Gets a unique unspillable

  (define (get-unique-name-p ls p)
    (get-unique-name-main ls p #f))
  
  (define (get-unique-label-p ls p)
    (get-unique-name-main ls p #t))
  
  (define (get-unique-name ls)
    (get-unique-name-p ls 't))

  ;; Fix -- Super badddd
  (define (get-unique-name-main ls p isLabel)
    (let ((n (if isLabel (unique-label p) (unique-name p))))      
      (if (memq (string->number (extract-suffix n)) (labelLs->suffx ls))
          (begin
            (get-unique-name-main ls p isLabel))              
          n)))  
  
  (define-who (get-conflict program list cgvar?)        
    ;; An exp is divided into Program, Body,Tail, Effect, Var, Triv
    ;; Writing a function for each part                         
    ;; Combine two conflict graph
     (define (s-cdr ls)
      (cond
       ((or (null? ls) (not ls)) (error who "Null cdr in frame" ls))
       (else (cdr ls))))
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
       ((uvar? (car ls)) (let* ((xc (assq (car ls) cg)))
                           (if xc
                               (set-box! (cdr xc) (union (unbox (cdr xc)) `(,v))))
                           (add-conflict-others (cdr ls) v cg)))
       (else (add-conflict-others (cdr ls) v cg))))

    (define (add-conflict v ls cg)
      (if (uvar? v)
          (let* ((ax (assq v cg))
                 (x (if ax (cdr ax) (box '())))
                 (b (unbox x)))
            (add-conflict-others ls v cg)
            (set-box! x (union b ls))
            cg)
          (if (cgvar? v)
              (add-conflict-others ls v cg)
              ;; Don't add - just return the same cg
              cg)))

    
    ;; Validate Pred
    (define (Pred exp ls cg s)
      (match exp
        ((true) (values ls cg s))
        ((false) (values ls cg s))
        ((if ,x ,y ,z) (let*-values (((l1 g1 s) (Pred z ls cg s))
                                     ((l2 g2 s) (Pred y ls cg s)))
                         (Pred x (union l1 l2) (combine-cg g1 g2 '()) s)))
        ((begin ,x ... ,p) (let-values (((l g s) (Pred p ls cg s)))
                             (Effect* x l g s)))                                      
        ((,x ,y ,z) (let* ((l (if (cgvar? y) (union `(,y) ls) ls))
                           (l (if (cgvar? z) (union `(,z) l) l)))
                        (values l cg s)))))

    (define (Effect exp ls cg s)                   ;get-trace-define
      (match exp
        [(if ,x ,y ,z) (let*-values (((l1 g1 s) (Effect z ls cg s))
                                     ((l2 g2 s) (Effect y ls cg s)))
                         (Pred x (union l1 l2) (combine-cg g1 g2 '()) s))]
        [(begin ,x ...) (Effect* x ls cg s)]
        [(set! ,v (,b ,t1 ,t2)) 
         (let* ((l (difference ls `(,v)))
                (g (add-conflict v l cg))
                (l (if (cgvar? t1) (union `(,t1) l) l))
                (l (if (cgvar? t2) (union `(,t2) l) l)))
           (values l g s))]
        [(set! ,v ,t) (let* ((l (difference ls `(,v)))
                             (g (add-conflict v l cg))
                             (l (if (cgvar? t) (union `(,t) l) l)))                             
                        (values l g s))]
        [(return-point ,lab ,t) (let*-values
                                       (((l1 cg s) (Tail t ls cg s)))
                                  (values l1 cg (union ls s)))]
        [,x (values ls cg s)]))
    
    (define (Effect* ex ls cg s)
      (match ex
        ((,x ... ,y) (let-values (((l g s) (Effect y ls cg s)))
                       (Effect* x l g s)))
        (,else (values ls cg s))))
    
    ;; Validate Tail
    (define (Tail exp ls cg s)                   ;get-trace-define
      (match exp
        ((begin ,x ... ,t) (let*-values
                               (((ls cg s) (Tail t ls cg s)))
                             (Effect* x ls cg s)))
        ((if ,x ,y ,z) (let*-values (((l1 cg1 s) (Tail y ls cg s))
                                     ((l2 cg2 s) (Tail z ls cg s)))
                         (Pred x (union l1 l2) (combine-cg cg1 cg2 '()) s)))
        ((,x ,y ...) (values (filter cgvar? (union ls (cons x y))) cg s))))
    
    (define (init-cg ls)
      (map (lambda(x) `(,x . ,(box '()))) ls))
    (define (unbox-cg cg)
      (map (lambda(x) `(,(car x) . ,(unbox (cdr x)))) cg))
    (let-values
        (((ls cg s) (Tail program '() (init-cg list) '())))
      (values (unbox-cg cg) s))))
