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
    
    (define (get-conflict-fvs ls s)
      (map frame-var->index
           (filter frame-var? (map (lambda(x)
                                     (if (uvar? x)
                                         (let ((k (assq x s)))
                                           (if k
                                               (car (cdr k))
                                               k))
                                         x)) ls))))
    
    ;; Gets a sorted list
    (define (find-free-ind i ls)
      (cond
       ((null? ls) i)
       ((< i (car ls)) i)
       ((eqv? i (car ls)) (find-free-ind (add1 i) ls))
       (else (find-free-ind i (cdr ls)))))
    
    (define (assign x cg s start)
      (let ((c (assq x cg)))
        (if c
            (let* ((conflict-fv-ls (get-conflict-fvs (cdr c) s))
                   (sort-ls (sort < conflict-fv-ls))
                   (ind (find-free-ind start sort-ls)))
              (cons `(,x ,(index->frame-var ind)) s))
            (cons `(,x ,(index->frame-var start)) s))))
    
    (define (assign* ls cg s start)
      (cond
       ((null? ls) s)
       (else (assign* (cdr ls) cg (assign (car ls) cg s start) start))))

    (define (replace-from-locate x loc)
      (if (uvar? x)
          (let ((r (assq x loc)))
            (if r
                (cadr r)
                x))
          x))

    
    (define (Effect exp ind)
      (match exp       
        ((if ,x ,y ,z) (let ((x (Pred x ind))
                             (y (Effect y ind))
                             (z (Effect z ind)))
                         `(if ,x ,y ,z)))
        ((begin ,x ...) (let*((x (map (lambda(x) (Effect x ind)) x)))
                             `(begin ,x ...)))
        ;; Verify value of nb
        ((return-point ,x ,y) (let* ((nb (+ (* word-shift 8) (* ind 8)))
                                     (fp frame-pointer-register))
                                     `(begin
                                        (set! ,fp (+ ,fp ,nb))
                                        (return-point ,x ,y)
                                        (set! ,fp (- ,fp ,nb)))))                                        
        (,else exp)))
    
    (define (Pred exp ind)
      (match exp
        ((if ,x ,y ,z) (let ((x (Pred x ind))
                             (y (Pred y ind))
                             (z (Pred z ind)))
                         `(if ,x ,y ,z)))
        ((begin ,x ... ,y) (let*((x (map (lambda(x) (Effect x ind)) x))
                                 (y (Pred y ind)))
                             `(begin ,x ... ,y)))
        (,else exp)))
    
    (define (Tail exp ind)
      (match exp
        ((if ,x ,y ,z) (let ((x (Pred x ind))
                             (y (Tail y ind))
                             (z (Tail z ind)))
                         `(if ,x ,y ,z)))
        ((begin ,x ... ,y) (let*((x (map (lambda(x) (Effect x ind)) x))
                                 (y (Tail y ind)))
                             `(begin ,x ... ,y)))
        (,x exp )))
    
    (define (Body exp)
      (match exp
        ((locals ,x
                 (new-frames ,new-frames
                             (locate ,locate
                                     (frame-conflict ,fgraph
                                                     (call-live ,call-live ,y)))))
         (let* ((call-live-max-ind (fold-left (lambda(s x)
                                                (let ((fvind (frame-var->index (replace-from-locate x locate))))
                                                  (if (< s (if fvind fvind 0))
                                                      fvind
                                                      s))) 0 call-live))
                (frame-size (add1 call-live-max-ind))
                (loc (fold-left (lambda(s nf)
                                  (append s (assign* nf fgraph '() frame-size)))
                                '() new-frames)))
           `(locals ,(difference x new-frames)
                    (locate ,(append locate loc)
                            (frame-conflict ,fgraph
                                            ,(Tail y frame-size))))))))

    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x (lambda () ,(Body tail))))))

    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))

    (define (assign-new-frame exp)                   ;get-trace-define
      (Program exp))
    
    (assign-new-frame program)))
