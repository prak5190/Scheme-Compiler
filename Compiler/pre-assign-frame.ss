(library (Compiler pre-assign-frame)
  (export
   pre-assign-frame
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
    
  (define-who (pre-assign-frame program)
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
    
    (define (assign x cg s)
      (let ((c (assq x cg)))
        (if c
            (let* ((conflict-fv-ls (get-conflict-fvs (cdr c) s))
                   (sort-ls (sort < conflict-fv-ls))
                   (ind (find-free-ind 0 sort-ls)))
              (cons `(,x ,(index->frame-var ind)) s))
            (cons `(,x ,(index->frame-var 0)) s))))
    
    (define (assign* ls cg s)
      (cond
       ((null? ls) s)
       (else (assign* (cdr ls) cg (assign (car ls) cg s)))))
    
    (define (Body exp)
      (match exp
        ((locals ,x
                 (new-frames ,new-frames
                             (spills ,spills
                                     (frame-conflict ,fgraph
                                                     (call-live ,call-live ,y)))))
         (let* ((fgraph (sort (lambda(x y) (< (length x) (length y))) fgraph))                
                (locate (assign* spills fgraph '())))
           `(locals ,(difference x spills)
                    (new-frames ,new-frames
                                (locate ,locate
                                        (frame-conflict ,fgraph
                                              (call-live ,call-live ,y)))))))))
      
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x (lambda () ,(Body tail))))))

    
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))

    (define (pre-assign-frame exp)                   ;get-trace-define
      (Program exp))
    
    (pre-assign-frame program)))
