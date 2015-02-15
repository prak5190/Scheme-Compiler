(library (Compiler assign-frame)
  (export
   assign-frame
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
    
  (define-who (assign-frame program)
    (define (get-conflict-fvs ls s)
      (map frame-var->index
           (filter frame-var? (map (lambda(x)
                                     (let ((k (assq x s)))
                                       (if k
                                           (car (cdr k))
                                           k))) ls))))
    
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
                             
    ;; Gets a sorted list by degree
    (define (assign* ls cg s)
      (cond
       ((null? ls) s)
       (else (assign* (cdr ls) cg (assign (car ls) cg s)))))
    ;;    (index->frame-var n)
    (define (Body exp)
      (match exp
        ((locals (,x ...) (ulocals ,ul (spills ,sp (locate ,z (frame-conflict ,cg ,y)))))
         (let* ((cg (sort (lambda(x y) (< (length x) (length y))) cg))
                (frame-loc (filter (lambda(x) (frame-var? (cadr x))) z))
                (ar (assign* sp cg frame-loc)))
           `(locals (,x ...) (ulocals ,ul (locate ,(append ar z) (frame-conflict ,cg ,y))))))
        ((locate (,x ...) ,y) exp)))


    ;; Validate letrec label exp :   [label (lambda() Tail)]
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x (lambda () ,(Body tail))))))

    ;; Validate Program
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))
    (Program program)))
