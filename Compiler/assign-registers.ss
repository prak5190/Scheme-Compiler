(library (Compiler assign-registers)
  (export
   assign-registers
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
  
  ;; A variable is a either a register or a frame variable 
  (define (var? exp)                   ;get-trace-define
                (or (register? exp) (frame-var? exp) (uvar? exp)))  

  ;; Using define-who macro 
  (define-who (assign-registers program)
    ;;
    (define (map-to-reg ls s)
      (cond
       ((null? s) '())
       ((memq (caar s) ls) (cons (cadar s) (map-to-reg ls (cdr s))))
       (else (map-to-reg ls (cdr s)))))
    
    ;; Gets a sorted list by degree
    (define (assign cg regls s spill)
      (cond
       ((null? cg) (values (reverse s) spill))
       ;; 
;;       ((null? regls) (values s (append (map car cg) spill)))
;        (errorf who "No registers left, Have to spill !!! ~s" cg))
       (else (let* ((k (cdar cg))
                    (cg-assigned-reg (union k (map-to-reg k s)))
                    (aregls (difference regls cg-assigned-reg)))
               (if (null? aregls)
                   (assign (cdr cg) regls
                           s (cons (caar cg) spill))
;                   (errorf who "Cannot allocate register for ~s" (car k))                   
                   (assign (cdr cg) regls
                           (cons `(,(caar cg) ,(car aregls)) s) spill))))))

    (define (sort-graph cg ul)
      (let-values (((ul-cg cg2) (partition (lambda (x) (memq (car x) ul)) cg)))
        (append
         (sort (lambda(x y) (< (length x) (length y))) ul-cg)
         (sort (lambda(x y) (< (length x) (length y))) cg2)
         )))
    
    (define (Body exp)
      (match exp
        ((locals ,x (ulocals ,ul (locate ,z (frame-conflict ,fc (register-conflict ,cg ,y)))))
         (let ((sort-cg (sort-graph cg ul)))
           (let-values (((ar spills) (assign sort-cg registers '() '())))
             (if (null? spills)
                 `(locate ,(append ar z) ,y)
                 `(locals ,(difference x spills) (ulocals ,ul (spills ,spills
                                                        (locate ,z (frame-conflict ,fc ,y)))))))))
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
