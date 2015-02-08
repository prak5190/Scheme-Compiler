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
  
  ;; If it is a relational operator or not
  (define (relop? exp)                   ;get-trace-define
    (define relops '(< > = <= >=))
    (and (memq exp relops) #t))
  
  ;; A variable is a either a register or a frame variable 
  (define (var? exp)                   ;get-trace-define
                (or (register? exp) (frame-var? exp) (uvar? exp)))  
  ;; extract-suffix name -> use this to enforce unique name
  ;; Using define-who macro 
  (define-who (assign-registers program)

    ;; Get a sorted list by degree
    (trace-define (assign cg regls)
      (cond
       ((null? cg) '())
       ((null? regls) '());(errorf who "No registers left, Have to spill !!! ~s" cg))
       (else (let* ((k (cdar cg))
                    (aregls (difference regls (cdr k))))
               (if (null? regls)
                   (errorf who "Cannot allocate register for ~s" (car k))
                   (cons `(,(caar cg) ,(car aregls)) (assign (cdr cg) (intersection
                                                                     (cdr aregls) regls))))))))

    (define (Body exp)
      (match exp
        ((locals (,x ...) (register-conflict ,cg ,y))
         (let ((ar (assign (sort (lambda(x y) (< (length x) (length y))) cg) registers)))
           `(locate ,ar ,y)))))


    ;; Validate letrec label exp :   [label (lambda() Tail)]
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,tail)) `(,x (lambda () ,(Body tail))))))

    ;; Validate Program
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x] ...) ,y) `(letrec (,x ...) ,(Body y)))))
    (Program program)))
