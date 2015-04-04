(library (Compiler optimize-jumps)
  (export
   optimize-jumps
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers))   

  (define-who (optimize-jumps program)
    ;; An exp is divided into Program, Tail, Effect, Var, Triv
    ;; Writing a function for each part

    ;; Validate letrec label exp :   [label (lambda() Tail)]
    (define (Exp exp)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,y))  `(,x . ,y))))
    
    (define (substitute x al)
      (cond
       ((assq x al) => (lambda(r) (cadr r)))
       (else x)))
   
    (define (Tail exp al)                   ;get-trace-define
      (match exp
        ((if ,x ,y ,z) (let* ((x (Tail x al))
                              (y (Tail y al))
                              (z (Tail z al)))
                         `(if ,x ,y ,z)))
        ((begin ,x ...) `(begin ,(map (lambda(x) (Tail x al)) x) ...))
        ((,x ...)  `(,(map (lambda(x) (substitute x al)) x) ...))))
    
    (define (remap* al)
      (fold-left remap '() al))
    (define (remap s al)
      (cond
       ((null? al) s)
       ((assq (cadr al) s) => (lambda(r) (cons `(,(car al) . ,(cdr r)) s)))
       (else (cons al s))))

    (define (substitute-from-al* x al)
      (fold-left (lambda(s x)
                   (substitute-from-al s x al)) '() x))
    (define (substitute-from-al s x al)
      (cond
       ((null? x) s)
       ((assq (car x) al) => (lambda(r) (if (eq? (car r) (cadr r))
                                            (cons `(,(car r) (lambda() ,(cdr r))) s)
                                            s)))
       (else (cons `(,(car x) (lambda() ,(Tail (cdr x) al))) s))))
        
    ;; Validate Program
    (define (Program exp)                   ;get-trace-define
      (match exp
        ((letrec (,[Exp -> x]  ...) ,y) (let*((al (filter (lambda(x)
                                                   (match (cdr x)
                                                     ((,x) (guard label? x) #t)
                                                     (,else #f))) x))
                                              (al (remap* al))
                                              (expls (substitute-from-al* x al))
                                              (y (Tail y al)))
                                          `(letrec ,expls ,y)))))
    
    (define (optimize-jumps exp)                   ;get-trace-define
      (Program exp))
    (optimize-jumps program)))
