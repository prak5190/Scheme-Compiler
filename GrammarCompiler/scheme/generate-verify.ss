(library (GrammarCompiler scheme generate-verify)
         (export generate-verify verifier-name)
         (import (chezscheme)
                 (GrammarCompiler common match)
                 (GrammarCompiler common aux))

(define generate-verify
  (lambda (x)
    (match x
      ((,[Name -> name]
        (start ,st)
        ;(with-terminals ,[Terminal -> t/p*] ...)
        ,[Type -> t*] ...)
       `(define ,name
          (lambda (x)
            ,t* ...
            ;,t/p* ...
            (let ((res (,st x)))
              (if res (errorf ',name "~a" res) x))))))))

(define Name
  (lambda (s)
    (string->symbol
     (string-append
      "verify-grammar:"
      (symbol->string s)))))

(define verifier-name Name)

(define Terminal
  (lambda (t/p)
    (let ((term (car t/p)) (pred (cadr t/p)))
      `(define ,term
         (lambda (x)
           (if (,pred x) #f (invalid-expr ',term x)))))))

(define Type
  (lambda (x)
    (match x
      ((,name . ,sub*)
       (let-values (((s ns) (partition non-terminal? sub*)))
         (let ((ns (map Sub ns))
               (s (map Simple s)))
           `(define ,name
              (lambda (x)
		,(make-match-chain name 'x (append s ns))
                ; (match x
                ;   ,s ...
                ;   ,ns ...
                ;   (,(uq 'e) (invalid-expr ',name e)))
		))))))))
       ;(let ((sub* (map (if (for-all non-terminal? sub*) Simple Sub) sub*)))
       ;  `(define ,name
       ;     (lambda (x)
       ;       (match x
       ;         ,sub* ...
       ;         (,(uq 'e) (invalid-expr ',name e))))))))))

(define Simple
  (lambda (s)
    `(,(uq 'e) (guard (not (,s e))) #f)))

(define (make-match-chain name obj clauses)
  (match clauses
    [() `(invalid-expr ',name ,obj)]
    [(,last) `(let ((next (lambda () (invalid-expr ',name ,obj))))
		(match ,obj ,last (,(uq 'e) (next))))]
    [(,fst . ,rst) 
     `(let ((next (lambda () ,(make-match-chain name obj rst))))
	(match ,obj
	  ,fst
	  (,(uq 'e) (next))))]
    ))

;; Handle each nontrivial pattern for a nonterminal:
(define (Sub s)
  (let-values
        (((s n seen)
          (let loop ((s s) (n 1) (seen '()))
            ;; seen - accumulate nonterminals encountered
            (cond
              ((null? s) (values '() n seen))
              (else
               (let ((a (car s)) (d (cdr s)))
                 (cond
                   ((pair? a)
                    (let-values (((a n seen) (loop a n seen)))
                      (let-values (((d n seen) (loop d n seen)))
                        (values `(,a . ,d) n seen))))
                   ((terminal? a)
                    (let-values (((d n seen) (loop d n seen)))
                      (values `(,a . ,d) n seen)))
                   ((non-terminal? a)
                    (let ((name (number-symbol "x" n)))
                      (let-values (((d n seen) (loop d (add1 n) (cons name seen))))
                        (values
                         (cons (uq `(,a -> ,name)) d)
                         n
                         seen))))
                   ((eq? a '*)
                    (let-values (((d n seen) (loop d n seen)))
                      (values `(... . ,d) n seen))))))))))
      `(,s (and (any . ,seen) (next)))
      ))

(define uq
  (lambda (s)
    (list 'unquote s)))

)
