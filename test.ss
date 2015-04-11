(import 
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (Framework driver)
    (Framework wrappers)
    (Framework match)
    (Framework helpers)
    ;; Load your passes from the files you wrote them in:
    (Compiler remove-complex-opera*)
    (Compiler expose-allocation-pointer)
    (Compiler specify-representation)
    (Compiler flatten-set!)
    (Compiler impose-calling-conventions)
    (Compiler verify-uil)
    (Compiler uncover-free)
    (Compiler convert-closures)
    (Compiler uncover-frame-conflict)
    (Compiler optimize-jumps)
    (Compiler select-instructions)
    (Compiler introduce-allocation-forms)
    (Compiler uncover-register-conflict)
    (Compiler specify-representation)
    (Compiler normalize-context)
    (Compiler verify-scheme)
    (Compiler uncover-locals)
    (Compiler introduce-procedure-primitives)
    (Compiler remove-let)
    (Compiler discard-call-live)
    (Compiler assign-registers)
    (Compiler finalize-locations)
    (Compiler expose-frame-var)
    (Compiler expose-basic-blocks)
    (Compiler flatten-program)
    (Compiler lift-letrec)
    (Compiler generate-x86-64))
(define x `(letrec ([vectors?$1 (lambda (v.1 v.2)
                            (if (vector? v.1)
                                (vector? v.2)
                                '#f))])
             (let ([v.3 (make-vector '2)] [v.4 (make-vector '2)])
               (begin
                 (vector-set! v.3 '0 '10)
                 (vector-set! v.3 '1 '20)
                 (vector-set! v.4 '0 '5)
                 (vector-set! v.4 '1 '15)
                 (if (eq? (vectors?$1 v.3 v.4) '#t)
                     (+
                      (* (vector-ref v.3 '0) (vector-ref v.4 '0))
                      (* (vector-ref v.3 '1) (vector-ref v.4 '1)))
                     '100)))))
(define n '(letrec ([f.1 (lambda ()
                    (letrec ([loop.2 (lambda (link.3)
                                       (letrec ([anon.5 (lambda ()
                                                          (link.3))])
                                         anon.5))])
                      (loop.2
                        (letrec ([anon.4 (lambda () '668)]) anon.4))))])
        ((f.1))))

(define t1 '(let ((a.100 '1))
(letrec ([fact.0 (lambda (n.3 k.4)
                         (if (eq? n.3 '0)
                             (k.4 '1)
                             (fact.0 (- n.3 '1)
                                     (letrec ([anon.5 (lambda (v.6)
                                                        (k.4 (* n.3 v.6)))])
                                       anon.5))))]
               [anon.1 (lambda (v.2) (begin (fact.0 a.100 '2) v.2))])
        (fact.0 '5 anon.1))))

(pretty-print
 (introduce-procedure-primitives
 (convert-closures
 (uncover-free
  (verify-scheme t1)))))





