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
    (Compiler uncover-frame-conflict)
    (Compiler optimize-jumps)
    (Compiler select-instructions)
    (Compiler introduce-allocation-forms)
    (Compiler uncover-register-conflict)
    (Compiler specify-representation)
    (Compiler normalize-context)
    (Compiler verify-scheme)
    (Compiler uncover-locals)
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
(define n '(letrec ()
  (let ([x.1 '1] [y.2 '2] [a.3 '3] [b.4 '4] [p.5 (cons '#f '#t)])
    (begin
      (* (begin (set-car! p.5 '#t) x.1) y.2)
      (if (if (car p.5) (if (< x.1 y.2) '#f '#t) '17)
          (if (= a.3 b.4) '#f '#t)
          (<= y.2 x.1))))))
#;(pretty-print
 (normalize-context
 (lift-letrec n)))


(define o1 '(letrec ([c$9 (lambda () (c$5))]
         [a$10 (lambda () (if (= rax 40) (c$7) (a$8)))]
         [c$7 (lambda () (c$5))]
         [a$8 (lambda () (if (= rax 32) (c$5) (a$6)))]
         [c$5 (lambda () (begin (set! rax (+ rax rax)) (r15)))]
         [a$6 (lambda () (begin (set! rax (* rax 2)) (r15)))])
  (begin (set! rax 40) (if (= rax 48) (c$5) (a$10)))))

(pretty-print
 (optimize-jumps o1))


