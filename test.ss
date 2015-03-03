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
    (Compiler flatten-set!)
    (Compiler impose-calling-conventions)
    (Compiler verify-scheme)
    (Compiler uncover-frame-conflict)
    (Compiler select-instructions)
    (Compiler introduce-allocation-forms)
    (Compiler uncover-register-conflict)
    (Compiler discard-call-live)
    (Compiler assign-registers)
    (Compiler finalize-locations)
    (Compiler expose-frame-var)
    (Compiler expose-basic-blocks)
    (Compiler flatten-program)
    (Compiler generate-x86-64))
(define t '(letrec ([f$0 (lambda (x.1) (locals () (+ x.1 1)))]
             [g$1 (lambda (y.2) (locals () (f$0 (f$0 y.2))))])
      (locals () (+ (f$0 1) (g$1 1)))))
(define t1 '(letrec ([f$1 (lambda (n.2 a.3 b.4 c.5 x.6)
                    (locals ()
                      (begin
                        (f$1 1 2 3 4 5)
                      (if (= n.2 0)
                          (+ (* a.3 (* x.6 x.6)) (+ (* b.4 x.6) c.5))
                          (+ (f$1 (sra n.2 3)
                                  (+ a.3 (logand n.2 4))
                                  (+ b.4 (logand n.2 2))
                                  (+ c.5 (logand n.2 1))
                                  x.6)
                             1))
                      )))])
      (locals () (f$1 16434824 1 0 -1 7))))
(pretty-print
(impose-calling-conventions    
(flatten-set!
 (remove-complex-opera*
  (verify-scheme t1)))))
