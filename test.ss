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
    (Compiler optimize-direct-call)
    (Compiler expose-allocation-pointer)
    (Compiler specify-representation)
    (Compiler flatten-set!)
    (Compiler impose-calling-conventions)
    (Compiler expose-memory-operands)
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



(define t1 '(letrec ((a.1 (lambda() '1)))
  (letrec ((a.2 (lambda() (a.1))))
    (let ((k.4 '11))
      (begin
        (lambda(x.3) (begin
                               (let ([v.4099 (make-vector '3)])
                                 (begin
                                   (vector-set! v.4099 '0 '1)
                                   (vector-set! v.4099 '1 '2)
                                   (vector-set! v.4099 '2 '3)
                                   v.4099))
                               (+ x.3 '4))
                       )
        (+ (a.2) ((lambda(x.31) (begin                                 
                                 (+ x.31 '4))
                         ) '4)))))))
(pretty-print
 (optimize-direct-call
  (verify-scheme t1)))





