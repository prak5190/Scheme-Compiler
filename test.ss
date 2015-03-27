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
    (Compiler select-instructions)
    (Compiler introduce-allocation-forms)
    (Compiler uncover-register-conflict)
    (Compiler verify-scheme)
    (Compiler uncover-locals)
    (Compiler remove-let)
    (Compiler discard-call-live)
    (Compiler assign-registers)
    (Compiler finalize-locations)
    (Compiler expose-frame-var)
    (Compiler expose-basic-blocks)
    (Compiler flatten-program)
    (Compiler generate-x86-64))

(define t1 `(letrec () (let ((a.1 '1))
                         (if (eq? '1 '1)
                             (cons '1 '2)
                             '#f))))
(pretty-print
 (specify-representation
  (verify-scheme t1)))
