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
    ;; (Compiler impose-calling-conventions)
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
(define t '(letrec ([div$0 (lambda (x.1)
                             (locals ()
                                     (begin
                                       (set! x.1 (sra x.1 1))
                                       (div$1 x.1))))]
                    [div$1 (lambda (result.1)
                             (locals () result.1))]
                    [div$2 (lambda ()
                             (locals () 1))])
             (locals (label-temp.1)
                     (begin
                       (set! label-temp.1 div$0)
                       (label-temp.1 64)))))

(pretty-print
;(flatten-set!
 (remove-complex-opera* t))
