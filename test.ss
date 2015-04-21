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
    (Compiler uncover-assigned)
    (Compiler specify-representation)
    (Compiler convert-complex-datum) 
    (Compiler optimize-known-call)
    (Compiler remove-anonymous-lambda)
    (Compiler sanitize-binding-forms)              
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




#;(pretty-print
 (sanitize-binding-forms
 (remove-anonymous-lambda
  (optimize-direct-call
(verify-scheme t1)))))

(define t1 '(let ((f.2 (lambda() '(2 3))))
              (let ([f.1 (lambda () '(1 2))]
                    [x.4 '1])
                (begin                  
                  (eq? (eq? (f.1) (f.1)) '#(32 (33 33) 34))
                  (set! x.4 (+ x.4 x.4))))))

(pretty-print
 (uncover-assigned
 (convert-complex-datum t1)))








