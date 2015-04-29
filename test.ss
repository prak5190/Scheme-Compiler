(import 
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (Framework driver)
    (Framework wrappers)
    (Framework match)
    (Framework helpers)
    ;; Load your passes from the files you wrote them in:
    (Compiler parse-scheme)
    (Compiler remove-complex-opera*)
    (Compiler optimize-direct-call)
    (Compiler expose-allocation-pointer)
    (Compiler uncover-assigned)
    (Compiler optimize-source)
    (Compiler convert-assignments)
    (Compiler specify-representation)
    (Compiler convert-complex-datum) 
    (Compiler optimize-known-call)
    (Compiler remove-anonymous-lambda)
    (Compiler purify-letrec)
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




(define t1 '(if '#t
                (let ((k.3 '()))
                  (let ((k.1 (+ '1 (+ '2 (+ '3 '4)))) (k.2 k.3))
                    (+ k.1 '2)))
                '20))
(define t2 '(let ((k.1 '()))
              (if (null? k.1)
                  '2
                  '1)))

(pretty-print
 (optimize-source t2))








