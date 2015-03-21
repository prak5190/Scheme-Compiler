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

(define t1 '(letrec ((a$1 (lambda(a.4 b.2) (+ a.4 b.2))))
              (let ((a.5 1) (b.3 2))
                (begin
                  (mset! a.5 0 (alloc 10))
                  (let ((k.7 (let((k.11 (let ((a.12 11)) 11))) k.11)))
                    (mset! a.5 0 (+ (+ k.7 2) (+ (a$1 1 2) 3))))
                  5))))
(pretty-print
 (uncover-locals
  (verify-scheme t1)))
