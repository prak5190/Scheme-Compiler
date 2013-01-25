(library (Compiler compile)
  (export p423-compile p423-step)
  (import 
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (Framework driver)
    (Framework wrappers)
    (Framework match)
    (Framework helpers)
    ;; Load your passes from the files you wrote them in:
    (Compiler verify-scheme)
    (Compiler uncover-register-conflict)
    (Compiler assign-registers)
    (Compiler discard-call-live)
    (Compiler verify-scheme)
    (Compiler finalize-locations)
    (Compiler expose-frame-var)
    (Compiler expose-basic-blocks)
    (Compiler flatten-program)
    (Compiler generate-x86-64))

;; Given a thunk that generates assembly code, this will compile the 
;; resulting assembly code and output it to a file named t.s
(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk 
    'replace)
  (unless (zero? (system "cc -m64 -o t t.s Framework/runtime.c"))
    (error 'assemble "assembly failed"))
  "./t")

(define-compiler (p423-compile p423-step pass->wrapper)
  (verify-scheme)
  (uncover-register-conflict)
  (assign-registers)
  (discard-call-live)
  (finalize-locations)
  (expose-frame-var)
  (expose-basic-blocks)
  (flatten-program)
  (generate-x86-64 assemble))

)
