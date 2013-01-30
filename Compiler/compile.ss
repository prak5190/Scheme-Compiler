(library (Compiler compile)
  (export p423-compile p423-step)
  (import
    (chezscheme)
    (Framework driver)
    (Framework wrappers)
    (Framework match)
    (Framework helpers)
    (Compiler verify-scheme)
    (Compiler uncover-register-conflict)
    (Compiler assign-registers)
    (Compiler discard-call-live)
    (Compiler finalize-locations)
    (Compiler expose-frame-var)
    (Compiler expose-basic-blocks)
    (Compiler flatten-program)
    (Compiler generate-x86-64))

(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk 
    'replace)
  (unless (zero? (system "cc -m64 -o t t.s Framework/runtime.c"))
    (error 'assemble "assembly failed"))
  "./t")

;; Compose the complete compiler as a pipeline of passes.
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
