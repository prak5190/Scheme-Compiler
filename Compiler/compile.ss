(library (Compiler compile)
  (export p423-compile p423-compile-passes)
  (import
    (chezscheme)
    (Framework driver)
    (Framework wrappers)
    (Framework match)
    (Framework helpers)
    (Compiler verify-scheme)
    (Compiler expose-frame-var)
    (Compiler flatten-program)
    (Compiler generate-x86-64))

(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk 
    'replace)
  (unless (zero? (system "cc -m64 -o t t.s Framework/runtime.c"))
    (error 'assemble "assembly failed"))
  "./t")

(define-compiler (p423-compile p423-compile-passes pass->wrapper)
  (verify-scheme)
  (expose-frame-var)
  (flatten-program)
  (generate-x86-64 assemble))

)
