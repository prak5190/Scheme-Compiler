(library (Compiler compile)
  (export p423-compile p423-compile-passes)
  (import
    (chezscheme)
    (framework driver)
    (framework wrappers)
    (framework match)
    (framework helpers)
    (compiler verify-scheme)
    (compiler finalize-locations)
    (compiler expose-frame-var)
    (compiler expose-basic-blocks)
    (compiler flatten-program)
    (compiler generate-x86-64))

(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk 
    'replace)
  (unless (zero? (system "cc -m64 -o t t.s Framework/runtime.c"))
    (error 'assemble "assembly failed"))
  "./t")

(define-compiler (p423-compile p423-compile-passes pass->wrapper)
  (verify-scheme)
  (finalize-locations)
  (expose-frame-var)
  (expose-basic-blocks)
  (flatten-program)
  (generate-x86-64 assemble))

)
