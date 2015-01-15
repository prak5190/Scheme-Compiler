(library (compiler compile)
  (export p423-compile)
  (import 
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (Framework driver)
    (Framework wrappers)
    (Framework match)
    (Framework helpers)
    ;; Load your passes from the file you wrote them in. 
    ;; If that file is called passes.ss, it would be
    (Compiler passes))

;; Given a thunk that generates assembly code, this will compile the 
;; resulting assembly code and output it to a file named t.s
(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk 
    'replace)
  (unless (zero? (system "cc -m64 -o t t.s Framework/runtime.c"))
    (error 'assemble "assembly failed"))
  "./t")

;; Defines the compiler
(define-compiler (p423-compile p423-compile-passes pass->wrapper)
  (verify-scheme)
  (generate-x86-64 assemble))

;; See the drivers.ss file for other options when defining a compiler

) ;; End library
