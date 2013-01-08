
(library (Compiler compile)
  (export p423-compile)
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
    (Compiler generate-x86-64))

;; Given a thunk that generates assembly code, this will compile the 
;; resulting assembly code and output it to a file named t.s
(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk 
    'replace)
  (unless (zero? (system "cc -m64 -o t t.s Framework/runtime.c"))
    (error 'assemble "assembly failed"))
  ;; By convention, return the command which will run the code:
  "./t")

;; Defines the compiler
(define (p423-compile input)
  (assemble 
   (lambda ()
     (generate-x86-64
      (verify-scheme input)))))

) ;; End library
