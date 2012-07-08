(library (GrammarCompiler scheme emit-scheme)
         (export emit-scheme)
         (import (chezscheme))

(define emit-scheme
  (lambda (x)
    (scheme-boilerplate)
    (newline)
    (pretty-print x)))

(define scheme-boilerplate
  (lambda ()
    (pretty-print
     '(import (Framework match)
              (Framework prims)))))

)
