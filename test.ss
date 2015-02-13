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
    (Compiler uncover-frame-conflict)   
    (Compiler uncover-register-conflict)
    (Compiler discard-call-live)
    (Compiler assign-registers)
    (Compiler finalize-locations)
    (Compiler expose-frame-var)
    (Compiler expose-basic-blocks)
    (Compiler flatten-program)
    (Compiler generate-x86-64))

(define t4 '(letrec ([f$1 (lambda ()
                    (locals (x.1 y.2 z.3)
                      (begin
                        (set! x.1 1)
                        (set! y.2 2)
                        (set! rax (+ x.1 y.2))
                        (r15 rax rcx rdx rbx rbp rdi rsi r8 r9 r10
                          r11 r12 r13 r14))))])
      (locals () (f$1 rbp r15))))
(pretty-print
 (uncover-frame-conflict
  (verify-scheme t4)))


