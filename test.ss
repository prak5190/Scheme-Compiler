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
    (Compiler discard-call-live)
    (Compiler assign-registers)
    (Compiler finalize-locations)
    (Compiler expose-frame-var)
    (Compiler expose-basic-blocks)
    (Compiler flatten-program)
    (Compiler generate-x86-64))

(define t1 '(letrec ([get$0 (lambda (x.1 ls.2)
                              (locals (size.4 ls.3)
                                      (begin
                                        (set! size.4 (mref ls.2 0))
                                        (if (> x.1 size.4)
                                            -1
                                            (mref ls.2 (* 8 x.1))))))]
                     [a$2 (lambda() (locals() (alloc 10)))]
                     [a$3 (lambda() (locals() (mref 10 10)))])
              (locals (ls.1 x.2)
                      (begin
                        (set! ls.1 (alloc 48))
                        (set! x.2 (alloc (begin (set! x.2 10) x.2)))
                        (mset! (get$0 4 (alloc (get$0 4 ls.1))) 1 1)
                        (mset! ls.1 0 5)
                        (get$0 4 (alloc (get$0 4 ls.1)))
                        (mset! ls.1 8 9)
                        (mset! ls.1 16 (begin (set! x.2 1) (mset! ls.1 0 (begin (set! x.2 1)  x.2)) x.2))
                        (mset! ls.1 24 7)
                        (mset! ls.1 32 8)
                        (mset! ls.1 40 3)
                        (get$0 4 ls.1)))))


(pretty-print
(expose-allocation-pointer 
(impose-calling-conventions    
(flatten-set!
 (remove-complex-opera*
  (verify-uil t1))))))
