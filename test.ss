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
    (Compiler finalize-locations)
    (Compiler expose-frame-var)
    (Compiler expose-basic-blocks)
    (Compiler flatten-program)
    (Compiler generate-x86-64))
(define t27 '(letrec ([if-test$1 (lambda ()
                       (locate ()
                               (begin
                                 (if (begin (set! rax 5) (= rax 5))
                                     (set! rax (+ rax 10))
                                     (set! rax (- rax 10)))
                                 (set! rax (* rax 10))
                                 (r15))))])
   (locate () (if-test$1))))
(define t1 '(letrec ([fib$0 (lambda ()
                   (locate ([n.1 rax] [a.2 rbx] [b.3 rcx])
                           (begin
                             (set! a.2 0)
                             (set! b.3 1)
                             (fib$1))))]
          [fib$1 (lambda ()
                   (locate ([n.1 rax] [a.2 rbx] [b.3 rcx] [t.4 fv1]
                            [return.5 rax])
                           (if (= n.1 0)
                               (begin
                                 (set! return.5 a.2)
                                 (fv0))
                               (begin
                                 (set! n.1 (- n.1 1))
                                 (set! t.4 a.2)
                                 (set! a.2 b.3)
                                 (set! b.3 (+ b.3 t.4))
                                 (fib$1)))
                           ))])
   (locate ([n.1 rax])
           (begin
             (set! fv0 r15)
             (set! n.1 5)
             (fib$0)))))
(define asd '(letrec ([f$1 (lambda ()
                 (locate ([x.1 r8] [y.2 r9])
                         (if (if (= x.1 1) (true) (> y.2 1000))
                             (begin (set! rax y.2) (r15))
                             (begin
                               (set! y.2 (* y.2 2))
                               (set! rax x.1)
                               (set! rax (logand rax 1))
                               (if (= rax 0) (set! y.2 (+ y.2 1)) (set! rax 11))
                               (set! x.1 (sra x.1 1))
                               (f$1)))))]
          [g$2 (lambda () (locate () (f$1)))]
          [h$3 (lambda () (locate () (g$2)))]
          [s$4 (lambda () (locate () (t$5)))]
          [t$5 (lambda () (locate () (s$4)))])
   (locate ()
           (begin
             (set! r8 3)
             (set! r9 10)
             (f$1)))))

(define a1 '(letrec ()
              (locate ()
                      (begin
                        (if (< rax 10) (set! rax 1) (nop))
                        (r15)))))

(define t29 '(letrec ([if-test$3 (lambda ()
                       (locate ()
                               (begin
                                 (set! rax 2)
                                 (if (if (= rax 0)
                                         (true)
                                         (if (= rax 1)
                                             (true)
                                             (= rax 2)))
                                     (begin
                                       (set! rax (* rax 5))
                                       (r15))
                                     (begin
                                       (set! rax (- rax 5))
                                       (r15))))))])
   (locate () (if-test$3))))
(define t32 '(letrec ([if-test$6 (lambda ()
                       (locate ([n.1 rdi] [x.2 rax] [y.3 rbx])
                               (begin
                                 (set! x.2 1)
                                 (begin
                                   (set! y.3 1)
                                   (if (= n.1 0)
                                       (set! x.2 (+ x.2 y.3))
                                       (set! y.3 (+ y.3 x.2)))
                                   (set! x.2 n.1))
                                 (if (if (= n.1 y.3) (false) (true))
                                     (set! n.1 (+ n.1 x.2))
                                     (set! n.1 (+ n.1 y.3)))
                                 (set! x.2 n.1)
                                 (r15))))])
   (locate ([n.1 rdi])
           (begin
             (set! n.1 1)
             (if-test$6)))))
(define t15 '(letrec ()
   (locate ()
           (begin
             (set! rax 5)
             (if (< rax 10) (set! rax (* rax 10)) (nop))
             (r15)))))
(pretty-print
 (flatten-program
 (expose-basic-blocks
 (expose-frame-var
                                     (finalize-locations(verify-scheme t29))
                                     ))))


