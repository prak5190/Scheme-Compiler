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
    (Compiler optimize-direct-call)
    (Compiler expose-allocation-pointer)
    (Compiler specify-representation)
    (Compiler optimize-known-call)
    (Compiler remove-anonymous-lambda)
    (Compiler sanitize-binding-forms)              
    (Compiler flatten-set!)
    (Compiler impose-calling-conventions)
    (Compiler expose-memory-operands)
    (Compiler verify-uil)
    (Compiler uncover-free)
    (Compiler convert-closures)
    (Compiler uncover-frame-conflict)
    (Compiler optimize-jumps)
    (Compiler select-instructions)
    (Compiler introduce-allocation-forms)
    (Compiler uncover-register-conflict)
    (Compiler specify-representation)
    (Compiler normalize-context)
    (Compiler verify-scheme)
    (Compiler uncover-locals)
    (Compiler introduce-procedure-primitives)
    (Compiler remove-let)
    (Compiler discard-call-live)
    (Compiler assign-registers)
    (Compiler finalize-locations)
    (Compiler expose-frame-var)
    (Compiler expose-basic-blocks)
    (Compiler flatten-program)
    (Compiler lift-letrec)
    (Compiler generate-x86-64))



(define t1 '(letrec ((a.1 (lambda() '1)))
              (letrec ((a.2 (lambda() (a.1))))
                (let ((k.4 '11))
                  (begin
                    (lambda(x.3) (begin
                                   (let ([v.4099 (make-vector '3)])
                                     (begin
                                       (vector-set! v.4099 '0 '1)
                                       (vector-set! v.4099 '1 '2)
                                       (vector-set! v.4099 '2 '3)
                                       v.4099))
                                   (+ x.3 '4))
                           )
                    (let ([x.41 '0] [f.21 (lambda (x.11) x.11)] [y.32 '1])
                      (+ x.41 (f.21 y.32)))
                    (+ (a.2) ((lambda(x.31) (begin                                 
                                              (+ x.31 '4))
                                     ) '4)))))))
(define o2 '(cons
  (letrec ([f$4090 (lambda (cp.4101 h.4089 v.4088)
                     (bind-free (cp.4101) (* h.4089 v.4088)))])
    (closures ([f.4090 f$4090])
      (letrec ([k$4092 (lambda (cp.4100 x.4091)
                         (bind-free (cp.4100) (+ x.4091 '5)))])
        (closures ([k.4092 k$4092])
          (let ([x.4098 (void)])
            (let ([x.4093 (cons x.4098 (void))])
              (begin
                (let ([x.4097 '15]) (set-car! x.4093 x.4097))
                (letrec ([g$4094 (lambda (cp.4099 x.4095)
                                   (bind-free (cp.4099) (+ '1 x.4095)))])
                  (closures ([g.4094 g$4094])
                    (k.4092
                      k.4092
                      (g.4094
                        g.4094
                        (let ([g.4096 '3])
                          (f.4090 f.4090 g.4096 (car x.4093))))))))))))))
  '()))
(define o1 '(letrec ([map$1 (lambda (cp.13 f.3 ls.2)
                  (bind-free (cp.13 map.1)
                    (if (null? ls.2)
                        '()
                        (cons
                          (f.3 f.3 (car ls.2))
                          (map$1 map.1 f.3 (cdr ls.2))))))])
  (closures ([map.1 map$1 map.1])
    (letrec ([mulx$6 (lambda (cp.12 x.4)
                       (bind-free (cp.12)
                         (letrec ([anon$9 (lambda (cp.11 y.5)
                                            (bind-free (cp.11 x.4)
                                              (* x.4 y.5)))])
                           (closures ([anon.9 anon$9 x.4]) anon.9))))])
      (closures ([mulx.6 mulx$6])
        (map$1 map.1
          (mulx$6 mulx.6 '7)
          (map$1 map.1
            (letrec ([anon$8 (lambda (cp.10 z.7)
                               (bind-free (cp.10) (+ z.7 '1)))])
              (closures ([anon.8 anon$8]) anon.8))
            (cons '1 (cons '2 '())))))))))
#;(pretty-print
 (sanitize-binding-forms
 (remove-anonymous-lambda
  (optimize-direct-call
(verify-scheme t1)))))
(define s1 '(letrec ()
              (locals
               (tm.1111 bounded-memoize.3592 assq.3572 t.1132 t.1133 t.1131)
               (begin                 
                 (if (true)
                     (begin
                       (set! t.1117
                             (begin                               
                               (set! tm.1104 t.1118)
                               tm.1106))
                       (mset! t.1116 -1 t.1117))
                     1)
                 ans.3579))))
(pretty-print
 (optimize-known-call o2))





