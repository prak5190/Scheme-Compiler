(import 
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (Framework driver)
    (Framework wrappers)
    (Framework match)
    (Framework helpers)
    ;; Load your passes from the files you wrote them in:
    (Compiler parse-scheme)
    (Compiler remove-complex-opera*)
    (Compiler optimize-free)
    (Compiler optimize-direct-call)
    (Compiler uncover-well-known)
    (Compiler common)
    (Compiler expose-allocation-pointer)
    (Compiler uncover-assigned)
    (Compiler optimize-source)
    (Compiler convert-assignments)
    (Compiler specify-representation)
    (Compiler convert-complex-datum) 
    (Compiler optimize-known-call)
    (Compiler remove-anonymous-lambda)
    (Compiler purify-letrec)
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




(define t1 '(if '#t
                (let ((k.3 '()))
                  (let ((k.1 (+ '1 (+ '2 (+ '3 '4)))) (k.2 k.3))
                    (+ k.1 '2)))
                '20))
(define t2 '(let ((k.3 (cons '11 '2)) (k.2 (cons '1 '2)))
              (if (if (= k.2 k.3)
                      (> '1 '2)
                      '#f)
                  '222
                  '333)))
(define t3 '(letrec ([k$1 (lambda (cp.2) '1)])
              (let ([k.1 (make-procedure k$1 '0)]) '221)))


;; (pretty-print
;;  (parse-scheme '(let ((f.999 1))
;;   (letrec((a.999 1))
;;     (let ((b 2))
;;       (+ a.999 b)
;;       (if (> b 2)
;;           (= b 2)
;;           a.999))))))




(define s1 '(letrec ()
              (locals (rp.6 a.9 a.10)
                      (ulocals ()
                               (locate ()
                                       (frame-conflict ([rp.6 fv0])
                                                       (begin
                                                         (set! rp.6 r15)
                                                         (set! a.9 rp.6)
                                                         (set! a.10 (+ a.9 rp.6)))))))))










(define q1 '(letrec ([z$1 (lambda (cp.9 x.2)
                (bind-free (cp.9 z.1) (z$1 z.1 '1)))])
  (closures ([z.1 z$1 z.1])
    (letrec ([e$4 (lambda (cp.8 x.6)
                    (bind-free (cp.8 z.1 o.3)
                      (if (z$1 z.1 x.6) '#t (o$3 o.3 (- x.6 '1)))))]
             [o$3 (lambda (cp.7 x.5)
                    (bind-free (cp.7 z.1 e.4)
                      (if (z$1 z.1 x.5) '#f (e$4 e.4 (- x.5 '1)))))])
      (closures ([e.4 e$4 z.1 o.3] [o.3 o$3 z.1 e.4])
        (e$4 e.4 '3))))))
(define q2 '(letrec ([f$2 (lambda (cp.8) (bind-free (cp.8) '0))]
         [g$1 (lambda (cp.7) (bind-free (cp.7) '1))])
  (closures ([f.2 f$2] [g.1 g$1])
    (letrec ([h$3 (lambda (cp.6 p1.5 p2.4)
                    (bind-free (cp.6 f.2)
                      (cons (f$2 f.2)
                        (cons (p1.5 p1.5) (p2.4 p2.4)))))])
      (closures ([h.3 h$3 f.2])
        (h$3 h.3 f.2 g.1))))))



(define q3 '(let ([x.1 '77])
              (letrec ([a$5 (lambda (cp.11 p.7)
                              (bind-free (cp.11) (p.7 p.7)))]
                       [b$4 (lambda (cp.10 y.6)
                              (bind-free (cp.10 b.4 a.5 x.1)
                                         (if y.6 (b$4 b.4 (a$5 a.5 y.6)) x.1)))]
                       [c$3 (lambda (cp.9) (bind-free (cp.9) '#f))]
                       [d$2 (lambda (cp.8)
                              (bind-free (cp.8 b.4 c.3) (b$4 b.4 c.3)))])
                (closures ([a.5 a$5]
                           [b.4 b$4 b.4 a.5 x.1]
                           [c.3 c$3]
                           [d.2 d$2 b.4 c.3])
                          (d$2 d.2)))))
(pretty-print
 (optimize-free
 (uncover-well-known q3)
 ))
