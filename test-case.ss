(letrec ((a.1 (lambda() '1)))
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
        (+ (a.2) ((lambda(x.31) (begin                                 
                                 (+ x.31 '4))
                         ) '4))))))
