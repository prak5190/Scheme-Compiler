(let ((f.2 (lambda() '(2 3))))
              (let ([f.1 (lambda () '(1 2))]
                    [x.4 '1])
                (begin                  
                  (eq? (eq? (f.1) (f.1)) '#(32 (33 33) 34))
                  (set! x.4 (+ x.4 x.4)))))
