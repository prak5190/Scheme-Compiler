(letrec
    ([a$0 (lambda (x.1) (locals ()
                                (begin                                   
                                  (+ (+ 2 3) x.1))))])
  (locals (x.1)
          (begin            
            (set! x.1 (+ 1 (a$0 (begin
                                  (set! x.1 3) (+ x.1 1)))))
            x.1)))
