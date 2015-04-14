(letrec ((a.1 (lambda() '1)))
  (letrec ((a.2 (lambda() (a.1))))
    (a.2)))
