(letrec ((a$1 (lambda(a.4 b.2) (+ a.4 b.2))))
              (let ((a.5 1) (b.3 2))
                (begin
                  (mset! a.5 0 (alloc 10))
                  (let ((k.7 (let((k.11 (let ((a.12 11)) 11))) k.11)))
                    (mset! a.5 0 (+ (+ k.7 2) (+ (a$1 1 2) 3))))
                  5)))
