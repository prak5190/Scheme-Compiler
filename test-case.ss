;; Trying to hit the unique number and getting a clash
(let ((f.1000 1))
  (letrec((a.1000 1))
    (let ((b 2))
      (+ a.1000 b))))
