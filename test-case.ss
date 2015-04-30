;; Trying to hit the unique number and getting a clash
(let ((f.999 1))
  (letrec((a.999 1))
    (let ((b 2))
      (+ a.999 b))))
