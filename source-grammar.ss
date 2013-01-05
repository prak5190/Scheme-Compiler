;; P423
;; Week 1 grammars
;;
;; Passes:
;; * verify-scheme
;; * generate-x86-64

(p423-grammars
  (l01-verify-scheme
    (start Prog)
    (Prog
      (begin Statement * Statement))
    (Statement
      (set! Var Integer)
      (set! Var Var)
      (set! Var (Binop Var Integer))
      (set! Var (Binop Var Var)))
    (Var Reg)))
