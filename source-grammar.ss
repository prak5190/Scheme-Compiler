;; P423
;; Week 2 grammars
;;
;; Passes:
;;   verify-scheme    l-01 -> l-01
;; * expose-frame-var l-01 -> l-36
;; * flatten-program  l-36 -> l-40
;;   generate-x86-64  l-40 -> ()

(p423-grammars
  (l01-verify-scheme
    (start Prog)
    (Prog
      (letrec ((Label (lambda () Tail)) *) Tail))
    (Tail
      (Triv)
      (begin Effect * Tail))
    (Effect
      (set! Var Triv)
      (set! Var (Binop Triv Triv)))
    (Triv
      Var
      Integer
      Label)
    (Var
      Reg
      FVar))

(l36-expose-frame-var
  (%rename
    (FVar -> Disp)))

(l40-flatten-program
  (%remove
    Prog
    Tail)
  (%rename
    (Effect -> Statement))
  (%add
    (Prog
      (code Statement * Statement))
    (Statement
      (jump Triv)
      Label))))

