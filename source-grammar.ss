;; P423
;; Week 3 grammars
;;
;; Passes:
;;   verify-scheme       l-01 -> l-01
;; * finalize-locations  l-01 -> l-36
;;   expose-frame-var    l-36 -> l-37
;; * expose-basic-blocks l-37 -> l-39
;;   flatten-program     l-39 -> l-41
;;   generate-x86-64     l-41 -> ()

(p423-grammars
  (l01-verify-scheme
    (start Prog)
    (Prog
      (letrec ((Label (lambda () Body)) *) Body))
    (Body
      (locate ((UVar Loc) *) Tail))
    (Tail
      (if Pred Tail Tail)
      (begin Effect * Tail)
      (Triv))
    (Pred
      (true)
      (false)      
      (if Pred Pred Pred)
      (begin Effect * Pred)
      (Relop Triv Triv))
    (Effect
      (nop)
      (set! Var Triv)
      (set! Var (Binop Triv Triv))
      (if Pred Effect Effect)
      (begin Effect * Effect))
    (Triv
      Var
      Integer
      Label)
    (Var
      UVar
      Loc)
    (Loc
      Reg
      FVar))

 (l36-finalize-locations
  (%remove
    (Body locate)
    UVar
    Var)
  (%rename
    (Body -> Tail)
    (Var -> Loc)))

 (l37-expose-frame-var
   (%rename
     (FVar -> Disp)))

 (l39-expose-basic-blocks
   (%remove
     (Tail if)
     Pred
     (Effect nop if begin))
   (%add
     (Tail
       (if (Relop Triv Triv) (Label) (Label)))))

 (l41-flatten-program
   (%remove
     Prog
     Tail)
   (%rename
     (Effect -> Statement))
   (%add
     (Prog
       (code Statement * Statement))
     (Statement
       (if (Relop Triv Triv) (jump Label))
       (if (not (Relop Triv Triv)) (jump Label))
       (jump Triv)
       Label)))
)

