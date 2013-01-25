;; P423
;; Week 4 grammars
;;
;; Passes:
;;   verify-scheme              l-01 -> l-01
;; * uncover-register-conflict  l-01 -> l-32
;; * assign-registers           l-32 -> l-33
;; * discard-call-live          l-33 -> l-35
;;   finalize-locations         l-35 -> l-36
;;   expose-frame-var           l-36 -> l-37
;;   expose-basic-blocks        l-37 -> l-39
;;   flatten-program            l-39 -> l-41
;;   generate-x86-64            l-41 -> ()

(p423-grammars
  (l01-verify-scheme
    (start Prog)
    (Prog
      (letrec ((Label (lambda () Body)) *) Body))
    (Body
      (locals (UVar *) Tail))
    (Tail      
      (if Pred Tail Tail)
      (begin Effect * Tail)
      (Triv Loc *))
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

(l32-uncover-register-conflict
  (%remove
    (Body locals))
  (%add
    (Body
      (locals (UVar *)
        (register-conflict ((UVar UVar * Reg *) *)
          Tail)))))

(l33-assign-registers
  (%remove
    (Body locals))
  (%add
    (Body
      (locate ((UVar Reg) *)
        Tail))))

(l35-discard-call-live
  (%remove
    (Tail Triv))
  (%add
    (Tail (Triv))))

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
