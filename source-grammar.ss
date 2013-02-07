;; P423
;; Week 5 grammars
;;
;; Passes:
;;   verify-scheme              l-01 -> l-01
;;
;; * uncover-frame-conflict     l-01 -> l-29
;; * introduce-allocation-forms l-29 -> l-30
;; * select-instructions        l-30 -> l-31 
;;
;;   uncover-register-conflict  l-31 -> l-32
;;   assign-registers           l-32 -> l-33
;;   discard-call-live          l-33 -> l-35
;;   finalize-locations         l-35 -> l-36
;;   expose-frame-var           l-36 -> l-37
;;   expose-basic-blocks        l-37 -> l-39
;;   flatten-program            l-39 -> l-41
;;   generate-x86-64            l-41 -> ()

;; !!!!!!! CSZ: TODO: need to add l-29-31
;; need to update top level grammar
;;    (Framework GenGrammars l29-uncover-frame-conflict)
;;    (Framework GenGrammars l30-introduce-allocation-forms)
;;    (Framework GenGrammars l31-select-instructions)
;;

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

 (l28-uncover-frame-conflict
    (%remove 
      (Body locals))
    (%add
      (Body
        (locals (UVar *)
                (frame-conflict ((UVar Var *) *)
                Tail)))))

(l29-introduce-allocation-forms
    (%remove 
      (Body locals))
    (%add
      (Body
        (locals (UVar *)
                (ulocals ()
                         (locate () 
                                 (frame-conflict ((UVar Var *) *)
                                 Tail))))

        (locate ((UVar Loc) *) Tail)
        )))

(l30-select-instructions
    (%remove 
      (Body locals))
    (%add
      (Body
        (locals (UVar *)
                (ulocals (UVar *)
                         (locate ((UVar FVar) *) 
                                 (frame-conflict ((UVar Var *) *)
                                 Tail)))))))

(l31-uncover-register-conflict
  (%remove
    (Body locals))
  (%add
    ;; Ignore conflicts with frame vars:
    (Conflict Reg UVar)
    (Body
      ;        (register-conflict ((UVar UVar * Reg *) *)
      (locals (UVar *)
              (ulocals (UVar *)
                       (locate ((UVar FVar) *) 
                               (frame-conflict ((UVar Var *) *)
                                               (register-conflict ((UVar Conflict *) *)
                                                                  Tail)))))))) 
(l32-assign-registers
  (%remove
    (Body locals))
  (%add
    (Body
      (locals (UVar *)
              (ulocals (UVar *)
                       (spills (UVar *)
                               (locate ((UVar FVar) *) 
                                       (frame-conflict ((UVar Var *) *) 
                                                       Tail))))))))
(l33-assign-frame)

(l34-finalize-frame-locations)

(l35-discard-call-live
  (%remove
    (Conflict)
    (Body locals)
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
