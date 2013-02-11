;; P423
;; Week 5 grammars
;;
;; Passes:
;;   verify-scheme              l-01 -> l-01
;;   uncover-frame-conflict     l-01 -> l-27
;;   introduce-allocation-forms l-27 -> l-28
;;     select-instructions       l-28 -> l-28
;;     uncover-register-conflict l-28 -> l-32
;;     assign-registers          l-32 -> l-33
;;     everybody-home?           l-33 -> bool
;;     assign-frame              l-33 -> l-28
;;     finalize-frame-locations  l-28 -> l-28
;;   discard-call-live          l-33 -> l-35
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

 (l27-uncover-frame-conflict
    (%remove 
      (Body locals))
    (%add
      (Body
        (locals (UVar *)
                (frame-conflict ((UVar Var *) *)
                Tail)))))

;; This is an important grammar.  Its the one that is used at the top
;; and end of every iteration of the register-allocation loop.
(l28-introduce-allocation-forms
    (%remove 
      (Body locals))
    (%add
      (Body
        (locals (UVar *)
                (ulocals (UVar *)
                         (locate ((UVar FVar) *) 
                                 (frame-conflict ((UVar Var *) *)
                                 Tail))))
        (locate ((UVar Loc) *) Tail)
        )))

;; Adds register-conflict to the deeply nested Body forms.
(l32-uncover-register-conflict
  (%remove
    (Body locals))
  (%add
    ;; Ignore conflicts with frame vars:
    (Conflict Reg UVar)
    (Body
      (locals (UVar *)
              (ulocals (UVar *)
                       (locate ((UVar FVar) *) 
                               (frame-conflict ((UVar Var *) *)
                                               (register-conflict ((UVar Conflict *) *)
                                                                  Tail))))))))
;; Adds the 'spill' form.
(l33-assign-registers
  (%remove
    (Body locals)
    (Conflict))
  (%add
    (Body
      (locals (UVar *)
              (ulocals (UVar *)
                       (spills (UVar *)
                               (locate ((UVar FVar) *) 
                                       (frame-conflict ((UVar Var *) *) 
                                                       Tail))))))))


; assign-frame: This is the same as l28-introduce-allocation-forms
; finalize-frame-locations: also uses l28-introduce-allocation-forms

(l35-discard-call-live
  (%remove
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
