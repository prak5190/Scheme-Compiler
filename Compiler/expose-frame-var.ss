(library (compiler expose-frame-var)
  (export expose-frame-var)
  (import (chezscheme)
          (framework helpers)
          (framework match))

;;; expose-frame-var traverses the scheme source in the same grammar
;;; accepted by verify-scheme and changes frame-vars in the form
;;; fv0, fv1, etc. into explicit integer offsets from the register
;;; pointing to the frame-pointer register. To accomplish this,
;;; expose-frame-var makes use of make-disp-opnd which creates a
;;; displacement operand record expressing a register and fixed
;;; number displacment each displacement is the original frame var
;;; number multiplied by the word size (8 for 64-bit target machine)
;;; to get the byte offset.
;;; (i.e. fv0 => (make-disp-opnd frame-pointer-register 0)
;;;       fv1 => (make-disp-opnd frame-pointer-register 8)
;;;       fv2 => (make-disp-opnd frame-pointer-register 16)
;;;       fv3 => (make-disp-opnd frame-pointer-register 24)
;;;       ... well you get the idea.)
;;;
;;; Note: we use shift left by word-shift (3 for 64-bit target
;;; machine) to calculate the multiplication.

(define-who expose-frame-var
  (define Triv
    (lambda (t)
      (if (frame-var? t)
          (make-disp-opnd frame-pointer-register
            (ash (frame-var->index t) word-shift))
          t)))
  (define Pred
    (lambda (pr)
      (match pr
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[test])
         `(begin ,ef* ... ,test)]
        [(,relop ,[Triv -> tr1] ,[Triv -> tr2]) `(,relop ,tr1 ,tr2)]
        [,pr (error who "invalid Pred ~s" pr)])))
  (define Effect
    (lambda (st)
      (match st
        [(nop) '(nop)]
        [(set! ,[Triv -> var] (,binop ,[Triv -> t1] ,[Triv -> t2]))
         `(set! ,var (,binop ,t1 ,t2))]
        [(set! ,[Triv -> var] ,[Triv -> t])
         `(set! ,var ,t)]
        [(begin ,[ef] ,[ef*] ...) `(begin ,ef ,ef* ...)]
        [(if ,[Pred -> test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [,st (error who "invalid syntax for Effect ~s" st)])))
  (define Tail
    (lambda (tail)
      (match tail
        [(,[Triv -> t]) `(,t)]
        [(begin ,[Effect -> ef*] ... ,[tail])
         `(begin ,ef* ... ,tail)]
        [(if ,[Pred -> test] ,[conseq] ,[altern])
         `(if ,test ,conseq ,altern)]
        [,tail (error who "invalid syntax for Tail ~s" tail)])))
  (lambda (program)
    (match program
      [(letrec ([,label* (lambda () ,[Tail -> tail*])] ...) ,[Tail -> tail])
       `(letrec ([,label* (lambda () ,tail*)] ...) ,tail)]
      [,program (error who "invalid syntax for Program: ~s" program)])))
  
)