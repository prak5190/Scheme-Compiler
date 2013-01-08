(library (Compiler expose-frame-var)
  (export expose-frame-var)
  (import (chezscheme)
          (Framework helpers)
	  (Framework GenGrammars l37-expose-frame-var)
          (Framework match))

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
  (define Effect
    (lambda (st)
      (match st
        [(set! ,[Triv -> var] (,binop ,[Triv -> t1] ,[Triv -> t2]))
         `(set! ,var (,binop ,t1 ,t2))]
        [(set! ,[Triv -> var] ,[Triv -> t])
         `(set! ,var ,t)]
        [,st (errorf who "invalid syntax for Effect ~s" st)])))
  (define Tail
    (lambda (tail)
      (match tail
        [(,[Triv -> t]) `(,t)]
        [(begin ,[Effect -> ef*] ... ,[tail])
         `(begin ,ef* ... ,tail)]
        [,tail (errorf who "invalid syntax for Tail ~s" tail)])))
  (lambda (program)
    (verify-grammar:l37-expose-frame-var
     (match program
      [(letrec ([,label* (lambda () ,[Tail -> tail*])] ...) ,[Tail -> tail])
       `(letrec ([,label* (lambda () ,tail*)] ...) ,tail)]
      [,program (errorf who "invalid syntax for Program: ~s" program)]))))
  
)
