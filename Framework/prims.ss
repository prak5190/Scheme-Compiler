
;; This library exposes two categories of primitives for dealing with
;; language terminals in grammars (i.e. primitive things like Int and UVar).
;;
;;  (1) predicates: isFoo function returns #t if its input is a Foo
;;  (2) checkers: the Foo function checks that the input is a valid
;;      Foo, returning #f if everything is ok, and an error string otherwise.

(library (Framework prims)
         (export
          UVar FVar Label Reg Relop Binop Disp Ind Int64 Int32 UInt6 Integer
          isUVar isFVar isLabel isReg isRelop isBinop isDisp isInd
          isInt64 isInt32 isUInt6 invalid-expr)
         (import (chezscheme)
                 (Framework match)
                 (Framework helpers))

(define split
  (lambda (c ls)
    (cond
      ((null? ls) (values '() '()))
      ((eq? (car ls) c) (values '() (cdr ls)))
      (else
       (let-values (((b a) (split c (cdr ls))))
         (values (cons (car ls) b) a))))))

;; Return a string representing an error message:
(define invalid-expr
  (lambda (t e)
    (format "Invalid ~a: ~a\n" t e)))

(define Index
  (lambda (ls)
    (and (not (null? ls))
         (list? ls)
         (not (eq? (car ls) #\0))
         (for-all char-numeric? ls))))

;; This is very slow... TODO: operate directly on the strings:
(define isUVar
  (lambda (x)
    (and (symbol? x)
      (let ((ls (string->list (symbol->string x))))
	(let-values (((b a) (split #\. ls)))
	  (Index a))))))

(define isFVar
  (lambda (x)
    (and (symbol? x)
      (let ((ls (string->list (symbol->string x))))
	(match ls
	  ((#\f #\v . ,ind) (Index ind))
	  (,e #f))))))

(define isLabel
  (lambda (x)
    (and (symbol? x)
      (let ((ls (string->list (symbol->string x))))
	(let-values (((b a) (split #\$ ls)))
	  (Index a))))))

(define relops '(< <= = >= >))
(define binops '(* - + logand logor sra))

(define isReg
  (lambda (x)
    (and (memq x registers) #t)))

(define isRelop
  (lambda (x)
    (and (memq x relops) #t)))

(define isBinop
  (lambda (x)
    (and (memq x binops) #t)))

(define isDisp
  (lambda (x)
    (match x
      [(disp ,[isReg -> reg] ,[isInt64 -> ind])
       (and reg ind)]
      [,_ #f])))

(define isInd
  (lambda (x)
    (match x
      [(ind ,[isReg -> reg] ,[isReg -> off])
       (and reg off)]
      [,_ #f])))

(define isInt64 int64?)

(define isInt32 int32?)

(define isUInt6 uint6?)

;; Terminals -- the contract is that these functions return #f if
;; NOTHING IS WRONG (i.e. the datum passes).  Otherwise, they return
;; an error message.
(define UVar (lambda (x) (if (isUVar x) #f (invalid-expr 'UVar x))))
(define FVar (lambda (x) (if (isFVar x) #f (invalid-expr 'FVar x)))) 
(define Label (lambda (x) (if (isLabel x) #f (invalid-expr 'Label x))))
(define Reg (lambda (x) (if (isReg x) #f (invalid-expr 'Reg x))))
(define Relop (lambda (x) (if (isRelop x) #f (invalid-expr 'Relop x))))
(define Binop (lambda (x) (if (isBinop x) #f (invalid-expr 'Binop x))))
(define Disp (lambda (x) (if (isDisp x) #f (invalid-expr 'Disp x))))
(define Ind (lambda (x) (if (isInd x) #f (invalid-expr 'Ind x))))
(define Int64 (lambda (x) (if (isInt64 x) #f (invalid-expr 'Int64 x))))
(define Int32 (lambda (x) (if (isInt32 x) #f (invalid-expr 'Int32 x))))
(define UInt6 (lambda (x) (if (isUInt6 x) #f (invalid-expr 'UInt6 x))))
(define Integer (lambda (x) (if (integer? x) #f (invalid-expr 'Integer x))))

)
