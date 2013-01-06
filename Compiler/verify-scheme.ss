(library (Compiler verify-scheme)
  (export verify-scheme)
  (import (chezscheme)
	  (Framework helpers)
	  (Framework match))

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; Program   -> (begin Statement+)
;;; Statement -> (set! Var1 int64)
;;;            | (set! Var1 Var2)
;;;            | (set! Var1 (Binop Var1 int32))
;;;            | (set! Var1 (Binop Var1 Var2))
;;; Var       -> rax | rcx | rdx | rbx | rbp | rsi | rdi
;;;            | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
;;; Binop     -> + | - | *
;;;
;;; You can also find this grammar in source-grammar.ss
;;;
;;; If the input is a valid program, verify-scheme returns it
;;; unchanged; otherwise, it signals an error.

;;; define-who is defined in helpers.ss.  It not only defines the
;;; variable who to be the name of the pass being defined but also
;;; gives us a handy place to stick local helpers:

(define-who verify-scheme
  (define verify-binop
    (lambda (x)
      (unless (memq x '(+ - *))
        (error who "invalid binop ~s" x))))
  (define verify-var
    (lambda (x)
      (unless (register? x)
        (error who "invalid variable name ~s" x))))
  (define verify-int32
    (lambda (x)
      (unless (int32? x)
        (error who "invalid int32 ~s" x))))
  (define verify-int64
    (lambda (x)
      (unless (int64? x)
        (error who "invalid int64 ~s" x))))
  (define Statement
    (lambda (st)
      (match st
        [(set! ,var ,n)
         (guard (number? n))
         (verify-var var)
         (verify-int64 n)]
        [(set! ,var1 ,var2)
         (guard (symbol? var2))
         (verify-var var1)
         (verify-var var2)]
        [(set! ,var (,prim ,var ,n))
         (guard (number? n))
         (verify-binop prim)
         (verify-var var)
         (verify-int32 n)]
        [(set! ,var1 (,prim ,var1 ,var2))
         (verify-binop prim)
         (verify-var var1)
         (verify-var var2)]
        [,st (error who "invalid syntax for Statement ~s" st)])))
  (lambda (x)
    (match x
      [(begin ,[Statement -> st1] ,[Statement -> st2*] ...) x]
      [,x (error who "invalid syntax for Program: expected (begin stmt+)")])
    x))
)
