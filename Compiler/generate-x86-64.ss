#!chezscheme
(library (Compiler generate-x86-64)
  (export generate-x86-64)
  (import (chezscheme)
	  (Framework helpers)
	  (Framework GenGrammars l01-verify-scheme)
	  (Framework match))

;;; generate-x86-64 accepts a valid program in the grammar described
;;; in source-grammar.ss, and writes an equivalent x86_64 assembly
;;; language program to the current output port.  It uses the emit
;;; macro from helpers.ss, which is smart enough to handle register
;;; and immediate operands properly (among others), which simplifies
;;; the task.

(define-who generate-x86-64
  (define prim->inst
    (lambda (op)
      (case op
        [(+) 'addq]
        [(-) 'subq]
        [(*) 'imulq]
        [else (error who "unexpected binop ~s" op)])))
  (define Statement
    (lambda (st)
      (match st
        [(set! ,dst (,prim ,dst ,src))
         (emit (prim->inst prim) src dst)]
        [(set! ,dst ,src)
         (emit 'movq src dst)]
        [,st (error who "unexpected statement ~s" st)])))
  (lambda (x)
    (verify-grammar:l01-verify-scheme x)
    (match x
      [(begin ,st* ...) (emit-program (for-each Statement st*))]
      [,x (error who "unexpected program ~s" x)])))

)
