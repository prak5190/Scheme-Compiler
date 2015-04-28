(library (Compiler generate-x86-64)
  (export
    generate-x86-64
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers))
  
  ;; Generate x86-64 Code
  (define-who (generate-x86-64 exp)   
    (define (binop->instr binop)
      (match binop 
        [+ 'addq]
        [- 'subq]
        [* 'imulq]
        [sra 'sarq]
        [logand 'andq]
        [logor 'orq]))
    (define (relop->instr binop)
      (match binop
        [< 'jl]
        [> 'jg]
        [= 'je]
        [<= 'jle]
        [>= 'jge]))
    (define (relop->oppinstr binop)
      (match binop
        [< 'jge]
        [> 'jle]
        [= 'jne]
        [<= 'jg]
        [>= 'jl]))
    (define (convertStatement code)
      (match code
        [(if (,r ,a ,b) (jump ,x)) (emit 'cmp b a) (emit-jump (relop->instr r) x)]
        [(if (not (,r ,a ,b)) (jump ,x)) (emit 'cmp b a) (emit-jump (relop->oppinstr r) x)]        
        [(set! ,dst (,b ,t1 ,src)) (emit (binop->instr b) src dst)]
        [(set! ,dst ,src) (guard (label? src)) (emit 'leaq src dst)]        
        [(set! ,dst ,src) (emit 'movq src dst)]
        [(jump ,x) (emit-jump 'jmp x)]
        [,x (guard (label? x)) (emit-label x)]))
    
    (define (convert exp)
        (match exp         
          [(code ,x ...) (for-each convertStatement x)]          
          [,else (errorf who "unexpected statement ~S" else) else]))
    
    (emit-program (convert exp))))
