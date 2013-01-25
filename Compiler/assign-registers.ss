(library (Compiler assign-registers)
  (export assign-registers)
  (import (chezscheme)
          (Framework helpers)
          (Framework match))

(define-who assign-registers
  (define find-used
    (lambda (conflict* home*)
      (cond
        [(null? conflict*) '()]
        [(register? (car conflict*)) 
         (set-cons (car conflict*) (find-used (cdr conflict*) home*))]
        [(assq (car conflict*) home*) => 
         (lambda (x) (set-cons (cadr x) (find-used (cdr conflict*) home*)))]
        [else (find-used (cdr conflict*) home*)])))
  (define select-register
    (lambda (var conflict* home*)
      (let ([used* (find-used conflict* home*)])
        (let ([available* (difference registers used*)])
          (and (not (null? available*)) (car available*))))))
  (define rem-conflicts!
    (lambda (ct var conflict*)
      (for-each
        (lambda (x)
          (when (uvar? x)
            (let ([a (assq x ct)])
              (set-cdr! a (remq var (cdr a))))))
        conflict*)))
  (define find-homes
    (lambda (var* ct)
      (define k (length registers))
      (define low-degree?
        (lambda (var)
          (< (length (cdr (assq var ct))) k)))
      (let f ([var* var*])
        (if (null? var*)
            '()
            (let ([var (or (find low-degree? var*) (car var*))])
              (let ([conflict* (cdr (assq var ct))] [var* (remq var var*)])
                (rem-conflicts! ct var conflict*)
                (let ([home* (f var*)])
                  (let ([reg (select-register var conflict* home*)])
                    (if reg
                        (cons `[,var ,reg] home*)
                        home*)))))))))
  (define Body
    (lambda (x)
      (match x
        [(locals (,uvar* ...) (register-conflict ,ct ,tail))
         (let ([home* (find-homes uvar* ct)])
           (let ([spill* (difference uvar* (map car home*))])
             (if (null? spill*)
                 `(locate (,home* ...) ,tail)
                 (error who "unable to assign registers to ~s" spill*))))]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))
  
)