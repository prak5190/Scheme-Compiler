(library (Framework wrappers)
  (export pass->wrapper source/wrapper verify-scheme/wrapper generate-x86-64/wrapper)
  (import
    (chezscheme)
    (Framework GenGrammars l01-verify-scheme)
    (Framework helpers)
    (Framework driver))

(define env
  (environment
    '(chezscheme)
    '(Framework helpers)))

(define pass->wrapper
  (lambda (pass)
    (case pass
      ((source) source/wrapper)
      ((verify-scheme) verify-scheme/wrapper)
      ((generate-x86-64) generate-x86-64/wrapper)
      (else (errorf 'pass->wrapper
              "Wrapper for pass ~s not found" pass)))))

(define-language-wrapper
  (source/wrapper verify-scheme/wrapper)
  (x)
  (environment env)
  (import
    (except (chezscheme) set!)
    (Framework helpers))
  (define int64-in-range?
    (lambda (x)
      (<= (- (expt 2 63)) x (- (expt 2 63) 1))))
  (define handle-overflow
    (lambda (x)
      (cond
        [(not (number? x)) x]
        [(int64-in-range? x) x]
        [(not (= x (logand 18446744073709551615 x)))
         (handle-overflow (logand 18446744073709551615 x))]
        [(< x 0) (handle-overflow (+ x (expt 2 64)))]
        [else (handle-overflow (- x (expt 2 64)))])))
  (define-syntax set!
    (let ()
      (import scheme)
      (syntax-rules ()
        [(_ x expr) (set! x (handle-overflow expr))])))
  (reset-machine-state!)
  ,(if (grammar-verification) (verify-grammar:l01-verify-scheme x) x)
  ,return-value-register)

(define (generate-x86-64/wrapper program)
  (let-values ([(out in err pid)
                (open-process-ports
                  (format "exec '~a'" program)
                  (buffer-mode block)
                  (native-transcoder))])
    (read in)))

)
