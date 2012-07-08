(library (GrammarCompiler haskell emit-haskell)
         (export emit-haskell)
         (import (chezscheme)
                 (GrammarCompiler common match))

(define emit-haskell
  (lambda (path)
    (lambda (x)
      (match x
        (((module ,name . ,types) (print . ,prints))
         (let ((path (replace #\/ #\. path)))
           (begin
             (deriving-pragma)
             (printf "module ~a.~a where\n\n" path name)
             (haskell-imports)
             (Types types)
             (newline)
             (PP prints)
             (newline)
             (Deriving types))))))))

(define Types
  (lambda (types)
    (unless (null? types)
      (match (car types)
        ((data ,name ,form ,forms ...)
         (begin
           (printf "data ~a\n" name)
           (printf "  =")
           (Form #t form)
           (newline)
           (for-each
             (lambda (f)
               (printf "  |")
               (Form #t f)
               (newline))
             forms)
           (Types (cdr types))))
        (,else
         (begin
           (printf "-- unrecognized: ~a\n" else)
           (Types (cdr types))))))))

(define Form
  (lambda (lead-space? e)
    (unless (null? e)
      (match e
        (,s (guard (symbol? s))
            (when lead-space? (printf " "))
            (printf "~a" s))
        (((list ,l) . ,rest)
         (begin
           (when lead-space? (printf " "))
           (printf "[")
           (Form #f `(,l))
           (printf "]")
           (Form lead-space? rest)))
        (((tuple ,t . ,t*) . ,rest)
         (begin
           (when lead-space? (printf " "))
           (printf "(")
           (Form #f `(,t))
           (for-each
             (lambda (t)
               (printf ",")
               (Form #f `(,t)))
             t*)
           (printf ")")))
        ((,x . ,rest)
         (begin
           (Form lead-space? x)
           (Form #t rest)))))))

(define deriving-pragma
  (lambda ()
    (printf "{-# LANGUAGE StandaloneDeriving #-}\n")
    (newline)))

(define haskell-imports
  (lambda ()
    (printf "import StringTable.Atom\n")
    (printf "import Data.Int\n")
    (printf "import FrameworkHs.Prims\n")
    (printf "import FrameworkHs.Helpers\n")
    (newline)))

(define format-term/preds
  (lambda (t/p*)
    (let ((ss (map (lambda (t/p)
                     (let ((t (car t/p)) (p (cadr t/p)))
                       (format "~a, ~a\n" t p)))
                   t/p*)))
      (let ((first (string-append "  ( " (car ss)))
            (rest (map (lambda (s)
                         (string-append "  , " s))
                       (cdr ss))))
        (string-append
         (apply string-append first rest)
         "  )\n")))))

(define Deriving
  (lambda (types)
    (for-each
      (lambda (t)
        (match t
          ((data ,name . ,subs)
           (print-deriving '(Read Show Eq Ord) name))
          (,else (void))))
      types)))

(define print-deriving
  (lambda (classes type)
    (for-each
     (lambda (c)
       (printf "deriving instance ~a ~a\n" c type))
     classes)))

(define PP
  (lambda (prints)
    (map
     (lambda (p)
       (match p
         ((,name (,form* ,f*) ...)
          (printf "instance PP ~a where\n" name)
          (let loop ((form* form*) (f* f*))
            (unless (null? form*)
              (begin
                (printf "  pp ~a = ~a\n" (car form*) (format-pp (car f*)))
                (loop (cdr form*) (cdr f*))))))))
     prints)))

(define format-pp
  (lambda (f)
    (define List
      (lambda (l)
        (cond
          ((null? l) "")
          ((null? (cdr l)) (format-pp (car l)))
          (else (format "~a,~a" (format-pp (car l)) (List (cdr l)))))))
    (define Func
      (lambda (f)
        (match f
          ((lambda ,fml* ,body)
           (format "(\\(~a) -> ~a)" (List fml*) (format-pp body)))
          (pp 'pp))))
    (match f
      ((ppSexp ,[p])
       (format "(ppSexp ~a)" p))
      ((list . ,[List -> l])
       (format "[~a]" l))
      ((string ,s) (format "\"~a\"" s))
      ((map ,fn ,v)
       (format "(map ~a ~a)" (Func fn) v))
      ((cons ,[a] ,[d])
       (format "(~a : ~a)" a d))
      ((append ,[l1] ,[l2])
       (format "(~a ++ ~a)" l1 l2))
      ((pp ,s) (format "(pp ~a)" s))
      (,e (format "~a" e)))))

(define replace
  (lambda (old new s)
    (list->string
     (let loop ((ls (string->list s)))
       (cond
         ((null? ls) '())
         ((eq? (car ls) old)
          (cons new (loop (cdr ls))))
         (else
          (cons (car ls) (loop (cdr ls)))))))))

)
