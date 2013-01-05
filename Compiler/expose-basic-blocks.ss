(library (compiler expose-basic-blocks)
  (export expose-basic-blocks)
  (import
    (chezscheme)
    (framework helpers)
    (framework match))

(define-who expose-basic-blocks
  (define (Tail x)
    (match x
      [(if ,pred ,[conseq cb*] ,[altern ab*])
       (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
         (let-values ([(pred pb*) (Pred pred clab alab)])
           (values pred
             `(,pb* ...
               [,clab (lambda () ,conseq)]
               ,cb* ...
               [,alab (lambda () ,altern)]
               ,ab* ...))))]
      [(begin ,effect* ... ,[tail tb*])
       (let-values ([(x xb*) (Effect* effect* `(,tail))])
         (values x `(,xb* ... ,tb* ...)))]
      [(,triv) (values `(,triv) '())]
      [,x (error who "invalid Tail ~s" x)]))
  (define (Pred x tlab flab)
    (match x
      [(true) (values `(,tlab) '())]
      [(false) (values `(,flab) '())]
      [(if ,pred ,[conseq cb*] ,[altern ab*])
       (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
         (let-values ([(pred pb*) (Pred pred clab alab)])
           (values pred
             `(,pb* ...
               [,clab (lambda () ,conseq)]
               ,cb* ...
               [,alab (lambda () ,altern)]
               ,ab* ...))))]
      [(begin ,effect* ... ,[pred pb*])
       (let-values ([(x xb*) (Effect* effect* `(,pred))])
         (values x `(,xb* ... ,pb* ...)))]
      [(,relop ,triv1 ,triv2)
       (values `(if (,relop ,triv1 ,triv2) (,tlab) (,flab)) '())]
      [,x (error who "invalid Pred ~s" x)]))
  (define (Effect* x* rest*)
    (match x*
      [() (values (make-begin rest*) '())]
      [(,x* ... ,x) (Effect x* x rest*)]))
  (define (Effect x* x rest*)
    (match x
      [(nop) (Effect* x* rest*)]
      [(set! ,lhs ,rhs) (Effect* x* `((set! ,lhs ,rhs) ,rest* ...))]
      [(if ,pred ,conseq ,altern)
       (let ([clab (unique-label 'c)]
             [alab (unique-label 'a)]
             [jlab (unique-label 'j)])
         (let-values ([(conseq cb*) (Effect '() conseq `((,jlab)))]
                      [(altern ab*) (Effect '() altern `((,jlab)))]
                      [(pred pb*) (Pred pred clab alab)])
           (let-values ([(x xb*) (Effect* x* `(,pred))])
             (values x
               `(,xb* ...
                 ,pb* ...
                 [,clab (lambda () ,conseq)]
                 ,cb* ...
                 [,alab (lambda () ,altern)]
                 ,ab* ...
                 [,jlab (lambda () ,(make-begin rest*))])))))]
      [(begin ,effect* ...) (Effect* `(,x* ... ,effect* ...) rest*)]
      [,x (error who "invalid Effect ~s" x)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Tail -> tail* b**])] ...) ,[Tail -> tail b*])
       `(letrec ([,label* (lambda () ,tail*)] ... ,b** ... ... ,b* ...) ,tail)]
      [,x (error who "invalid Program ~s" x)])))

;; ;; alternative with special-casing obviated by optimize-jumps
;; (define-who expose-basic-blocks
;;   (define (Tail x)
;;     (match x
;;       ;; special-case cases where conseq and/or altern are jumps to labels
;;       [(if ,pred (,clab) (,alab))
;;        (guard (label? clab) (label? alab))
;;        (Pred pred clab alab)]
;;       [(if ,pred ,[conseq cb*] (,alab))
;;        (guard (label? alab))
;;        (let ([clab (unique-label 'c)])
;;          (let-values ([(tail xb*) (Pred pred clab alab)])
;;            (values tail
;;              `(,xb* ...
;;                 [,clab (lambda () ,conseq)]
;;                 ,cb* ...))))]
;;       [(if ,pred (,clab) ,[altern ab*])
;;        (guard (label? clab))
;;        (let ([alab (unique-label 'a)])
;;          (let-values ([(tail xb*) (Pred pred clab alab)])
;;            (values tail
;;              `(,xb* ...
;;                 [,alab (lambda () ,altern)]
;;                 ,ab* ...))))]
;;       [(if ,pred ,[conseq cb*] ,[altern ab*])
;;        (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
;;          (let-values ([(pred pb*) (Pred pred clab alab)])
;;            (values pred
;;              `(,pb* ...
;;                 [,clab (lambda () ,conseq)]
;;                 ,cb* ...
;;                 [,alab (lambda () ,altern)]
;;                 ,ab* ...))))]
;;       [(begin ,effect* ... ,[tail tb*])
;;        (let-values ([(x xb*) (Effect* effect* `(,tail))])
;;          (values x `(,xb* ... ,tb* ...)))]
;;       [(,triv) (values `(,triv) '())]
;;       [,x (error who "invalid Tail ~s" x)]))
;;   (define (Pred x tlab flab)
;;     (match x
;;       [(true) (values `(,tlab) '())]
;;       [(false) (values `(,flab) '())]
;;       ;; (not (not x))
;;       [(if ,x (true) (false)) (Pred x tlab flab)]
;;       ;; (not x)
;;       [(if ,x (false) (true)) (Pred x flab tlab)]
;;       ;; (or (not x) y)
;;       [(if ,pred ,[conseq cb*] (true))
;;        (let ([clab (unique-label 'c)])
;;          (let-values ([(expr xb*) (Pred pred clab tlab)])
;;            (values expr
;;              `(,xb* ...
;;                 [,clab (lambda () ,conseq)]
;;                 ,cb* ...))))]
;;       ;; (and x y)
;;       [(if ,pred ,[conseq cb*] (false))
;;        (let ([clab (unique-label 'c)])
;;          (let-values ([(expr xb*) (Pred pred clab flab)])
;;            (values expr
;;              `(,xb* ...
;;                 [,clab (lambda () ,conseq)]
;;                 ,cb* ...))))]
;;       ;; (or x y)
;;       [(if ,pred (true) ,[altern ab*])
;;        (let ([alab (unique-label 'a)])
;;          (let-values ([(expr xb*) (Pred pred tlab alab)])
;;            (values expr
;;              `(,xb* ...
;;                 [,alab (lambda () ,altern)]
;;                 ,ab* ...))))]
;;       ;; (and (not x) y)
;;       [(if ,pred (false) ,[altern ab*])
;;        (let ([alab (unique-label 'a)])
;;          (let-values ([(expr xb*) (Pred pred flab alab)])
;;            (values expr
;;              `(,xb* ...
;;                 [,alab (lambda () ,altern)]
;;                 ,ab* ...))))]
;;       [(if ,pred ,[conseq cb*] ,[altern ab*])
;;        (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
;;          (let-values ([(pred pb*) (Pred pred clab alab)])
;;            (values pred
;;              `(,pb* ...
;;                 [,clab (lambda () ,conseq)]
;;                 ,cb* ...
;;                 [,alab (lambda () ,altern)]
;;                 ,ab* ...))))]
;;       [(begin ,effect* ... ,[pred pb*])
;;        (let-values ([(x xb*) (Effect* effect* `(,pred))])
;;          (values x `(,xb* ... ,pb* ...)))]
;;       [(,relop ,triv1 ,triv2)
;;        (values `(if (,relop ,triv1 ,triv2) (,tlab) (,flab)) '())]
;;       [,x (error who "invalid Pred ~s" x)]))
;;   (define (Effect* x* rest*)
;;     (match x*
;;       [() (values (make-begin rest*) '())]
;;       [(,x* ... ,x) (Effect x* x rest*)]))
;;   (define (Effect x* x rest*)
;;     (match x
;;       [(nop) (Effect* x* rest*)]
;;       [(set! ,lhs ,rhs) (Effect* x* `((set! ,lhs ,rhs) ,rest* ...))]
;;       ;; (if x y)
;;       [(if ,pred ,conseq (nop))
;;        (let ([clab (unique-label 'c)] [jlab (unique-label 'j)])
;;          (let-values ([(conseq cb*) (Effect '() conseq `((,jlab)))]
;;                       [(expr xb*) (Pred pred clab jlab)])
;;            (let-values ([(expr eb*) (Effect* x* `(,expr))])
;;              (values expr
;;                `(,eb* ...
;;                   ,xb* ...
;;                   [,clab (lambda () ,conseq)]
;;                   [,jlab (lambda () ,(make-begin rest*))]
;;                   ,cb* ...)))))]
;;       ;; (if (not x) y)
;;       [(if ,pred (nop) ,altern)
;;        (let ([alab (unique-label 'a)] [jlab (unique-label 'j)])
;;          (let-values ([(altern ab*) (Effect '() altern `((,jlab)))]
;;                       [(expr xb*) (Pred pred jlab alab)])
;;            (let-values ([(expr eb*) (Effect* x* `(,expr))])
;;              (values expr
;;                `(,eb* ...
;;                   ,xb* ...
;;                   [,alab (lambda () ,altern)]
;;                   [,jlab (lambda () ,(make-begin rest*))]
;;                   ,ab* ...)))))]
;;       [(if ,pred ,conseq ,altern)
;;        (let ([clab (unique-label 'c)]
;;              [alab (unique-label 'a)]
;;              [jlab (unique-label 'j)])
;;          (let-values ([(conseq cb*) (Effect '() conseq `((,jlab)))]
;;                       [(altern ab*) (Effect '() altern `((,jlab)))]
;;                       [(pred pb*) (Pred pred clab alab)])
;;            (let-values ([(x xb*) (Effect* x* `(,pred))])
;;              (values x
;;                `(,xb* ...
;;                   ,pb* ...
;;                   [,clab (lambda () ,conseq)]
;;                   ,cb* ...
;;                   [,alab (lambda () ,altern)]
;;                   ,ab* ...
;;                   [,jlab (lambda () ,(make-begin rest*))])))))]
;;       [(begin ,effect* ...) (Effect* `(,x* ... ,effect* ...) rest*)]
;;       [,x (error who "invalid Effect ~s" x)]))
;;   (lambda (x)
;;     (match x
;;       [(letrec ([,label* (lambda () ,[Tail -> tail* b**])] ...) ,[Tail -> tail b*])
;;        `(letrec ([,label* (lambda () ,tail*)] ... ,b** ... ... ,b* ...) ,tail)]
;;       [,x (error who "invalid Program ~s" x)])))

;; ;; alternative: collects bindings via side effects
;; (define-who expose-basic-blocks
;;   (lambda (x)
;;     (define b* '())
;;     (define (Tail x)
;;       (match x
;;         [(if ,pred ,[conseq] ,[altern])
;;          (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
;;            (set! b* `(,b* ...
;;                       [,clab (lambda () ,conseq)]
;;                       [,alab (lambda () ,altern)]))
;;            (Pred pred clab alab))]
;;         [(begin ,effect* ... ,[tail]) (Effect* effect* `(,tail))]
;;         [(,triv) `(,triv)]
;;         [,x (error who "invalid Tail ~s" x)]))
;;     (define (Pred x tlab flab)
;;       (match x
;;         [(true) `(,tlab)]
;;         [(false) `(,flab)]
;;         [(if ,pred ,[conseq] ,[altern])
;;          (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
;;            (set! b* `(,b* ...
;;                       [,clab (lambda () ,conseq)]
;;                       [,alab (lambda () ,altern)]))
;;            (Pred pred clab alab))]
;;         [(begin ,effect* ... ,[expr]) (Effect* effect* `(,expr))]
;;         [(,relop ,triv1 ,triv2) `(if (,relop ,triv1 ,triv2) (,tlab) (,flab))]
;;         [,x (error who "invalid Pred ~s" x)]))
;;     (define (Effect* x* rest*)
;;       (match x*
;;         [() (make-begin rest*)]
;;         [(,x* ... ,x) (Effect x* x rest*)]))
;;     (define (Effect x* x rest*)
;;       (match x
;;         [(nop) (Effect* x* rest*)]
;;         [(set! ,lhs ,rhs) (Effect* x* `((set! ,lhs ,rhs) ,rest* ...))]
;;         [(if ,pred ,conseq ,altern)
;;          (let ([clab (unique-label 'c)]
;;                [alab (unique-label 'a)]
;;                [jlab (unique-label 'j)])
;;            (let ([conseq (Effect '() conseq `((,jlab)))]
;;                  [altern (Effect '() altern `((,jlab)))]
;;                  [pred (Pred pred clab alab)])
;;              (set! b* `(,b* ...
;;                         [,clab (lambda () ,conseq)]
;;                         [,alab (lambda () ,altern)]
;;                         [,jlab (lambda () ,(make-begin rest*))]))
;;              (Effect* x* `(,pred))))]
;;         [(begin ,effect* ...) (Effect* `(,x* ... ,effect* ...) rest*)]
;;         [,x (error who "invalid Effect ~s" x)]))
;;     (match x
;;       [(letrec ([,label* (lambda () ,[Tail -> tail*])] ...) ,[Tail -> tail])
;;        `(letrec ([,label* (lambda () ,tail*)] ... ,b* ...) ,tail)]
;;       [,x (error who "invalid Program ~s" x)])))

;; ;; alternative: threads b*
;; (define-who expose-basic-blocks
;;   (define (Tail x b*)
;;     (match x
;;       [(if ,pred ,[conseq b*] ,altern)
;;        (let-values ([(altern b*) (Tail altern b*)])
;;          (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
;;            (Pred pred clab alab
;;              `(,b* ...
;;                [,clab (lambda () ,conseq)]
;;                [,alab (lambda () ,altern)]))))]
;;       [(begin ,effect* ... ,[tail b*]) (Effect* effect* `(,tail) b*)]
;;       [(,triv) (values `(,triv) b*)]
;;       [,x (error who "invalid Tail ~s" x)]))
;;   (define (Pred x tlab flab b*)
;;     (match x
;;       [(true) (values `(,tlab) b*)]
;;       [(false) (values `(,flab) b*)]
;;       [(if ,pred ,[conseq b*] ,altern)
;;        (let-values ([(altern b*) (Pred altern tlab flab b*)])
;;          (let ([clab (unique-label 'c)] [alab (unique-label 'a)])
;;            (Pred pred clab alab
;;              `(,b* ...
;;                [,clab (lambda () ,conseq)]
;;                [,alab (lambda () ,altern)]))))]
;;       [(begin ,effect* ... ,[pred b*]) (Effect* effect* `(,pred) b*)]
;;       [(,relop ,t1 ,t2) (values `(if (,relop ,t1 ,t2) (,tlab) (,flab)) b*)]
;;       [,x (error who "invalid Pred ~s" x)]))
;;   (define (Effect* x* rest* b*)
;;     (match x*
;;       [() (values (make-begin rest*) b*)]
;;       [(,x* ... ,x) (Effect x* x rest* b*)]))
;;   (define (Effect x* x rest* b*)
;;     (match x
;;       [(nop) (Effect* x* rest* b*)]
;;       [(set! ,lhs ,rhs) (Effect* x* `((set! ,lhs ,rhs) ,rest* ...) b*)]
;;       [(if ,pred ,conseq ,altern)
;;        (let ([clab (unique-label 'c)]
;;              [alab (unique-label 'a)]
;;              [jlab (unique-label 'j)])
;;          (let*-values ([(conseq b*) (Effect '() conseq `((,jlab)) b*)]
;;                        [(altern b*) (Effect '() altern `((,jlab)) b*)]
;;                        [(pred b*) (Pred pred clab alab b*)])
;;            (Effect* x* `(,pred)
;;              `(,b* ...
;;                [,clab (lambda () ,conseq)]
;;                [,alab (lambda () ,altern)]
;;                [,jlab (lambda () ,(make-begin rest*))]))))]
;;       [(begin ,effect* ...) (Effect* `(,x* ... ,effect* ...) rest* b*)]
;;       [,x (error who "invalid Effect ~s" x)]))
;;   (lambda (x)
;;     (match x
;;       [(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
;;        (let-values ([(tail* b*)
;;                      (let f ([tail* tail*] [b* '()])
;;                        (if (null? tail*)
;;                            (values '() b*)
;;                            (let-values ([(tail b*) (Tail (car tail*) b*)])
;;                              (let-values ([(tail* b*) (f (cdr tail*) b*)])
;;                                (values (cons tail tail*) b*)))))])
;;          (let-values ([(tail b*) (Tail tail b*)])
;;            `(letrec ([,label* (lambda () ,tail*)] ... ,b* ...) ,tail)))]
;;       [,x (error who "invalid Program ~s" x)])))

)