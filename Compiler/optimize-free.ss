(library (Compiler optimize-free)
  (export
   optimize-free
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  (define-record node (name free*) ([wkmt #t] [link* '()]))
  ;; --> freels wkmt*
  (define (prune-free name* free** wk* wkmt*)
    (let* ((is-in-letrec (lambda(x) (memq x name*)))
           (is-free? (lambda(x) (not (or (memq x wkmt*) (memq x name*)))))
           (node-ls (map make-node name* free**))
           (node-als (map (lambda(x y) `(,x ,y)) name* node-ls))
           (get-node (lambda(name) (cadr (assq name node-als)))))
      ;; Create links
      (let loop ((name* name*) (free** free**))
        (if (not (null? name*))
            (let* ((name (car name*)) (free* (car free**))
                   (node (get-node name))
                   (fr (node-free* node))
                   (lfr (filter is-in-letrec fr)))
              (set-node-free*! node (filter is-free? fr))
              (map (lambda(x) (let* ((n (get-node x))
                                     (link (node-link* n)))
                                (set-node-link*! n (cons node link)))) lfr)          
              (loop (cdr name*) (cdr free**)))))      
      ;; Set false recurrsively for links 
      (let l1 ((name* name*))
        (if (not (null? name*))
            (begin
              (if (is-in-letrec (car name*))
                  (let* ((name (car name*))
                         (node (get-node name))
                         (fr (node-free* node))
                         (ln (node-link* node)))
                    (if (node-wkmt node)
                        (if (or (not (memq name wk*)) (not (null? fr)))
                            (begin
                              (set-node-wkmt! node #f)
                              (map (lambda(n)                                 
                                     (set-node-free*! n (cons name (node-free* n)))
                                     (set-node-wkmt! n #f)) ln)
                              ;; All the free links in the n
                              (map (lambda(n) (l1 (node-free* n))) ln)
                              )))))
              (l1 (cdr name*)))))
      ;; Now map all node's fr and also return
      (let ((nodes (map get-node name*)))
        (values (map node-free* nodes) (append (map node-name (filter node-wkmt nodes)) wkmt*)))))
  
  
  (define-who (optimize-free program)
    ;; All ls passed in is the set of well known procedures and returned is the same - SPS
    (define (Exp* expls free** ls)
      (cond
       ((null? expls) (values expls ls))
       (else (let*-values (((x ls) (Exp (car expls) (car free**) ls))
                           ((y ls) (Exp* (cdr expls) (cdr free**) ls)))
               (values (cons x y) ls)))))
    
    (define (Exp exp bfc ls)                   ;get-trace-define
      (match exp
        ((,x (lambda (,y ...) (bind-free ,bf ,z))) (let-values (((z ls) (Expr z ls)))                                                     
                                                     (if (and (null? bfc) (memq x (map unique-label ls)))
                                                         (values `(,x (lambda (,(cdr y) ...) (bind-free (dummy) ,z))) ls)
                                                         (values `(,x (lambda (,y ...) (bind-free (,(car bf) ,bfc ...) ,z))) ls))))))
  
    (define (Expr* expls ls)
      (cond
       ((null? expls) (values '() ls))
       (else (let*-values (((x ls) (Expr (car expls) ls))
                           ((y ls) (Expr* (cdr expls) ls)))
               (values (cons x y) ls)))))
    
    (define (Expr exp ls)
      (match exp
        ((if ,x ,y ,z) (let*-values (((x ls) (Expr x ls))
                                     ((y ls) (Expr y ls))
                                     ((z ls) (Expr z ls)))
                         (values `(if ,x ,y ,z) ls)))
        ((begin ,x ...) (let*-values (((x ls) (Expr* x ls)))
                          (values `(begin ,x ... ) ls)))
        ((let ((,x ,y) ...) ,z) (let*-values (((y ls) (Expr* y ls))
                                              ((z ls) (Expr z ls)))
                                  (values `(let ,(map (lambda(x y) `(,x ,y)) x y) ,z) ls)))
        ((letrec (,x ...) (closures ,lls (well-known ,wls ,y))) (let*-values (((free** ls) (prune-free (map car lls) (map cddr lls)  wls ls))
                                                                              ((x ls) (Exp* x free** ls))
                                                                              ((y ls) (Expr y ls)))
                                                                  (let* ((lls (filter (lambda(x) (not (memq (car x) ls))) lls)))
                                                                    (values `(letrec (,x ...) (closures ,lls ,y)) ls))))
        ((quote ,x) (values exp ls))
        ((,x ,y ...) (guard (prim? x)) (let-values (((y ls) (Expr* y ls)))
                                         (values `(,x ,y ...) ls)))
        ;; Replace calls present in wkt
        ((,x ,y ,z ...) (guard (and (label? x) (uvar? y))) (let*-values (((z ls) (Expr* z ls)))
                                                             (if (memq y ls)             
                                                                 (values `(,x ,z ...) ls)
                                                                 (values `(,x ,y ,z ...) ls))))
        ;; This probably will never happen
        ((,x ...) (let-values (((x ls) (Expr* x ls)))
                    (values `(,x ...) ls)))
        (,x (guard (uvar? x)) (values x ls))
        (,else (values else ls))))
           
    (define (Program exp)                   ;get-trace-define
      (let-values (((exp ls) (Expr exp '())))
        exp))

    (define (optimize-free exp)                   ;get-trace-define
      (Program exp))
    
    (optimize-free program)))
