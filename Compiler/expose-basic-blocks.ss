;#!chezscheme
(library (Compiler expose-basic-blocks)
  (export
   expose-basic-blocks
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:)
    (Framework match)
    (Framework helpers)
    (Compiler common))
  
  ;; A variable is a either a register or a frame variable 
  (define (var? exp)                   ;get-trace-define
                (or (register? exp) (frame-var? exp) (uvar? exp)))  
  ;; extract-suffix name -> use this to enforce unique name
  ;; Using define-who macro 
  (define-who (expose-basic-blocks program)
    ;; An exp is divided into Program, Body,Tail, Effect, Var, Triv
    ;; Writing a function for each part
    ;; Trivial is Var | int | label  -- No int? so putting int64?        
     ;; Get a list to memq from label list
    (define (labelLs->suffx ls)
      (map (lambda(x) (string->number (extract-suffix (car x)))) ls))
    
    ;; Gets a unique name and checks if it is not in list
    ;; If name exists then it calls it again
    (define (get-unique-name ls c)
      (let ((n (unique-label c)))      
        (if (memq (string->number (extract-suffix n)) (labelLs->suffx ls))
            (begin
              (get-unique-name ls c))              
            n)))
        
    
    ;; Return - values list expr
    ;; expr is a jump statement or empty if nothing exists
    (define (create-label exp parser ls fls c)
      ;; if exp and fls are null then just return empty
      (if (and (null? exp) (null? fls))
          (values ls '())
          (let ((n (if (label? c) c (get-unique-name ls c))))
            (let-values (((list xp) (parser exp fls ls)))
              (values (cons (match xp
                              ((begin ,x ... ,y) `(,n (lambda() ,xp)))
                              ((if ,x ,y ,z) `(,n (lambda() ,xp)))
                              ((,x) `(,n (lambda() ,x)))
                              ((,x ...) `(,n (lambda() (begin ,x ...)))))
                            list) `((,n)))))))
    
   

   (define (Effect* exp-ls fls ls)
     (match exp-ls
       ((,x ... ,t) (let-values (((list exp) (Effect t fls ls)))
                      (Effect* x exp list)))
       (,else (values ls fls))))

   ;; Dummy - this list need no parsing but create requires one
   (define (JumpList exp fls ls)     
         (values ls exp))         

   ;;
   (define (Pred exp fls ls)
     (match exp
        ((true) (values ls (car fls)))
        ((false) (values ls (cadr fls)))
        ((if ,x ,y ,z) (let*-values (((yl yexp) (create-label y Pred ls fls 'tp))
                                     ((zl zexp) (create-label z Pred yl fls 'ep)))
                         (Pred x `(,yexp ,zexp) zl)))
        ((begin ,x ... ,t) (let-values (((list exp) (Pred t fls ls)))
                             (let-values (((elist exp) (Effect* x exp list)))
                               (values elist exp))))
        ((,x ,y ,z) (values ls `((if ,exp ,(car fls) ... ,(cadr fls) ...))))))

   
   ;; Takes in a list of Effects 
   ;; (values list exp)
   (define (Effect exp fls ls)                   ;get-trace-define
      (match exp
        ((begin ,x ... ,t) (let-values (((list exp) (Effect t fls ls)))
                             (let-values (((elist exp) (Effect* x exp list)))
                               (values elist `((begin ,exp ...))))))
        ;[(begin ,x ... ,t) `(begin ,x ... ,t)]
        [(nop) (values ls fls)]
        [(if ,x ,y ,z) (let*-values (((list exp) (create-label fls JumpList  ls '() 'j))
                                      ((yl yexp) (create-label y Effect list exp 't))
                                      ((zl zexp) (create-label z Effect yl exp 'e))
                                      ((xl xexp) (Pred x `(,yexp ,zexp) zl)))
                         (values xl xexp))]         
        ;;                    (values zl `(if ,x ,yexp ,zexp)))]        
        [(set! ,v (,b ,t1 ,t2)) (values ls `((set! ,v (,b ,t1 ,t2)) ,fls ...))]
        [(set! ,v ,t) (values ls `((set! ,v ,t) ,fls ...))]
        [(return-point ,x ,y) (let*-values (((ls exp) (create-label (make-begin fls) Effect  ls '() x))
                                            ((ls y) (Effect y fls ls)))
                                (values ls `(,(make-begin y))))]
        ((,x) (values ls `((,x))))
        ))
    ;; Validate Tail
    ;; Returns values list exp where the labelList car is passed on 
    (define (Tail exp fls ls)                   ;get-trace-define
      (match exp
        [(if ,x ,y ,z) (let*-values (((list exp) (create-label fls JumpList ls '() 'jt))
                                     ((yl yexp) (create-label y Tail list exp 'tt))
                                     ((zl zexp) (create-label z Tail yl exp 'et)))
                         (Pred x `(,yexp ,zexp) zl))]
                         ;(values zl `(if ,x ,yexp ,zexp)))]        
        ((begin ,x ... ,t) (let-values (((list exp) (Tail t fls ls)))
                             (let-values (((elist exp) (Effect* x exp list)))
                               (values elist `((begin ,exp ...))))))
        ((,x) (values ls `((,x))))))
    
    ;; Returns a list of labels - Add this expression to the list and return 
    (define (Exp exp ls)                   ;get-trace-define
      (match exp
        ((,x (lambda () ,y)) (let-values (((list exp) (Tail y '() ls)))
                               (cons `(,x (lambda () ,exp ...)) list))))) 

    ;; Validate Program
    (define (Program exp)                   ;get-trace-define
      (match exp        
        ((letrec (,x ...) ,y)
         (let ((label-list (fold-right (lambda(y s) (Exp y s)) '() x)))
           (let-values (((tlist texp) (Tail y '() label-list)))             
           `(letrec ,tlist ,texp ...))))))
    
    (define (expose-bb exp)                   ;get-trace-define
      (Program exp))
    (expose-bb program)))
