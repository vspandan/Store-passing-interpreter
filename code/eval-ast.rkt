#lang racket

;;; =============================================================
;;; Evaluator for the STORE-PASSING/IMPLICIT-ALLOCATION  language
;;; =============================================================
  
(provide
   eval-ast)
(require "ast.rkt")
(require "store-list.rkt")
(require "env.rkt")
(require "init-store-env.rkt")
(require "semantic-domains.rkt")
(require eopl/eopl)


;;; answer? = store? expressible-value?

;;; eval-ast : [ast? env? store?]-> answer?
;;; eval-ast :  throws error


(define eval-ast
  (lambda (a env store)
    (cases ast a
      [number (datum) (values store datum)]
      [boolean (datum) (values store datum)]
      [id-ref (id)  (values store (deref store  (lookup-env env id)))]
      [ifte (test then else) 
            (let-values ([(new-store b) (eval-ast test env store)])
              (if (boolean? b)
                  (eval-ast  (if b then else) env new-store)
                  (error 'eval-asts "wrong answer")
                  ))]
      [function (formals body)
                (values store (closure formals body env))]   
      [app (rator rand)
           (let*-values ([(new-store1 p) (eval-ast rator env store)]
                 [(new-store2 args) (eval-asts rand env new-store1)])
             (apply-proc p args new-store2 env))]
      [assume (binds body)
              (let*-values ([(ids) (map bind-id binds)]
                            [(asts) (map bind-ast binds)]
                            [(new-store1 vals) (eval-asts asts env store)])
                (let-values ([(new-store2 reference) (new-refs new-store1 vals)])
                  (eval-ast body (extended-env ids reference env) new-store2)))]
     
      [assume-rec (binds body) 
           (let* ([ids (map bind-id binds)]
                 [asts (map bind-ast binds)]
                 [vals (map (lambda(x)
                         (cases ast x
                           [function (formals body) 
                               (rec-closure formals body)]
                           [else (error 'parse "uninitialized variable ~a" x)]
                          )
                         ) asts)])
             (let*-values ([(p q) (new-refs store vals)])
                      (let ([new-env (extended-env ids q env)])
                        (eval-ast body new-env p)
                       ))
              )]
      
      [set (lhs rhs)
           (let-values ([(new-store1 res) (eval-ast rhs env store)])          
             (if (symbol? lhs) (values (setref new-store1 (lookup-env env lhs) res) res) 
                 (error 'eval-ast "wrong input")))]
      [seq (stmts)
           (let-values ([(new-store res) (eval-asts stmts env store)])
             (values new-store (last res)))]
      [else (error 'eval-ast "wrong input")]
      )))



                           
;;; eval-asts : [(listof ast?) env? store?] -> store? (listof expressible-value?)

(define eval-asts
  (lambda (asts env store)  
    (letrec
        ([loop (lambda (asts env store val)
                 (if (eq? (length asts) 0)
                     (values store val)
                     (let-values ([ (new-store result) (eval-ast (first asts) env store)])
                       (loop (rest asts) env new-store (append val (list result) )))))])
                  (loop asts env store '())  
                 )))
    
    ;(let-values ([(new-store res) (eval-ast (first asts) env store)])
     ; (if (> (length (rest asts)) 0)
     ;  (let-values ([(rest-store rest-res) (eval-asts (rest asts) env new-store)])
      ;   (values  rest-store  (append (list first) rest-res))) 
      ; (values new-store (list res))))))
  

      

;;; apply-proc :
;;;  [proc? (list-of expressible-value?) store?]
;;;    -> answer?

(define apply-proc
  (lambda (p args store env) 
    (cases proc p
      [prim-proc (prim sig)
        (values store (apply-prim-proc prim sig args))]
      
      [closure (formals body env)
        (apply-closure formals body env args store)]
      [rec-closure(formals body)
         (apply-closure formals body env args store)]
      )))



;;; apply-prim-proc :
;;;  [procedure? (listof procedure?)
;;;     (listof expressible-value?)] -> expressible-value?
;;;
;;; apply-prim-proc : throws error when number or type of
;;;     args do not match the signature of prim-proc

(define apply-prim-proc
  (lambda (prim sig args) 
    (cond
      [(and (= (- (length sig) 1) (length args))  
            (andmap match-arg-type (rest sig) args))
       (apply prim args)
       ]
      [else (error 'apply-prim-proc "unable to handle some cases")]
      )))

;;; match-arg-type : [procedure? any/c] -> boolean?
(define match-arg-type
    (lambda (arg-type val)
      (arg-type val)))


;;; apply-closure : [closure? (listof expressible-value?)]
;;;                  -> answer?

(define apply-closure
  (lambda (formals body env args store) 
    (let-values ([(new-store references)(new-refs  store args)])
      (eval-ast body (extended-env formals references env) new-store))))
      
   ; ( let ([new-env (extended-env formal args env)])
    ; (eval-ast body new-env new-store))))

                                        
                       
                     
;;; Unit testing
;;; ============

(require rackunit)

(define-simple-check
  (check-eval-ast? ast env store expected label)
  (let-values ([(store val) (eval-ast ast env store)])
    (check-equal? val expected label)))


(define s1-e1
  (make-init-store-env '(x y z) '(1 2 3)))

(define s1 (first s1-e1))
(define e1 (second s1-e1))

(check-eval-ast? (number 5)  e1  s1 5 "eval-ast: n5 test")
(check-eval-ast? (boolean #t)  e1  s1 #t "eval-ast: bt test")
(check-eval-ast? (id-ref 'x)  e1  s1 1 "eval-ast: id1 test")
(check-eval-ast? (id-ref 'y) e1  s1 2 "eval-ast: y test")


(check-eval-ast?
  (assume (list (make-bind 'x (number 4)))
    (id-ref 'x))
  e1
  s1
  4
  "eval-ast: new-ref1")


(check-eval-ast?
  (assume (list (make-bind 'x  (number 4)))
    (assume (list (make-bind 'y  (id-ref 'x)))
      (id-ref 'y)))
  e1
  s1
  4
  "eval-ast: new-ref2")

(check-eval-ast?
  (assume (list (make-bind 'x  (number 4)))
    (assume (list (make-bind 'ignore (set 'x (number 7))))
      (id-ref 'x)))
  e1
  s1
  7
  "eval-ast: new-ref3")


(check-eval-ast?
  (assume (list (make-bind 'x  (number 4)))
    (seq
      (list 
        (set 'x (number 7))
        (id-ref 'x))))
  e1
  s1
  7
  "eval-ast: new-ref3")

;; trying to set a non-reference
    
    (check-exn exn?
      (lambda ()
        (eval-ast 
          (assume (list (make-bind 'x  (number 4)))
            (set (number 7) (boolean #f)))
          e1
          s1)
        "eval-ast: error-set"))



(check-exn exn?
  (lambda ()
    (eval-ast
      (assume-rec (list (make-bind 'x) (id-ref 'x))
        (id-ref 'x))
      e1
      s1
      "eval-ast: uninitialized")))


