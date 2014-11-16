#lang racket

;;; ==================================================================
;;; Abstract Syntax for the STORE-PASSING/IMPLICIT ALLOCATION language
;;; ==================================================================



;;; <ast> ::= <num-ast> |
;;;           <bool-ast> |
;;;           <id-ref-ast> |
;;;           <assume-ast> |
;;;           <ifte-ast>   |  
;;;           <rec-ast> |
;;;           <function-ast> |
;;;           <app-ast> |

;;;           <set-ast> |
;;;           <seq-ast>

;;; <num-ast>        ::= (number <number>)
;;; <bool-ast>       ::= (boolean <boolean>)
;;; <function-ast>   ::= (function (<id> ... ) <ast>)
;;; <app-ast>        ::= (app  <ast>  <ast> ...)
;;; <assume-ast>     ::= (assume (<bind> ...) <ast>)
;;; <bind-ast>       ::= (<id> <ast>)
;;; <id-ref-ast>     ::= (id-ref <id>)
;;; <id>             ::= <symbol>
;;; <ifte-ast>       ::= (ifte <ast> <ast> <ast>)
;;; <assume-rec-ast> ::= (assume-rec (<bind> ...) <ast>)

;;; <set-ast>        ::= (set <id> <ast>)
;;; <seq-ast>        ::= (seq <ast> <ast> ...)


(require eopl/eopl)

(provide
  ast
  ast?
  number
  boolean
  id-ref
  ifte
  assume
  assume-rec
  make-bind
  bind-id
  bind-ast
  function
  app
  set
  seq
  )

(define-datatype ast ast?
  [number (datum number?)]
  [boolean (datum boolean?)]
  [id-ref (sym id?)]
  [ifte (test ast?) (then ast?) (else-ast ast?)]
  [assume (binds  (list-of bind?)) (body ast?)]
  [assume-rec (binds (list-of bind?)) (body ast?)]
  [function (formals (list-of id?)) (body ast?)]
  [app (rator ast?) (rands (list-of ast?))]
  [set (lhs id?) (rhs ast?)]
  [seq (stmts (list-of ast?))])

(define-datatype bind bind?
  [make-bind (b-id id?) (b-ast ast?)])

;;; bind-id : bind? -> id?
(define bind-id
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-id])))

;;; bind-ast : bind? -> ast?
(define bind-ast
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-ast])))

(define-datatype fbind fbind?
  [make-fbind (fb-id id?)
              (fb-formals (list-of id?))
              (fb-body ast?)])

(define id? symbol?)

;;; unit Testing
;;; ============

;;; Racket's unit testing framework
(require rackunit)


(define-simple-check
  (check-ast? thing)
  (ast? thing))

(check-ast? (number 5) "number-5 test")
(check-ast? (boolean #t) "boolean-#t test")
(check-ast? (id-ref 'x) "id-ref-x test")
(check-ast? (function
              '(x y z)
              (app (id-ref '+)
                (list (id-ref 'x)
                  (app (id-ref '*)
                    (list (id-ref 'y) (id-ref 'z)))))) "function-test")


(check-ast?
  (app (id-ref '+)
    (list (number 5) (number 6))) "app test")


(check-ast?
  (assume (list (make-bind 'x (number 5))
            (make-bind 'y (number 6)))
    (app (id-ref '+)
      (list (id-ref 'x) (id-ref 'y)))) "assume-test")



;;; A feasible concrete syntax for recursive:
;;; (recursive ([even? (n) (if (0? n) #t (odd? (- n 1)))]
;;;             [odd?  (n) (if (0? n) #f (even? (- n 1)))])
;;;    (even? 5))

(check-ast?
  (assume-rec
   (list
    (make-bind 'even?
      (make-function '(n)
        (ifte (app (id-ref '0?) (list (id-ref 'n)))
          (boolean #t)
          (app (id-ref 'odd?)
            (list (app (id-ref '-) (list (id-ref 'n) (number 1))))))))

    (make-bind 'odd?
      (make-function '(n)
        (ifte (app (id-ref '0?) (list (id-ref 'n)))
          (boolean #f)
          (app (id-ref 'even?)
            (list (app (id-ref '-) (list (id-ref 'n) (number 1)))))))))
   (app (id-ref 'even?) (list (number 3))))
   "recursive-ast test")


(check-ast?
  (assume (list (make-bind 'x (number 5)))
    (seq (list (set 'x (number 3))
               (id-ref 'x)))))




