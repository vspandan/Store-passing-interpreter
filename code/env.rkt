#lang racket

;;; =================================
;;; Semantic Domains and Environments
;;; =================================

(provide
  env
  env?
  empty-env
  extended-env
  lookup-env  
  )


;;; Expressible Values (types of values returned by
;;; evaluating an ast)


(require eopl/eopl)
(require (only-in "store-list.rkt" [ref? store:ref?]))
(define-datatype env env?
  [empty-env]
  [extended-env
    (syms (list-of symbol?))
    (vals (list-of store:ref?))
    (outer-env env?)])

;;; Subtype Predicates
;;; ==================

;;; empty-env? : env? -> boolean?
(define empty-env?
  (lambda (e)
    (cases env e
      [empty-env () #t]
      [else #f])))

;;; extended-env? : env? -> boolean?
(define extended-env?
  (lambda (e)
    (cases env e
      [extended-env (syms vals outer-env) #t]
      [else #f])))

;;; Returns the loction of the element in a list, -1 if the
;;; element is absent.

;;; list-index : [(listof any/c)  any/c] -> 
(define list-index
  (lambda (ls a)
    (letrec ([loop
               (lambda (ls ans)
                 (cond
                   [(null? ls) -1]
                   [(eq? (first ls) a) ans]
                   [#t (loop (rest ls) (+ 1 ans))]))])
      (loop ls 0))))

;;; lookup-env: [env?  symbol?] -> any/c
;;; lookup-env: throws "unbound identifier" error
(define lookup-env
  (lambda (e x)
    (cases env e
      [empty-env ()
        (error
          'lookup-env
          "unbound identifier ~a" x)]
      [extended-env (syms vals outer-env)
        (let ([j (list-index syms x)])
          (cond
            [(= j -1) (lookup-env outer-env x)]
            [#t (list-ref vals j)]))])))


;;; Unit testing
;;; ============
(require rackunit)

(check-pred env? (empty-env) "env?-empty-env")
(check-pred empty-env? (empty-env) "empty-env?-empty-env")
(check-exn exn? (lambda () (lookup-env (empty-env) 'a)) "lookup-empty-env-a")

(define e1
  (extended-env '(x y z) '(1 2 3) (empty-env)))

(check-pred env?  e1 "env?-extended-env")
(check-pred extended-env? e1 "extended-env?-extended-env")

(check-equal? 1 (lookup-env e1 'x) "lookup-e1-x")
(check-equal? 2 (lookup-env e1 'y) "lookup-e1-y")
(check-exn exn? (lambda () (lookup-env e1 'a)) "lookup-e1-a")

(define e2
  (extended-env '(w x) '(5 6) e1))

(check-equal? 5 (lookup-env e2 'w) "lookup-e2-w")
(check-equal? 6 (lookup-env e2 'x) "lookup-e2-x")
(check-equal? 2 (lookup-env e2 'y) "lookup-e2-y")
(check-equal? 3 (lookup-env e2 'z) "lookup-e2-z")
(check-exn exn? (lambda () (lookup-env e2 'a)) "lookup-e2-a")




