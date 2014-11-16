#lang racket
(require "ast.rkt")
(require "init-store-env.rkt")
(require "semantic-domains.rkt")
(require "eval-ast.rkt")
(require "top.rkt")
(require "run.rkt")
(require rackunit)
(check-equal? (go '(assume-rec ([fact (function (n) (ifte (0? n) 1 (* n (fact (- n 1)))))])
         (fact 4)))  24 "first testCase")
    
(check-equal? (go '(assume-rec ([fibo (function (n) (ifte (< n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))])
         (fibo 3))) 3 "second testCase")

(check-equal? (go '(assume-rec ([f (function (n) (ifte (0? n) 1 (* n (f (- n 1)))))]
                  [fib (function (n) (ifte (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))]
                  [addfactfib (function (n) (+ (f n) (fib n)))])
                  (addfactfib 3)))  9 "third testcase" )

(check-equal? (go '(assume ([fib (function (n) (* n n))]) 
             (assume ([fib (function (n) (ifte (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))])
                     (fib 3)))) 5 "fourth testcase")

(check-equal? (go '(assume ([fib (function (n) (* n n))]) 
             (assume-rec ([fib (function (n) (ifte (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))])
                     (fib 8)))) 34 "fifth testcase" ) 


(check-equal?
(run 
 (assume-rec
  (list
(make-bind 'even?
(function '(n)
(ifte (app (id-ref '0?) (list (id-ref 'n)))
(boolean #t)
(app (id-ref 'odd?)
(list (app (id-ref '-) (list (id-ref 'n) (number 1))))))
))

(make-bind 'odd?
(function '(n)
(ifte (app (id-ref '0?) (list (id-ref 'n)))
(boolean #f)
(app (id-ref 'even?)
(list (app (id-ref '-) (list (id-ref 'n) (number 1))))))
)))
(app (id-ref 'even?) (list (number 3)))))
#f
"recursive-ast test")