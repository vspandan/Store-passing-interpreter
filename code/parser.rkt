#lang racket

;;; =================================
;;; Parser for the RECURSION language
;;; =================================

;;; Concrete Syntax

;;; <exp> :=  <num> |
;;;           <bool>
;;;           <id> |
;;;           (ifte <exp> <exp> <exp>) |
;;;           (function (<id> ...) <exp) |
;;;           (assume ([<id> <exp>] ...) <exp>) |
;;;           (assume-rec ([<id> <exp>] ...) <exp>) |
;;;           (set <id> <exp>) |
;;;           (seq <exp> ...)  |
;;;           (<exp> <exp> ...)

;;; example concrete syntax

;;; (assume-rec ([even? (function (n) (if (0? n) #t (odd? (sub1 n))))]
;;;             [odd?  (function (n) (if (0? n) #f (even? (sub1 n))))])
;;;    (even? 5))

(require racket/match)

(require "ast.rkt")

(provide
  parse)

(define *keywords*
  '(ifte function assume assume-rec seq set))

(define id?
  (lambda (x)
    (and
     (symbol? x)
     (not (memq x *keywords*)))))
         

(define parse
  (lambda (d)
    (match d
     [(? number? n) (number n)]
     [(? boolean? b) (boolean b)]
     [(? id? x) (id-ref x)]
     [(list 'ifte a b c)  (ifte (parse a) (parse b) (parse c))]

     [(list
       'function
       (list (? id? x) ...)
       body)
      (function x (parse body))]
     
     [(list 'assume
        (list (list (? id? x) e) ...) body)
      (let* ([a (map parse e)]
             [b (map make-bind x a)])
        (assume b (parse body)))]
     
     [(list 'assume-rec
        (list (list (? id? x) e) ...) body)
      (let* ([a (map parse e)]
             [b (map make-bind x a)])
        (assume-rec b (parse body)))]
     
     [(list 'set (? id? x) e)
      (set x (parse e))]
     
     [(list 'seq stmts ...)
      (seq (map parse stmts))]
     
     [(list rator rands ...)
      (let* ([rator (parse rator)]
             [rands (map parse rands)])
        (app rator rands))]
     [_ (error 'parse "don't know how to parse ~a" d)])))



;;; Unit Testing
;;; ============
(require rackunit)


(check-equal? (parse 4) (number 4) "parse-number")
(check-equal? (parse #t) (boolean #t) "parse-boolean")
(check-equal? (parse 'x) (id-ref 'x) "parse-id")

(check-equal?
 (parse '(ifte 3 4 8))
 (ifte (number 3) (number 4) (number 8))
 "parse-ifte")


(check-equal?
 (parse '(function (x y) 4))
 (function '(x y) (number 4))
 "parse-function")


(check-equal?
  (parse '(assume ([x 3]) 6))
  (assume (list (make-bind 'x (number 3))) (number 6))
  "parse-assume")


(check-equal?
  (parse '(assume-rec ([f (function (x y) x)]
                       [g (function (m n) 5)])
            9))
  (assume-rec
    (list
      (make-bind 'f (function '(x y) (id-ref 'x)))
      (make-bind 'g (function '(m n) (number 5))))
    (number 9))
  "parse-assume-rec")


(check-equal?
  (parse '(assume-rec () 9))
  (assume-rec
    (list)
    (number 9))
  "parse-empty-assume-rec")

(check-equal?
  (parse '(x y))
  (app (id-ref 'x)
       (list (id-ref 'y)))
  "parse-app")

(check-equal?
  (parse '(set x 5))
  (set 'x (number 5))
  "parse-set")

(check-equal?
   (parse '(seq (set x 5) x))
   (seq (list (set 'x (number 5)) (id-ref 'x)))
   "parse-seq")


(check-exn exn? (lambda () (parse "hello")) "parse-string-error")
(check-exn exn? (lambda () (parse '#(1 2))) "parse-vector-error")
(check-exn exn? (lambda () (parse '(1 . 2)) "parse-cons-error"))
(check-exn exn? (lambda () (parse '()) "parse-empty-error"))
(check-exn exn? (lambda () (parse '(set 4 5))) "parse-set-error")


