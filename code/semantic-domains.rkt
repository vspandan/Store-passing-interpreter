#lang racket

;;; =================================
;;; Semantic Domains and Environments
;;; =================================

(provide
  expressible-value?
  denotable-value?
  proc
  prim-proc
  closure
  proc?
  prim-proc?
  closure?
  rec-closure
  rec-closure?
  )

(require "env.rkt")

;;; Expressible Values (types of values returned by
;;; evaluating an ast)

;;; ========================================

;;; expressible-value ::=
;;;    number | boolean | proc

;;; Denotable Values (types of values denoted by
;;; identifiers)
;;; ============================================

;;; denotable-value ::=  Ref(expressible-value)
;;;

;;; expressible-value? is the set of things that are the
;;; results of evaluation of expressions (asts).


(require eopl/eopl)
(require "ast.rkt")
(require (prefix-in store: "store-list.rkt"))
(require "store-list.rkt")
;;; Procedure ADT
;;; ==============

(define-datatype proc proc? 
  [prim-proc (prim procedure?)
             (sig (list-of procedure?))]
  [closure  
   (formals (list-of symbol?)) 
   (body ast?) 
   (env env?)]
  [rec-closure
    (formals (list-of symbol?))
    (body ast?)])



;;; Subtype Predicates
;;; ==================

;;; prim-proc? : proc? -> boolean?
(define prim-proc?
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #t]
      [else #f])))



;;; closure? : proc? -> boolean?
(define closure? 
  (lambda (p) 
    (cases proc p
      [closure (formal body env) #t]
      [else #f])))


;;; rec-closure? : proc? -> boolean?
(define rec-closure? 
  (lambda (p)
    (cases proc p
      [rec-closure (formals body) #t]
      [else #f])))


;;; expressible-value? : any/c -> boolean?
(define expressible-value? 
  (lambda(ex)
    (or (number? ex) 
        (boolean? ex) 
        (proc? ex))))

;;; denotable-value? :any/c -> boolean?
(define denotable-value?
  (lambda(ex)
    (ref? ex)))



