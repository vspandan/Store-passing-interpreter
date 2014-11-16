#lang racket

;;; Primitive Procedures
;;; --------------------


(provide
  +p -p *p /p <p <=p eq?p 0?p)

(require "semantic-domains.ss")

(define nonzero? (and/c number? (not/c zero?)))


(define +p
  (prim-proc +
    (list number? number? number?)))

(define -p
  (prim-proc -
    (list number? number? number?)))

(define *p
  (prim-proc *
    (list number? number? number?)))

(define /p
  (prim-proc /
    (list number? number? nonzero?)))

(define <p
  (prim-proc  <
    (list boolean? number? number?)))

(define <=p
  (prim-proc   <=
    (list boolean? number? number?)))

(define eq?p
  (prim-proc eq?
    (list boolean? expressible-value? expressible-value?)))

(define 0?p
  (prim-proc zero?
    (list boolean? number?)))

