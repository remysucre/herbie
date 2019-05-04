#lang racket

(require math/bigfloat)
(require "../common.rkt" "../bigcomplex.rkt")

(provide types type? value? bigvalue?)

(define types '(bool real complex rplan attr matrix))
(define (type? x) (set-member? types x))

(define/match (value-of type) [('bool) boolean?] [('real) real?] [('complex) complex?] [('rplan) (const #f)][('matrix) (const #f)][('attr) (const #f)])
(define/match (bigvalue-of type) [('bool) boolean?] [('real) bigfloat?] [('complex) bigcomplex?] [('rplan) (const #f)][('matrix) (const #f)][('attr) (const #f)])

(define value? (apply or/c (map value-of types)))
(define bigvalue? (apply or/c (map bigvalue-of types)))

