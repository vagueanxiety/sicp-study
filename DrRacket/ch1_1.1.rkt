#lang sicp
(define x 2)
(define y (+ x 1))

(* 2 x)

(identity y)  ; y is still 2 because the definition is not sticky


(define z y)
(identity z)
(= z y)
(= x y)