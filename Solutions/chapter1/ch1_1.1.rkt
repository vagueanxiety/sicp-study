#lang sicp
(define x 2)
(define y (+ x 1))

(set! x 4)

(identity y)  ; y is still 3 because the definition is not sticky
(identity x) ; 4

(define z y) ; assign the value of y to z

(identity z) ; 3
(= z y)
(= x y)