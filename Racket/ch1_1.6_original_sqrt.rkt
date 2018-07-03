#lang sicp


(define (sqrt x)
  (sqrt-iter 1.0 x)
  )

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)
      )
  )


(define (good-enough? guess x)
  (< (abs(- (square guess) x)) 0.00000001)
  )


(define (improve guess x)
  (average guess (/ x guess))
  )

(define (average x y)
  (/ (+ x y) 2))


(define (square x)
  (* x x))

(define (abs x)
  (cond ((> x 0) x)
        (else (- x)))
  )

(square 10)
(square -10)
(abs 10)
(abs -10)
(abs 0)
(average 0 1.0)
(average -1 1)
(sqrt 4)
(sqrt 1024)

