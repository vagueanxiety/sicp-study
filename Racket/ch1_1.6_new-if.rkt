#lang sicp


(define (sqrt x)
  (sqrt-iter 1.0 x)
  )

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)
      )
  )


(define (good-enough? guess x)
  (< (abs(- (square guess) x)) 0.00001)
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


; new if implementation
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause))
)

; testing new-if 
(new-if (< 4 0) 7 8)
(new-if (< 4 0) 7 10)

; testing new sqrt
(sqrt 4)
(sqrt 5)