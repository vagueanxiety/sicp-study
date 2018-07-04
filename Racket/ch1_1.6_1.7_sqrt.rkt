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



(define (new-sqrt x)
  (new-sqrt-iter 1.0 10000.0 x)
  )

(define (new-good-enough? newGuess oldGuess)
  (= newGuess oldGuess))
    ; when the machine precison is hit, old guess and newguess become the same => the best guess the machine can get
    ; this solution is from GWB's comment in http://community.schemewiki.org/?sicp-ex-1.7


(define (new-sqrt-iter newGuess oldGuess x)
  (if (new-good-enough? newGuess oldGuess)
      newGuess
      (new-sqrt-iter (improve newGuess x) newGuess x)
      )
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
;(sqrt 4)
;(sqrt 0.00000004)
;(sqrt 152451495765625) ; expected: 12347125
;(sqrt 10000000000000)

(new-sqrt 0.00000004)
(new-sqrt 0.0000000004)
(new-sqrt 152451495765625)
(new-sqrt 10000000000000)
(new-sqrt 100000000)