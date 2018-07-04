#lang sicp


(define (new-cubert x)
  (new-cubert-iter 1.0 10000.0 x)
  )

(define (new-good-enough? newGuess oldGuess)
  (= oldGuess newGuess)
  )


(define (new-cubert-iter newGuess oldGuess x)
  (if (new-good-enough? newGuess oldGuess)
      newGuess
      (new-cubert-iter (improve newGuess x) newGuess x)
      )
  )



(define (improve guess x)
  (/ 
    (+ 
      (/ x (square guess))
      (* 2 guess)
    ) 
    3)
  )

(define (average x y)
  (/ (+ x y) 2))


(define (square x)
  (* x x))

(define (abs x)
  (cond ((> x 0) x)
        (else (- x)))
  )
(new-cubert -27)
(new-cubert 27)
(new-cubert 0.000000000000027)
(new-cubert 15625000000)
(new-cubert -8)
(new-cubert -1)
