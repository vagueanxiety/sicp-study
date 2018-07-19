#lang sicp

;repeated
(define (even? x) 
    (= 0 (remainder x 2))
)

(define (square x)
(* x x)
)

(define (compose func1 func2)
    (lambda (x) (func1 (func2 x))))

(define (repeated f n)
    (define (compose-iter g count)
        (cond ((= count n) g)
            ((< (* 2 count) n) (compose-iter (compose g g) (* count 2)) )
            (else (compose-iter (compose f g) (+ count 1))))
    )
    (compose-iter f 1)
)

; fixed point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance)) 
    (define (try guess)
        ; (display guess)
        ; (newline)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

; average damp
(define (average-damp f)
(lambda (x) (average x (f x))))
(define (average x y)
  (/ (+ x y) 2))


; testing nth root and their corresponding number of average damp applied
(define (n-rt-with-m-average-damp x n m)
    (fixed-point ((repeated average-damp m) (lambda (y) (/ x (expt y (- n 1)))))
    1.0))


;after experiments
(define (nubmerOfAverageDamp n)
        (cond ((= n 1) 1)
            (else (ceiling (- (* 1.443 (log (+ n 1))) 1))))
)
; other clever ways to calculte the number of AD
;http://community.schemewiki.org/?sicp-ex-1.45

(define (n-rt x n)
    (fixed-point ((repeated average-damp (nubmerOfAverageDamp n)) (lambda (y) (/ x (expt y (- n 1)))))
    1.0))

; testing and experiments
(display "\nn-rt-with-m-average-damp\n")

(newline)
(n-rt-with-m-average-damp 5 1 1)
(n-rt-with-m-average-damp 16 2 1) 
(n-rt-with-m-average-damp 27 3 1) 


(newline)

(n-rt-with-m-average-damp 81 4 2) 
(n-rt-with-m-average-damp 243 5 2)
(n-rt-with-m-average-damp 729 6 2)
(n-rt-with-m-average-damp 2187 7 2)

(newline)


(n-rt-with-m-average-damp 6561 8 3)
(n-rt-with-m-average-damp 19683 9 3)
(n-rt-with-m-average-damp 59049 10 3)
(n-rt-with-m-average-damp 177147 11 3)
(n-rt-with-m-average-damp 531441 12 3)
(n-rt-with-m-average-damp 1594323 13 3)
(n-rt-with-m-average-damp 4782969 14 3)
(n-rt-with-m-average-damp 14348907 15 3)

(newline)
(n-rt-with-m-average-damp 43046721 16 4)
(n-rt-with-m-average-damp 129140163 17 4)
(n-rt-with-m-average-damp 387420489 18 4)
;..
(n-rt-with-m-average-damp 617673396283947 31 4)

(newline)
(n-rt-with-m-average-damp 1.85302019e15 32 5)

;observations:
; # of cases covered: 1 2 4 8 16
; # of AD:            0 1 2 3 4
; expt 2 # of AD = # of cases covered
; define Sm to be the summation of geometric series = 2^0 + 2^1 + 2^3 + ... + 2^m
; then the number of AD required to solve nth root = the smallest m such that Sm >= n 

; the actual testing

(display "\nn-rt\n")
(n-rt 5 1)

(newline)
(n-rt 16 2) 
(n-rt 27 3) 


(newline)

(n-rt 81 4) 
(n-rt 243 5)
(n-rt 729 6)
(n-rt 2187 7)

(newline)


(n-rt 6561 8)
(n-rt 19683 9)
(n-rt 59049 10)
(n-rt 177147 11)
(n-rt 531441 12)
(n-rt 1594323 13)
(n-rt 4782969 14)
(n-rt 14348907 15)

(newline)
(n-rt 43046721 16)
(n-rt 129140163 17)
(n-rt 387420489 18)
;..
(n-rt 617673396283947 31)

(newline)
(n-rt 1.85302019e15 32)
