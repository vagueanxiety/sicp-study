#lang sicp

(define (smallest-divisor x)
    (find-divisor x 2)
)

(define (find-divisor x d)
    (cond
        ((> (square d) x) x)
        ((divides? x d) d)
        (else (find-divisor x (+ 1 d)))
    )

)


(define (divides? x d)
    (= 0 (remainder x d))
)

 (define (square x) (* x x))

;testing

(smallest-divisor 20)
(smallest-divisor 9)

 (smallest-divisor 199) 
 (smallest-divisor 1999) 
 (smallest-divisor 19999) 

