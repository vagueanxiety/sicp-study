#lang sicp

; helper functions
(define (even? x) 
    (= 0 (remainder x 2))
)

(define (divides? x d)
    (= 0 (remainder x d))
)

(define (square x) (* x x))



; prime? using fermat method
; randomly chose a number a less than n and check if its nth power
; is congruent to a modulo n
; this procedure only carries out one test

(define (fermat-test-iter n a count)
    (define (pass?)
        (= (expmod a n n) a)
    )

    (cond 
        ( (or (= count 0) (>= a n) (<= a 0)) true)
        ((pass?) (fermat-test-iter n (+ a 1) (- count 1)))
        (else false)
    )
)
; raise base to exponent and comppute its modulo
(define (expmod base exponent modulo)
    (cond 
        ((= exponent 0) 1) ; assuming modulo is greater than one
        ((even? exponent) 
            (remainder 
                (square (expmod base (/ exponent 2) modulo ))
                modulo
            )
        )
        ( else  
            (remainder 
                (* base (expmod base (- exponent 1) modulo ))
                modulo
            )
        )
    )
)

; x is a prime if all n < x pass the test
(define (fast-prime? x)
    (fermat-test-iter x 1 (- x 1))
)



(fast-prime? 2)
(fast-prime? 9)
(fast-prime? 19)
(fast-prime? 199)
(fast-prime? 1999)
(fast-prime? 19999)
(fast-prime? 199999)

(newline)
(fast-prime? 561)
(fast-prime? 1105)
(fast-prime? 1729)
(fast-prime? 2465)
(fast-prime? 2821)
(fast-prime? 6601)
