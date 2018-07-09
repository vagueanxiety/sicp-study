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
(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a)
    )

    (try-it ( + 1 (random (- n 1))) ) 
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

; x is a prime if all n randomly chosen numbers pass the test
(define (fast-prime? x n)
    (cond 
        ((= n 0) true)
        ((fermat-test x)(fast-prime? x (- n 1)))
        (else false)
    )
)




(fast-prime? 561 100)
(fast-prime? 1105 100)
(fast-prime? 1729 100)
(fast-prime? 2465 100)
(fast-prime? 2821 100)
(fast-prime? 6601 100)