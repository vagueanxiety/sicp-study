#lang sicp

; helper functions
(define (even? x) 
    (= 0 (remainder x 2))
)

(define (divides? x d)
    (= 0 (remainder x d))
)

(define (square x) (* x x))


(define (test-iter n a count)
    (define (pass?)
        (not (= (miller_rabin_expmod a n n) 0))
    )

    (cond 
        ( (or (= count 0) (>= a n) (<= a 0)) true)
        ((pass?) (test-iter n (+ a 1) (- count 1)))
        (else false)
    )
)


; raise base to exponent and compute its modulo
; return 0 if a “nontrivial square root of 1 modulo n” is found
(define (miller_rabin_expmod base exponent modulo)


    (define (remainder-with-checking sqrt)

        (define (check-sqrt)
            (not (or (= sqrt (- modulo 1)) (= sqrt 1)))
        )

        (define (check-sq)
            (= (remainder (square sqrt) modulo) 1)
        )

        (cond 
            ((= sqrt 0) 0) ; 0 is used to signal 
            ((and (check-sq) (check-sqrt)) 0) ; the sqrt is found 
            (else (remainder (square sqrt) modulo)) ; normal case
        )
    )

    ; body 
    (cond 
        ((= exponent 0) 1) ; assuming modulo is greater than one
        ((even? exponent) 
            ; signal the nontrvial sqrt of 1 mod n
            ( remainder-with-checking (miller_rabin_expmod base (/ exponent 2) modulo) )
        )
        ( else  
            (remainder 
                (* base (miller_rabin_expmod base (- exponent 1) modulo ))
                modulo
            )
        )
    )

)

; check at least half of a < n and determine if it is a prime
(define (miller-rabin-fast-prime? x)
    (test-iter x 1 (if (even? x) (/ x 2)
                          (/ (+ x 1) 2) )
    )
)

(miller-rabin-fast-prime? 1999)
(miller-rabin-fast-prime? 19999)
(miller-rabin-fast-prime? 9)
(miller-rabin-fast-prime? 19)
(miller-rabin-fast-prime? 2)
(miller-rabin-fast-prime? 3)
(newline)
(miller-rabin-fast-prime? 21)
(miller-rabin-fast-prime? 24)x
(miller-rabin-fast-prime? 4)
(miller-rabin-fast-prime? 17)

(newline)
(miller-rabin-fast-prime? 561)
(miller-rabin-fast-prime? 1105)
(miller-rabin-fast-prime? 1729)
(miller-rabin-fast-prime? 2465)
(miller-rabin-fast-prime? 2821)
(miller-rabin-fast-prime? 6601)



; takeaways:

; 1. Sometimes, instead of creating new variables, think about changing the
; primitive procedure so that checking is done **within** the procedure
; and the result can be signaled by its return value

; 2.