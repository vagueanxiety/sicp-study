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


; timing

(define (timed-prime-test n) 
    (newline)
    (display n)
    (start-prime-test n (runtime))
)


(define (start-prime-test n start-time) 
    (if (fast-prime? n 20)
    (report-prime (- (runtime) start-time)))
)


(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))



; search for m smallest prime numbers from odd integers greater than or equal to l
; @precondition: m >= 1; l >= 2

(define (search-for-primes m l)
    (cond 
        ((= m 0) (display "\nComplete!\n\n"))

        ((even? l)
            (search-for-primes m (+ 1 l)) 
        )

        (else 
            (timed-prime-test l)
            (if (fast-prime? l 20) (search-for-primes (- m 1) (+ 2 l))
                (search-for-primes m (+ 2 l))                   
            )
            
        )
    )
)




;testing
(timed-prime-test 5)
(timed-prime-test 19)
(timed-prime-test 199)
(timed-prime-test 1999)
(timed-prime-test 19999)

(display "\n\n")


(search-for-primes 3 1000)
(search-for-primes 3 10000)
(search-for-primes 3 100000)
(search-for-primes 3 1000000)



; results:
; 5 *** 341
; 19 *** 21
; 199 *** 32
; 1999 *** 42
; 19999


; 1001
; 1003
; 1005
; 1007
; 1009 *** 37
; 1011
; 1013 *** 39
; 1015
; 1017
; 1019 *** 39
; Complete!


; 10001
; 10003
; 10005
; 10007 *** 49
; 10009 *** 47
; 10011
; 10013
; 10015
; 10017
; 10019
; 10021
; 10023
; 10025
; 10027
; 10029
; 10031
; 10033
; 10035
; 10037 *** 48
; Complete!


; 100001
; 100003 *** 55
; 100005
; 100007
; 100009
; 100011
; 100013
; 100015
; 100017
; 100019 *** 56
; 100021
; 100023
; 100025
; 100027
; 100029
; 100031
; 100033
; 100035
; 100037
; 100039
; 100041
; 100043 *** 56
; Complete!


; 1000001
; 1000003 *** 61
; 1000005
; 1000007
; 1000009
; 1000011
; 1000013
; 1000015
; 1000017
; 1000019
; 1000021
; 1000023
; 1000025
; 1000027
; 1000029
; 1000031
; 1000033 *** 61
; 1000035
; 1000037 *** 63
; Complete!

; 10 times of input increases the run time by approximately 10 (a constant amount)