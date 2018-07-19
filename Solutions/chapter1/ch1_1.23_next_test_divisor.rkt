#lang sicp

; helper functions
(define (even? x) 
    (= 0 (remainder x 2))
)

(define (divides? x d)
    (= 0 (remainder x d))
)


(define (square x) (* x x))




; prime?
(define (smallest-divisor x)
    (find-divisor x 2)
)

(define (find-divisor x d)
    (cond
        ((> (square d) x) x)
        ((divides? x d) d)
        (else (find-divisor x (next d)))
    )
)

(define (prime? x)
    (= x (smallest-divisor x))
)


(define (next d)
    (if (= d 2) 3 (+ d 2))
)

; timing

(define (timed-prime-test n) 
    (newline)
    (display n)
    (start-prime-test n (runtime))
)


(define (start-prime-test n start-time) 
    (if (prime? n)
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
            (if (prime? l) (search-for-primes (- m 1) (+ 2 l))   ; a sad face for prime?
                (search-for-primes m (+ 2 l))                   
            )
            
        )
    )
)




;testing
(timed-prime-test 4)
(timed-prime-test 19)
(timed-prime-test 199)
(timed-prime-test 1999)
(timed-prime-test 19999)

(display "\n\n")


(search-for-primes 3 1000)
(search-for-primes 3 10000)
(search-for-primes 3 100000)
(search-for-primes 3 1000000)


; results
; 4
; 19 *** 21
; 199 *** 2
; 1999 *** 3
; 19999


; 1001
; 1003
; 1005
; 1007
; 1009 *** 2
; 1011
; 1013 *** 2
; 1015
; 1017
; 1019 *** 4
; Complete!


; 10001
; 10003
; 10005
; 10007 *** 5
; 10009 *** 4
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
; 10037 *** 5
; Complete!


; 100001
; 100003 *** 13
; 100005
; 100007
; 100009
; 100011
; 100013
; 100015
; 100017
; 100019 *** 12
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
; 100043 *** 13
; Complete!


; 1000001
; 1000003 *** 39
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
; 1000033 *** 39
; 1000035
; 1000037 *** 39
; Complete!

; the run time is more than a half of the orignal one. It is probably because adding a if 
; statement reduces the benefit of testing less divisors.