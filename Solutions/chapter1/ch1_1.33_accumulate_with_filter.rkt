#lang sicp

; accumulator
(define (recursive-filtered-accumulate combiner null-value term a next b filter)
    (if (> a b) null-value
        (combiner (if (filter a)
                      (term a)
                      null-value) 
                (recursive-filtered-accumulate combiner null-value term (next a) next b filter))
    )
)


(define (iterative-filtered-accumulate combiner null-value term a next b filter)
    (define (iter a result)
        (if (> a b) result
            (iter (next a) (combiner (if (filter a)
                                            (term a)
                                            null-value)
                                      result))
        )
    ) 
    (iter a null-value)
)



; helper functions
(define (even? x) 
    (= 0 (remainder x 2))
)

(define (divides? x d)
    (= 0 (remainder x d))
)

(define (square x) (* x x))


(define (gcd a b) (if (= b 0)
      a
      (gcd b (remainder a b))))

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

(define (next d)
    (if (= d 2) 3 (+ d 2))
)

(define (prime? x)
    (= x (smallest-divisor x))
)


(define (identity z) z)

(define (iterative-sumOfSqOfPrime a b)
    (iterative-filtered-accumulate + 0 square a inc b prime?)
     ; prime? can be strictter so that this procedure can tolerate some invalid inputs
)

(define (recursive-sumOfSqOfPrime a b)
    (recursive-filtered-accumulate + 0 square a inc b prime?)
)

(define (iterative-productOfPositivePrimeInt n)

    (define (my-filter x)
        (= 1 (gcd x n))
    )

   (iterative-filtered-accumulate * 1 identity 1 inc n my-filter) 
 
)


(define (recursive-productOfPositivePrimeInt n)
    (define (my-filter x)
        (= 1 (gcd x n))
    )

    (recursive-filtered-accumulate * 1 identity 1 inc n my-filter) 
)


; testing
; sum of squares of prime numbers from a to b
(iterative-sumOfSqOfPrime 2 10)
(recursive-sumOfSqOfPrime 2 10)

; product of all positive integers that are less than n and relative prime to n
(iterative-productOfPositivePrimeInt 10)
(rec-productOfPositivePrimeInt 10)

; 1 * 3 * 7 * 9 = 189

; takeaways:
; 1. number of params can be reduced by internalizing procedures

