#lang sicp 

; accumulator
(define (recursive-accumulate combiner null-value term a next b)
    (if (> a b) null-value
        (combiner (term a) (recursive-accumulate combiner null-value term (next a) next b))
    )
)


(define (iterative-accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b) result
            (iter (next a) (combiner (term a) result))
        )
    ) 
    (iter a null-value)
)



; sum

(define (iterative-sum term a next b) 
    (iterative-accumulate + 0 term a next b)
)

(define (recursive-sum term a next b)
    (recursive-accumulate + 0 term a next b)
)

; product


(define (recursive-product term a next b)
    (recursive-accumulate * 1 term a next b)
)

(define (iterative-product term a next b) 
    (iterative-accumulate * 1 term a next b)
)

; helper
(define (cube x) (* x x x))

; testing
(iterative-sum cube 0 inc 3)
(recursive-sum cube 0 inc 4)
(iterative-product cube 1 inc 3)
(recursive-product cube 1 inc 2)
