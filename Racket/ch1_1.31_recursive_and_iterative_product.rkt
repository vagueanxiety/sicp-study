#lang sicp

(define (recursive-product term a next b)
    (if (> a b) 0
        (* (term a) (recursive-product term (next a) next b))
    )
)

(define (iterative-product term a next b) 
    (define (iter a result)
        (if (> a b) result
            (iter (next a) (* (term a) result))
        )
    ) 
    (iter a 1)
)

(define (identity x)
    (x)
)

(define (recursive-factorial x)
    (recursive-product identity 1 inc x)
)

(define (iterative-factorial x)
    (iterative-product identity 1 inc x)
)

;testing
;