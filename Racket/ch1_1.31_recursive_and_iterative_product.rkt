#lang sicp

(define (recursive-product term a next b)
    (if (> a b) 1
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

(define (identity x) x)

(define (recursive-factorial x)
    (if (= x 0) 1
        (recursive-product identity 1 inc x)
    )
    
)

(define (iterative-factorial x)
    (if (= x 0) 1
        (iterative-product identity 1 inc x)
    )
)



; function that computes the product term
(define (f n)
    (if (even? n)
        (/ (+ n 2.0) (+ 1 n))
        (/ (+ 1.0 n) (+ n 2))
    )
)

(define (even? x) 
    (= 0 (remainder x 2))
)

(define (recursive-pi n)
    (* 4 (recursive-product f 1 inc n))
)


(define (iterative-pi n)
    (* 4 (iterative-product f 1 inc n))
)


;testing

(display "\nrecursive factorial: \n")

(recursive-factorial 5)
(recursive-factorial 1)
(recursive-factorial 4)
(recursive-factorial 0)

(display "\n\niterative factorial: \n")


(iterative-factorial 5)
(iterative-factorial 1)
(iterative-factorial 4)
(iterative-factorial 0)


(display "\n\nrecursive pi: \n")
(recursive-pi 10)
(recursive-pi 1000)
(recursive-pi 100000)
(recursive-pi 1000000)

(display "\n\niterative pi: \n")
(iterative-pi 10)
(iterative-pi 1000)
(iterative-pi 100000)
(iterative-pi 1000000)