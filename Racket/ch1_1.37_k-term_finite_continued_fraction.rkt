#lang sicp

(define k 100000)


(define (recursive-cont-frac n d k)
    (define (recursive-iter n d index)
        (cond ((= index (+ k 1)) 0)
            (else (/ (n index) (+ (d index) (recursive-iter n d (+ index 1)))))
        )
    )
    (recursive-iter n d 1)
)


(define (iterative-cont-frac n d k)
    (define (iterative-iter count quotient)
        (cond ((= count 0) quotient)
            (else (iterative-iter (- count 1) 
                                  (/ (n count) (+ (d count) quotient))
                )))
    )
    (iterative-iter k 0)
)


; testing by approximating 1/phi = 0.61803398875
(recursive-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)
(iterative-cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)