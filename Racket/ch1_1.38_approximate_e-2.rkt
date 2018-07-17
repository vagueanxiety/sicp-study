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

(define (my-d index)
    (cond ((or (= index 1) (= index 2)) index)
        ((= 0 
            (remainder (- index 5) 3))
         (* 2 
            (+ 2 (/ (- index 5) 3))))
        (else 1))
)
; testing by approximating e − 2 = 0.71828182845
(recursive-cont-frac (lambda (i) 1.0) my-d k)
(iterative-cont-frac (lambda (i) 1.0) my-d k)