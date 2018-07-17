#lang sicp

(define k 100000)
(define (square x)
(* x x)
)


(define (recursive-cont-frac n d k)
    (define (recursive-iter n d index)
        (cond ((= index (+ k 1)) 0)
            (else(/ (n index) (+ (d index) (recursive-iter n d (+ index 1)))))
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



(define (iterative-tan-cf x k)
    (define (my-n index)
        (cond ((= index  1) x)
            (else (- (square x)))
        )
    )

    (iterative-cont-frac my-n (lambda (i) (- (* i 2.0) 1)) k)
)

(define (recursive-tan-cf x k)
    (define (my-n index)
        (cond ((= index  1) x)
            (else (- (square x)))
        )
    )

    (recursive-cont-frac my-n (lambda (i) (- (* i 2.0) 1)) k)
)

; testing by approximating tan x
(recursive-tan-cf 35 k) ;0.47381472041
(iterative-tan-cf 35 k)
(newline)
(recursive-tan-cf 0 k);0
(iterative-tan-cf 0 k)
(newline)
(recursive-tan-cf 100 k) ;-0.58721391515
(iterative-tan-cf 100 k)