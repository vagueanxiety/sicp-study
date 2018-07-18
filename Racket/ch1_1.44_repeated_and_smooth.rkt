#lang sicp

; repeated
(define (even? x) 
    (= 0 (remainder x 2))
)

(define (square x)
(* x x)
)

(define dx 0.00001)

(define (average-3 a b c)
    (/ (+ a b c) 3))

(define (compose func1 func2)
    (lambda (x) (func1 (func2 x))))

(define (repeated f n)
    (define (compose-iter g count)
        (cond ((= count n) g)
            ((< (* 2 count) n) (compose-iter (compose g g) (* count 2)) )
            (else (compose-iter (compose f g) (+ count 1))))
    )
    (compose-iter f 1)
)

; smooth
(define (smooth f)
    (lambda (x) (average-3 (f x) 
                            (f (- x dx)) 
                            (f (+ x dx)))
                            ))


(define (n-fold-smooth f n)
    ((repeated smooth n) f))


