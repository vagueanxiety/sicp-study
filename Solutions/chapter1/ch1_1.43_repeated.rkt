#lang sicp

(define (even? x) 
    (= 0 (remainder x 2))
)

(define (square x)
(* x x)
)

(define (compose func1 func2)
    (lambda (x) (func1 (func2 x))))

; an iterative solution
(define (repeated f n)
    (define (compose-iter g count)
        (cond ((= count n) g)
            ((< (* 2 count) n) (compose-iter (compose g g) (* count 2)) )
            (else (compose-iter (compose f g) (+ count 1))))
    )
    (compose-iter f 1)
)
; a more elegant recursive solution: http://community.schemewiki.org/?sicp-ex-1.43

;testing
((repeated square 2) 5)
((repeated square 3) 3)
((repeated square 1) 3)
((repeated square 4) 2)