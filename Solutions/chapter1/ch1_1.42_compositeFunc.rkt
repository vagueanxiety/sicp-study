#lang sicp
(define (compose func1 func2)
    (lambda (x) (func1 (func2 x))))

(define (square x)
    (* x x)) 
;testing
((compose square inc) 6) 
;expected 49