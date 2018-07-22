#lang sicp
(define zero (lambda (f) (lambda (x) x))) 
; zero is a procedure that takes in whatever procedure and then returns the identity procedure

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))


; one = (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x)))))
; (zero f) => identity procedure
; i.e. (lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
; one = (lambda (f) (lambda (x) (f x))))  

; two = (add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x)))))
; two = (lambda (f) (lambda (x) (f (f x)))))



(define (one parameters)
    (lambda (f) (lambda (x) (f x))))

(define (two parameters)
    (lambda (f) (lambda (x) (f (f x)))))

(define (+ a b)
    (lambda (f) (lambda (x) ( (a f) ((b f) x)))))

; i guess the order of (a f) and (b f) does not matter
; as long as they are composed.


; in this representation, nonnegative numbers is just the number of f being composed together, which 
; then is applied to x.

