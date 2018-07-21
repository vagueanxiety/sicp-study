#lang sicp
(define (cons x y) (lambda (m) (m x y)))
; a pair is a procedure that takes in a procedure and then applies it to x and y

(define (car z)
    (z (lambda (p q) p)))
; the procedure gets passed into the pair is a procedure that spits out the first arg.
; (z (lambda (p q) p)) => (lambda(x y)) => p

(define (cdr z)
    (z (lambda (p q) q)))
