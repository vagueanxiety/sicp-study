#lang sicp

(define (double f)
    (lambda (x) (f (f x))))


;testing
(((double (double double)) inc) 5)
; (double double) => (double (double x)) => (doubletwice x)
; (double (double double)) => (doubletwice (doubletwice x))
; ((double (double double)) inc) => (doubletwice (doubletwice inc))
; (doubletwice (double (double inc))) => (doubletwice inc4)
; (double (double inc4)) => inc16
; expected: 21