#lang sicp

; this snippet is from sicp book
(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance)) 
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))



(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)


; proof that phi is the fixed point of funvtion x -> 1 + 1/x
; phi's definition: 
; phi^2 = phi + 1
; multiply both sides of the function by x:
; x^2 = x + 1
; we further solve this quadratic function and drop the negative root:
; http://www.wolframalpha.com/input/?i=sovle+x%5E2+%3D+x+%2B+1
; phi = (1 + sqrt of 5) / 2 = 1.6180
; which is also obtained by the fixed-point procedure above