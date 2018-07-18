#lang sicp
(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance)) 
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(define dx 0.00001)
(define (deriv g)
(lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess) (fixed-point (newton-transform g) guess))

(define (cubic a b c)
    (lambda (x) 
            (+  (* x x x)
                (* a x x)
                (* b x)
                c)
    )
)

; testing
; find the zeros of x^3 + x^2 + 2x + 1
; expected: x ≈ -0.569840
(newtons-method (cubic 1 2 1) 1)
; find the zeros of x^3 + x^2 - 2x + 1
; expected: x ≈ -2.14790
(newtons-method (cubic 1 (- 2) 1) 1)
