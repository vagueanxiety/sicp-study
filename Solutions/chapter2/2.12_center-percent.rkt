#lang sicp

; interval in problem domain (the use)
(define (add-interval x y)
(make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
            (p3 (* (upper-bound x) (lower-bound y)))
            (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))

(define (div-interval x y) 
    (let ((p (* (upper-bound y) (lower-bound y))))
        (if (or (< p 0) (= p 0)) 
            (display "Division by an interval that spans zero.\n")
            (mul-interval
                x
                (make-interval (/ 1.0 (upper-bound y))
                                (/ 1.0 (lower-bound y))))))
    )

(define (sub-interval x y)
    (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y))))) 

(define (interval-width x)
    (/ (- (upper-bound x) (lower-bound x)) 2.0))


; abstraction barriers (interfaces) and the underlying representation of interval
; NOTE that users HAVE TO pass lower bound and upper bound as the first and the second arguments
; Using primitive min and max can remove this restriction. See sicp solutions.
(define (make-interval a b) (cons a b))
(define (upper-bound intvl)
    (cdr intvl))
(define (lower-bound intvl)
    (car intvl))

(define (make-center-width c w) (make-interval (- c w) (+ c w)))
(define (center i)
(/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
(/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c percent) 
    (let ((w (* center percent)))
    (make-interval (- c w) (+ c w))
    )
)
(define (percent i)
    (/ (width i) (center i)))

; takeaway:
; before implementing anything new, think whether the new procedure (or its definition)
; can be built on previously implemented ones or not.