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

(define (div-interval x y) (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y)))))

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

(define x1 (make-interval 1 3))
(define y1 (make-interval 2 4))

(define x2 (make-interval (- 1) 1))
(define y2 (make-interval (- 3) (- 1)))

(interval-width x1)
(interval-width y1)
(newline)
(interval-width x2)
(interval-width y2)
(newline)
(newline)

(interval-width (mul-interval x1 y1))
(interval-width (div-interval x1 y1))

(newline)
(interval-width (mul-interval x2 y2))
(interval-width (div-interval x2 y2))



;
; For summing two intervals a b, with their respective upperbound and lowerbound, c d and e f.
; the width of a: (c - d)/2
; the width of b: (e - f)/2
; the width of a + b : ((c+e) - (d+f))/2  = the width of a + the width of b
;
; from: http://matuszek.org/functions/functions.html
; "In our pictures (see Figure 6), every element of 
; S has exactly ONE arrow coming from it. This kind of relation is called a function."
; 
; Therefore, all we need to show is that a pair of widths a and b may
; lead to different widths of (a * b) or (a / b)
; 
