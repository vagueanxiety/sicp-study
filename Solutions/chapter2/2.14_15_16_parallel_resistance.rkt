#lang sicp
(define (print-interval i) 
    (newline) 
    (display ": [") 
    (display (lower-bound i)) 
    (display ",") 
    (display (upper-bound i)) 
    (display "]\n")) 

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
    (let ((w (* c percent)))
    (make-interval (- c w) (+ c w))
    )
)
(define (percent i)
    (/ (width i) (center i)))


(define (par1 r1 r2)
(div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))


(define (par2 r1 r2)
(let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

                       
(define x (make-center-percent 10 0.03))
(define y (make-center-percent 10 0.03))

(define a (par1 x y))
(define b (par2 x y))

(print-interval a)
(center a)
(percent a)
(newline)
(print-interval b)
(center b)
(percent b)
(newline)
(newline)
(div-interval x x)
(div-interval x y)


; http://wiki.drewhess.com/wiki/SICP_exercise_2.14
; http://wiki.drewhess.com/wiki/SICP_exercise_2.15
; 

; >> from jz at http://community.schemewiki.org/?sicp-ex-2.14-2.15-2.16
; All 3 problems point to the difficulty of "identity" when dealing with intervals.
; So, any time we do algebraic manipulation of an equation involving intervals, we
; need to be careful any time we introduce the same interval (e.g. through fraction 
; reduction), since our interval package re-introduces the uncertainty, even if it shouldn't.


; >> from: http://wiki.drewhess.com/wiki/SICP_exercise_2.16
; In the interval arithmetic system we've defined, some of the laws of
; algebra that we're accustomed to don't apply to certain operations, so
; algebraic expressions that are equivalent in a non-interval arithmetic
; system are not necessarily equivalent in an interval arithmetic system.

; takeaways
; 1. when doing stuff in a different system, question the laws we are accustomed to.



