#lang sicp
; points with x and y coordinates
(define (xcor point)
    (car point))

(define (ycor point)
    (cdr point)) 

(define (make-point x y)
    (cons x y))
    
; points in problem domain
(define (print-point p) 
    (newline)
    (display "(") 
    (display (xcor p)) 
    (display ",")
    (display (ycor p))
    (display ")"))
    
; segments with start and end points
(define (seg-start seg)
    (car seg))
(define (seg-end seg)
    (cdr seg))
(define (make-seg start end)
    (cons start end))

; segment in problem domain
(define (seg-midpoint seg)
    (make-point (/ (+ (xcor (seg-start seg))  (xcor (seg-end seg)) ) 2) 
                (/ (+ (ycor (seg-start seg))  (ycor (seg-end seg)) ) 2)  ))


;testing
(define start (make-point 1 2))
(define end (make-point 3 2))
(define my-seg (make-point start end))
(print-point (seg-midpoint my-seg))