#lang sicp
; helpers
(define (square x)
    (* x x))


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

; segments in problem domain
(define (seg-midpoint seg)
    (make-point (/ (+ (xcor (seg-start seg))  (xcor (seg-end seg)) ) 2) 
                (/ (+ (ycor (seg-start seg))  (ycor (seg-end seg)) ) 2)  ))

(define (seg-length seg)
    (sqrt (+ (square (- (ycor (seg-start seg)) (ycor (seg-end seg)) ))
             (square (- (xcor (seg-start seg)) (xcor (seg-end seg)) ))   )))


; rectangles with four (successively connected and perpendicular) segments
; the correctness ***relies on users*** to make sure the four segments
; actually form a rectangle


; this representation uses pairs arranged like a tree
;       segs
;   w1,w2  h3,h4
(define (make-rectangle width1 width2 height3 height4)
    (cons (cons width1 width2) (cons height3 height4)))

; this is not that intutive though
(define (rectangle-seg segIndex rect)
    (cond ((= segIndex 1) (car (car rect)))
          ((= segIndex 2) (cdr (car rect)))
          ((= segIndex 3) (car (cdr rect)))
            (else (cdr (cdr rect)))))


; alternative representation
; this representation uses pairs arranged like a list
;  w1 
;    w2  
;       h3
;         h4

; (define (make-rectangle width1 width2 height3 height4)
;     (cons width1 (cons width2 (cons height3  height4))))

; (define (rectangle-seg segIndex rect)
;     (cond ((= segIndex 1) (car rect))
;           ((= segIndex 2) (car (cdr rect)))
;           ((= segIndex 3) (car (cdr (cdr rect))))
;           (else (cdr (cdr (cdr rect))))))



; rectangle in problem domain
(define (rectangle-area rect)
    (* (rectangle-height rect) (rectangle-width rect)))

(define (rectangle-perimeter rect)
    (* 2 (+ (rectangle-width rect) (rectangle-height rect) )))

(define (rectangle-width rect)
    (seg-length (rectangle-seg 1 rect)))
(define (rectangle-height rect)
    (seg-length (rectangle-seg 3 rect)))

;testing
(define upperLeft (make-point 2 4))
(define upperRight (make-point 5 4)) 
(define lowerLeft (make-point 2 2))
(define lowerRight (make-point 5 2))
(define aRect (make-rectangle (make-seg upperLeft upperRight) (make-seg lowerLeft lowerRight)
                              (make-seg upperLeft lowerLeft) (make-seg upperRight lowerRight)))

                              
(define area (rectangle-area aRect))
(define perimeter (rectangle-perimeter aRect))

area
perimeter