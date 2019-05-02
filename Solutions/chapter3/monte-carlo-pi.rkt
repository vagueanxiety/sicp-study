#lang sicp

(define TOP_LEFT_X -1)
(define TOP_LEFT_Y 1)
(define BOTTOM_RIGHT_X 1)
(define BOTTOM_RIGHT_Y -1)

(define (estimate-pi trials tlx tly brx bry)
  (* (monte-carlo trials cesaro-test) (rect-area tlx tly brx bry)))

(define (rect-area tlx tly brx bry)
  (* (- brx tlx) (- tly bry)))

(define (in-unit-circle x y)
  (< (+ (* x x) (* y y)) 1))

(define (cesaro-test)
  (in-unit-circle (my-rand)(my-rand)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (my-rand)
  (- (random-in-range 0.0 2.0) 1))

(define (println x)
  (display x)
  (newline))

(my-rand)
(my-rand)
(my-rand)
(in-unit-circle 0 0)
(in-unit-circle 0.5 0.5)
(in-unit-circle 1 1)
(rect-area TOP_LEFT_X TOP_LEFT_Y BOTTOM_RIGHT_X BOTTOM_RIGHT_Y)
(exact->inexact (estimate-pi 1000000 TOP_LEFT_X TOP_LEFT_Y BOTTOM_RIGHT_X
                             BOTTOM_RIGHT_Y))
