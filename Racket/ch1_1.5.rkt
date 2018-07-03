#lang sicp
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y)
  )

; p is the symbol of a procedure 
; evaluating procedure p will keep evaluating p

(test 0 (p))