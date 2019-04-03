#lang sicp

(define (make-accumulator sum)
  (define (accumulate amount)
    (begin (set! sum
             (+ sum amount))
           sum))
  (define (dispatch m)
    (cond ((number? m) (accumulate m))
          (else (error "Unknown request:
                       MAKE-ACCUMULATOR" m))))
  dispatch)

(define A (make-accumulator 5))
(A 10)
(A 10)
(A 'asdfasd)
