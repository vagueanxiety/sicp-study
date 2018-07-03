#lang sicp

; new if implementation
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause))
)

(if #t (display "good") (display "bad"))
(display "\n")
(new-if #t (display "good") (display "bad"))