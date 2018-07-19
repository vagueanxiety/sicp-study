; Exercise 1.34: Suppose we define the procedure 
#lang sicp
(define (f g) (g 2))

; (f square)
; 4
; (f (lambda (z) (* z (+ z 1)))) 
; 6
; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

; applicative order
; (f f)
; (f 2)
; (2 2)
; error

; normal-order
; (f f)
; (f 2)
; (2 2)

f
(f 2)

; application: not a procedure;
; expected a procedure that can be applied to arguments
; given: 2
; arguments...:
