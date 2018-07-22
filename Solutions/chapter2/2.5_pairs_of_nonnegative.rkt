;   from sicp book:
;   Exercise 2.5: Show that we can represent pairs of nonnegative integers 
;   using only numbers and arithmetic operations if we represent the pair 
;   a and b as the integer that is the product 2^a*3^b . Give the corresponding
;   definitions of the procedures cons, car, and cdr.

; approach
; 1. prove that one such product 2^a*3^b corresponds to a unique pair of a and b => injectivity
; 2. prove that every pair of nonnegative integers can be represented 2^a*3^b  => surjectivity
; 3. come up ways to extracting a and b from 2^a*3^b
; 
; the proof for 1. and 2. is in a separate pdf file.

#lang sicp

(define (cons a b)
    (* (expt 2 a) (expt 3 b))) ; no sanity check for now

(define (car p)
    (define (extract-iter product count)
        (cond ((even? product) (extract-iter (/ product 2) (+ 1 count))) 
                (else count)))
          b
    (extract-iter p 0)
)

(define (cdr p)
    (define (extract-iter product count)
        (cond ((= 0 (remainder product 3)) (extract-iter (/ product 3) (+ 1 count))) 
                (else count)))
    (extract-iter p 0)
)

(define a (cons 3 4))
(define b (cons 0 4))
(define c (cons 3 0))
(car a)
(car b)
(car c)

(cdr a)
(cdr b)
(cdr c)


(newline)
(car (cons 12 34)) 
(cdr (cons 12 34)) 
(car (cons  3 0)) 
(cdr (cons  3 0)) 
(car (cons  0 3)) 
(cdr (cons  0 3)) 
(car (cons  0 0)) 
(cdr (cons  0 0)) 