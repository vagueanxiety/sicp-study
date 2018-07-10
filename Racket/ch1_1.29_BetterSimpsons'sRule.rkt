#lang sicp

; NOTE that this solution is from http://community.schemewiki.org/?sicp-ex-1.29
; I only changed slightly some definitions and added a few comments and important takeaways.

(define (round-to-next-even x)
    (+ x (remainder x 2))
)

(define (simpson-integral f a b n)

    ; definitions of two variables?
    ; (define rounded-n (round-to-next-even n) )
    ; n could be rounded like above, but DrRacket gives an undefined error for line 16 (see the last takeaway)

    (define h (/ (- b a) n)) ; function-like variables :O

    ; define the term (a piece-wise function g(k) that computes the term, more precisely) in the summation
    ; the term depends on the value of k
    ; k is also the iteration variable
    (define (simpson-term k)

        ; similar to defining a variable, a new (high order) function is defined
        ; observation: when there is only one thing in the definition, brackets are omitted
        (define new-f (f( + a (* k h)))) 
        ; variable-like function :O MAYBE they are the same thing :O

        ; adding coefficient to the term
        (if (or (= k 0) (= k n)) 
         (* 1 new-f) 
         (if (even? k) 
             (* 2 new-f) 
             (* 4 new-f))))

        ; k ranges from 0 to rounded-n
        (* (/ h 3) (sum simpson-term 0 inc n))) 
    

; higher order procedure: sum
(define (sum term a next b)
    (if (> a b) 0
        (+ (term a) (sum term (next a) next b))
    )
)

(define (cube x)
    (* x x x)
)


; testing
(simpson-integral cube 0 1.0 100)
(simpson-integral cube 0 1.0 1000)
(simpson-integral cube 0 1.0 10000)
(simpson-integral cube 0 1.0 100000)
(simpson-integral cube 0 1.0 1000000)


; Main takeaways:
; - Definitions of variables and functions sometimes can be very similar syntactically. To distinguish them, look at the 
;   which one has unknown expression within its scope. In this case, definitions of rounded n and h are composed of known expressions, while
;   simpson-term has one unknown expression: k.
; - Sometimes, functions need to transformed (adding coefficients, adding piece-wise definitions, etc) before it is passed to a higher-order function.
; - Sequencial parsing is not guaranteed: https://stackoverflow.com/questions/43403098/scheme-racket-undefined-function-cannot-use-before-initialization

