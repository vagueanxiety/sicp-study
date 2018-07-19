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

    (define h (/ (- b a) n)) 
    ; revisiting after reading lambda: 

    ; "Sometimes we can use internal definitions to get the same effect as with let."

    ; "Understanding internal definitions well enough to be sure a program means what we intend it to mean 
    ; requires a more elaborate model of the evaluation process than we have presented in this chapter. The
    ; subtleties do not arise with internal definitions of procedures, however. We will
    ; return to this issue in Section 4.1.6, after we learn more about evaluation."

    ; define the term (a piece-wise function g(k) that computes the term, more precisely) in the summation
    ; the term depends on the value of k
    ; k is also the iteration variable
    (define (simpson-term k)

        (define new-f (f( + a (* k h)))) 
        ; i think it is a variable rather than a procedure, because it does not have **brackets** around it, nor a lambda expression after its name.
        ; revisiting after reading lambda: 
        ; https://stackoverflow.com/questions/20081911/is-this-possible-to-define-a-function-with-no-arguments-in-racket
        ; http://schemers.org/Documents/Standards/R5RS/r5rs.pdf (understanding add4 mentioned in the standard is not that easy.)
   
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
; - Sometimes, functions need to transformed (adding coefficients, adding piece-wise definitions, etc) before it is passed to a higher-order function.
; - Sequencial parsing is not guaranteed: https://stackoverflow.com/questions/43403098/scheme-racket-undefined-function-cannot-use-before-initialization

