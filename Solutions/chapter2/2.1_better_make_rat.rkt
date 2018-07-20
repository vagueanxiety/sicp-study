#lang sicp

(define (make-rat n d) 
    (define (xnor x y)
        (or (and x y) (not (or x y))))

    (let ((g (gcd (abs n) (abs n)))
            (n-negative? (negative? n))
            (d-negative? (negative? d)))
                (let ((positive-rat? (xnor n-negative? d-negative?)))
                
                (if positive-rat? (cons (/ (abs n) g) (/ (abs d) g))
                                  (cons (/ (- (abs n)) g) (/ (abs d) g)))

                )
    ))




(define (numer x) (car x)) 
(define (denom x) (cdr x))

(define (print-rat x) 
    (newline)
    (display (numer x)) (display "/") (display (denom x)))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                (* (denom x) (denom y))))
                
(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                (* (denom x) (denom y))))
(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
    (* (denom x) (denom y)))) 
        
(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
        (* (numer y) (denom x))))


(define x (make-rat 2 6))
(define y (make-rat 1 6))
(print-rat (add-rat x y))
(print-rat (mul-rat x y))


(define a (make-rat (- 2) 6))
(define b (make-rat (- 1) 6))
(print-rat (add-rat a b))
(print-rat (mul-rat a b))


(define c (make-rat (- 3) 6))
(define d (make-rat 1 6))
(print-rat (add-rat c d))
(print-rat (mul-rat c d))

;testing built in gcd
(newline)
(gcd 2 4)
(gcd -2 -4)
(gcd -2 4)


; The whole thing can be simply replaced by testing the product of n and d

; http://community.schemewiki.org/?sicp-ex-2.1
; the post above also points out that "If gcd is defined as described 
; in 1.2.5, it will have sign depending on the number of iterations 
; it runs and the signs of a and b." 

; related question: 
; https://math.stackexchange.com/questions/927050/can-we-find-the-gcd-of-a-positive-and-negative-number


; takeaways:
; 1. gcd{a,b}=gcd{|a|,b}=gcd{a,|b|}=gcd{|a|,|b|}.
; 2. testing if signs are the same by multiplication
; 3. built-in gcd handles the signs according to 1.
 
