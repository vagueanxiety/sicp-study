#lang sicp


(define (double x)
    (* x 2) 
)


(define (halve x)
    (cond 
        ((even? x) (/ x 2))
        (else (/ (- x 1) 2))
    )
)


(define (even? x) 
    (= 0 (remainder x 2))
)



(define (recursive-fast-mult a b)
    (cond 
        ( (= b 0) 0 )
        ( (even? b) 
            (double ( recursive-fast-mult 
                            a
                            (halve b) 
                            ))) 
        ( (> b 0) 
            (+ a (recursive-fast-mult a (- b 1)))) 
        ( else ; b < 0
            (+ (- a) (recursive-fast-mult a (+ b 1))))             ; double and addition are the deferred operation
    )
)

; tail recursion version is at http://community.schemewiki.org/?sicp-ex-1.17
; the solution needs to be modified slightly to handle multiplication with negative b

; part of this code is from http://community.schemewiki.org/?sicp-ex-1.17
(define (iterative-fast-mult a b)     
   (define (helper a b product) ;; "add" a b times 
     (cond ((= b 0) product) 
           ((even? b) (helper (double a) (halve b) product)) 
           ((> b 0) (helper a (- b 1) (+ a product)))
           ((< b 0) (helper a (+ b 1) (+ (- a) product)))
           
           
    )) 
   (helper a b 0)) 



;testing

(double 0)
(double 2)
(double 3)
(double -1)
(double -9)

(halve 0)
(halve 1)
(halve 2)
(halve 3)
(halve 4)
(halve -4)
(halve -3)


(display "testing recursive version\n")
; + + 
(recursive-fast-mult 1 1)
(recursive-fast-mult 1 5)
(recursive-fast-mult 2 6)
(recursive-fast-mult 4 6)
(recursive-fast-mult 7 8)
(recursive-fast-mult 3 9)

; 0
(recursive-fast-mult 1 0)
(recursive-fast-mult 0 0)

; - +
(recursive-fast-mult -1 1)
(recursive-fast-mult -1 5)
(recursive-fast-mult -2 6)
(recursive-fast-mult -4 6)
(recursive-fast-mult -7 8)
(recursive-fast-mult -3 9)

; + -
(recursive-fast-mult 1 -1)
(recursive-fast-mult 1 -5)
(recursive-fast-mult 2 -6)
(recursive-fast-mult 4 -6)
(recursive-fast-mult 7 -8)
(recursive-fast-mult 3 -9)

; - -
(recursive-fast-mult -1 -1)
(recursive-fast-mult -1 -5)
(recursive-fast-mult -2 -6)
(recursive-fast-mult -4 -6)
(recursive-fast-mult -7 -8)
(recursive-fast-mult -3 -9)


(display "testing iterative version\n") 
; + + 
(iterative-fast-mult 1 1)
(iterative-fast-mult 1 5)
(iterative-fast-mult 2 6)
(iterative-fast-mult 4 6)
(iterative-fast-mult 7 8)
(iterative-fast-mult 3 9)

; 0
(iterative-fast-mult 1 0)
(iterative-fast-mult 0 0)

; - +
(iterative-fast-mult -1 1)
(iterative-fast-mult -1 5)
(iterative-fast-mult -2 6)
(iterative-fast-mult -4 6)
(iterative-fast-mult -7 8)
(iterative-fast-mult -3 9)

; + -
(iterative-fast-mult 1 -1)
(iterative-fast-mult 1 -5)
(iterative-fast-mult 2 -6)
(iterative-fast-mult 4 -6)
(iterative-fast-mult 7 -8)
(iterative-fast-mult 3 -9)

; - -
(iterative-fast-mult -1 -1)
(iterative-fast-mult -1 -5)
(iterative-fast-mult -2 -6)
(iterative-fast-mult -4 -6)
(iterative-fast-mult -7 -8)
(iterative-fast-mult -3 -9)