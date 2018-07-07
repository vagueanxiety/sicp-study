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



(define (fast_multiply a b)
    (cond 
        ( (= b 0) 0 )
        ( (even? b) 
            (double ( fast_multiply 
                            a
                            (halve b) 
                            ))) 
        ( (> b 0) 
            (+ a (fast_multiply a (- b 1)))) 
        ( else ; b < 0
            (+ (- a) (fast_multiply a (+ b 1))))             ; double and addition are the deferred operation
    )
)

; (define (* a b)
;     (if (= b 0) 
;         0
;         ( + a (* a (- b 1)) )
;     )
; )



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




(fast_multiply 1 1)
(fast_multiply 1 5)
(fast_multiply 2 6)
(fast_multiply 4 6)
(fast_multiply 7 8)
(fast_multiply 3 9)

(fast_multiply 1 0)
(fast_multiply 0 0)

(fast_multiply -1 1)
(fast_multiply -1 5)
(fast_multiply -2 6)
(fast_multiply -4 6)
(fast_multiply -7 8)
(fast_multiply -3 9)

(fast_multiply 1 -1)
(fast_multiply 1 -5)
(fast_multiply 2 -6)
(fast_multiply 4 -6)
(fast_multiply 7 -8)
(fast_multiply 3 -9)

(fast_multiply -1 -1)
(fast_multiply -1 -5)
(fast_multiply -2 -6)
(fast_multiply -4 -6)
(fast_multiply -7 -8)
(fast_multiply -3 -9)