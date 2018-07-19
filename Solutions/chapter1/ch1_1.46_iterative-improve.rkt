#lang sicp


(define (iterative-improve good-enough? improve)
    (define (my-lambda guess)
        (if (good-enough? guess)(improve guess)
                                (my-lambda (improve guess))
        )
    )
    my-lambda
)


(define (sqrt x)
    ((iterative-improve 
                        (lambda (y) (< (abs(- (square y) x)) 0.00001))
                        (lambda (z) ((average z (/ x z))))
     )
     1.0
    )
  )

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
    ((iterative-improve 
        (lambda (x) (close-enough? x (f x))) ; !!!
        f
    )
    first-guess
    )
)

(define tolerance 0.00001)

(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))

;takeaways:
; 1. using lambda to reduce the number of params when params are related to each other in some way.
; 2. To implement a recursive procedure with lambda, an "iter" function can be defined internally 
;    and then the lambda can make a call to it.