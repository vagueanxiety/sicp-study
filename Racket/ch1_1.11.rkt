#lang sicp

; recursive process
(define (my-recursive-f x)
    (cond
        ((< x 3) x)
        (else (+
            (my-recursive-f (- x 1))
            (* 2 (my-recursive-f (- x 2)) )
            (* 3 (my-recursive-f (- x 3)) )
        )) ; summation and multiplication is the deferred operation
    )
)

; iterative process
; drawing inspiration from iterative fabonacci code
(define (my-iterative-f x)
    (define (iter a b c count)
        (cond 
            ((= count (- x 2)) a) ; after one iteration -> a becomes f(3) => n corresponds to f(n+2)
            (else (iter 
                    (+ a (* 2 b) (* 3 c))
                    a
                    b
                    (+ 1 count)
                    ))        
        )
    )


    (cond
        ((< x 3) x)
        (else (iter 2 1 0 0))  ; a = f(2) b = f(1) c = f(0) count = 0
    )

)



(my-recursive-f -1)
(my-recursive-f 0)
(my-recursive-f 1)
(my-recursive-f 2)
(my-recursive-f 3)
(my-recursive-f 4)
(my-recursive-f 5)



(my-iterative-f -1)
(my-iterative-f 0)
(my-iterative-f 1)
(my-iterative-f 2)
(my-iterative-f 3)
(my-iterative-f 4)
(my-iterative-f 5)