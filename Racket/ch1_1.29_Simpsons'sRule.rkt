#lang sicp

(define (integral f a b n)
    (define (add-2h x)
        (+ x 
            (* 2.0 (get-h))
        )
    )

    (define (add-nh x)
        (+ x 
            (* n (get-h))
        )
    )

    (define (get-h)
        (/ (- b a) n)
    )

    (* (/ (get-h) 3.0)
        (+ 
            (sum f 
                a 
                add-nh
                (+ a 
                    (* n (get-h))
                )
            )


            (* 4.0 
                (sum f 
                    (+ a (get-h)) 
                    add-2h
                    (+ a 
                        (* (- n 1) (get-h))
                    )
                )
            )

            (* 2.0 
                (sum f 
                    (+ a (* 2 (get-h)))
                    add-2h
                    (+ a 
                        (* (- n 2) (get-h))
                    )
                    
                )
            )
        )
    
    )
)

; high order procedure: sum
(define (sum term a next b)
    (if (> a b) 0
        (+ (term a) (sum term (next a) next b))
    )
)


(define (cube x)
    (* x x x)
)


; testing
(integral cube 0 1.0 100)
(integral cube 0 1.0 1000)
(integral cube 0 1.0 10000)
(integral cube 0 1.0 100000)
(integral cube 0 1.0 1000000)

; results:
; 0.23078806666666699
; 0.24800798800666748
; 0.2499999999999509 make sense :)
; 0.24998000080046223 weird?
; 0.24999999999333847 what?