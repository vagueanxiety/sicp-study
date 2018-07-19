#lang sicp

(define (my-expo c m)
    (define (even? x) 
        (= 0 (remainder x 2))
    )

    (define (square x)
        (* x x)
    )

    (define (iter b n a)
        (cond 
            ((= n 0) a)
            ((even? n) 
                (iter (square b) (/ n 2) a) ; (square b) becomes the new b 
                                            ; the action of squaring b produces logrithmic performance
                                            ; 2 -> 4 -> 8 -> 16 -> 32
                                            ; new n will eventually be odd and the program will proceed as the case belowe
            )
            ( else (iter b (- n 1) (* a b)) ) 
        )
    )

    (iter c m 1)
)

; testing
(my-expo 2 2)
(my-expo 3 2)
(my-expo 4 5)
(my-expo 2 10)
(my-expo 1 10)
(my-expo -1 10)
(my-expo -1 9)
(my-expo 0 10)


; takeaways:
; 1. In state transformations, ALL state variables can possibly be changed.
; 2. Making progress sometimes means translating a question to an **equivalent** question 
; (like the invariant quantity mentioned in the book)
; 3. In both odd and even cases, the objective is to reduce n (no necessarily to increase a)
; 4. Line 16 does not change the process to a recursive process, because all three state variables are still
;   a **complete** description of the state of the process, and the program can resume from here at any point.
