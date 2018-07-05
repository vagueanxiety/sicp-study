#lang sicp
; when the question says generating the elements of a pascal triangle, it
; means retriving the element at row r and column c in the left aligned triangle.


(define (pascal r c)
    (cond ( (or (= 1 c) (= r c)) 1 )
          (else 
          (+ 
            (pascal (- r 1) (- c 1)) ; numbers move towards top left corner and eventually hit left edge
            (pascal (- r 1) c ) ; numbers move towards up and eventually hit the right edge
          )
          )
    )
)



(pascal 1 1)
(pascal 2 2)
(pascal 3 3)
(pascal 4 3)
(pascal 5 4)

; (pascal 4 5) checks for row and column can be added 