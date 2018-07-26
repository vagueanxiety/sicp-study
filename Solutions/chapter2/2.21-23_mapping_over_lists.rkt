#lang sicp


; helpers
(define (square x)
    (* x x)
)

(define (map proc items) 
    (if (null? items)
        nil
        (cons   (proc (car items))
                (map proc (cdr items)))))

(define (square-list1 items) 
    (if (null? items)
        nil
        (cons   ((lambda (x) (* x x)) (car items))
                (square-list1 (cdr items))
        )
    ))
        
(define (square-list2 items)
    (map (lambda (x) (* x x)) items))    

; Louis's version of iterative square list
(define (square-list-louis1 items) 
    (define (iter things answer)
        (if (null? things) 
            answer
            (iter   (cdr things) 
                    (cons (square (car things)) answer)
            )
        ))


    (iter items nil))


; Unfortunately, defining square-list this way produces the
; answer list in the reverse order of the one desired. Why?
; expected: ; (1, (2, (3, (4, '()))))
; (4, (3, (2, (1, '()))))

; Because each time it cons the element at the FRONT of things to answer.

(define (square-list-louis2 items)
    (define (iter things answer) 
        (if (null? things)
                answer
                (iter   (cdr things)
                        (cons answer (square (car things))))
        ))
  (iter items nil))

; This doesn’t work either. Explain.
; ((('(), 1), 2), 3) ...
; a "mirrored" version of the previous one



(define (for-each proc items)  
    (cond   ((null? items) true)
            (else 
                (proc (car items))
                (for-each proc (cdr items))
            )
    ))

; testing

(square-list1 (list 3 7 4 13))
(square-list2 (list 3 7 4 13))
(square-list-louis1 (list 3 7 4 13))
(square-list-louis2 (list 3 7 4 13))
(for-each   (lambda (x) (newline) (display x)) 
            (list 1 3 4 5 0 8))

