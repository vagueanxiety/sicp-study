#lang sicp

(define (memq item x) 
    (cond ((null? x) false)
            ((eq? item (car x)) x) 
            (else (memq item (cdr x)))))



(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ;((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; false
(memq 'red '((red shoes) (blue socks))) ; false
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)


(define (equal? items1 items2)
    (cond ((and (pair? items1) (pair? items2) 
                (equal? (car items1) (car items2))
                (equal? (cdr items1) (cdr items2))) true)
        ((and 
            (not (or (pair? items1) (pair? items2)))
            (eq? items1 items2)) true)
        (else false)))

(equal? '(this is a list) '(this (is a) list))
(equal? '(this is a list) '(this is a list))
(equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6)) 
(equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6)) 
(car ''abracadabra)
; quote
; (car (quote (quote abracadabra)))
; since quote quotes the next object 
; which in this case is (quote abracadabra)
; therefore (car ''abracadabra) is evaluated to quote

