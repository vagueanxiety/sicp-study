#lang sicp
(define (display-list content)
    (display content)
    (newline))

(define (element-of-set? x set) 
    (cond ((null? set) false)
            ((equal? x (car set)) true)
            (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) 
    (if (element-of-set? x set)
        set
        (cons x set)))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
    (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))


;testing
(display-list (union-set (list 1 2 3 4) (list 4 6 2 9)))
(display-list (union-set (list 1 3) (list 4 6 2 9)))
(display-list (union-set '() (list 4 6 2 9)))
(display-list (union-set (list 1 2 3 4) '()))


; duplicates allowed in sets
(define (d-element-of-set? x set) 
    (cond ((null? set) false)
            ((equal? x (car set)) true)
            (else (element-of-set? x (cdr set)))))

(define (d-adjoin-set x set) 
        (cons x set))

(define (d-intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (d-union-set set1 set2)
    (append set1 set2))

; adjoin and union have better time complexities
; but extra space are used to store duplicates
; when there is a considerable nubmer of duplicates in a set, intersection and element-of-set?
; takes more time to execute.






(define (ordered-intersection-set set1 set2) 
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2) (intersection-set (cdr set1) set2))
            ((< x2 x1) (intersection-set set1 (cdr set2)))))))

(define (ordered-element-of-set? x set) 
    (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (ordered-adjoin-set x set) 
   (if (null? set) (list x)
     (let ((head (car set)))
        (cond ((= head x) set)
            ((> head x) (cons x set))
            (else (cons head (ordered-adjoin-set x (cdr set)))))
     )
   ))

(define (ordered-union-set set1 set2)
    (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
            (let ((x1 (car set1)) (x2 (car set2)))
                (cond 
                ((= x1 x2) (cons x1 (ordered-union-set (cdr set1) (cdr set2))))
                ((< x1 x2) (cons x1 (ordered-union-set (cdr set1) set2)))
                ((< x2 x1) (cons x2 (ordered-union-set set1 (cdr set2))))
                )))
    ))

;testing
(newline)
(display-list (ordered-intersection-set (list 1 2 3 4) (list 4 6 7 9)))
(display-list (ordered-element-of-set? 1 (list 4 6 7 9)))
(display-list (ordered-element-of-set? 4 (list 4 6 7 9)))
(display-list (ordered-adjoin-set 8 (list 4 6 7 9)))
(display-list (ordered-adjoin-set 8 '()))
(newline)
(display-list (ordered-union-set (list 1 2 3 4) (list 4 6 7 9)))
(display-list (ordered-union-set '() (list 4 6 7 9)))