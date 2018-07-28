#lang sicp

;helpers
(define (map proc items) 
    (if (null? items)
        nil
        (cons   (proc (car items))
                (map proc (cdr items)))))


(define (count-leaves x) 
    (cond   ((null? x) 0)
            ((not (pair? x)) 1)
            (else (+ (count-leaves (car x))
                    (count-leaves (cdr x))))))


(define (reverse items)
    (define (iter l t)
        (if (null? l) 
            t
            (iter (cdr l) (cons (car l) t)) 
        ))
    (iter items nil)                    
)    

; (list 1 (list 2 (list 3 4)))
; result printed by the interpreter: (2 (2 (3 4)))
; sort-of-box-and-pointer structure: 
; ----------\ 
;   |   | 
;   1   ------\ 
;        |  |
;        2  --------\
;             |   |
;             3   4  
;
; from http://community.schemewiki.org/?sicp-ex-2.24
;   +---+---+  +---+---+
;   | * | *-+->| * | / |
;   +-+-+---+  +-+-+---+
;     |          |   
;     V          V      
;   +---+      +---+---+  +---+---+
;   | 1 |      | * | *-+->| * | / |
;   +---+      +-+-+---+  +---+---+
;                |          |
;                V          V
;              +---+      +---+---+  +---+---+
;              | 2 |      | * | *-+->| * | / |
;              +---+      +-+-+---+  +-+-+---+
;                           |          |
;                           V          V
;                         +---+      +---+
;                         | 3 |      | 4 |
;                         +---+      +---+
; x 
; (1 3 (5 7) 9) cadaddr NOTE that the last a is to pull 7 out of pair 7|nil
; ((7)) caar
; (1 (2 (3 (4 (5 (6 7)))))) cd ad ad ad ad ad r
; each da => enter the list at a "deeper" level
; the last da is to pull 7 out of 7|nil
; Remember (a b) is a list!

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)    ;(1 2 3 4 5 6)
(cons x y)      ;((1 2 3) 4 5 6)
(list x y)      ;((1 2 3) (4 5 6))

(define m (list (list 1 2) (list 3 4)))

(define (deep-reverse items)
    (cond   
        ((not (pair? items)) items)
        (else (reverse (map deep-reverse items)))
    )
)
; the base case in this case is that, eventually the a list of non-list elements will be reached 
; and returned back the map proc, which in turn forms a list and wait to be reversed.


(define (fringe items)
    (cond   
        ((null? items) nil)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items))
            (fringe (cdr items)))))
    
)

;testing
(newline)
(list (list 1 2) 3 4)
(deep-reverse (list (list 1 2) 3 4))

(newline)
(list (list 1 2) (list 3 4))
(deep-reverse (list (list 1 2) (list 3 4)))

(newline)
(list 1 2 3 4)
(deep-reverse (list  1 2 3 4))

(newline)
(define fringee (list (list 1 2) (list 3 4))) 
(fringe fringee)
(list 1 2 3 4)

(newline)
(fringe (list fringee fringee))
(list 1 2 3 4 1 2 3 4)

(newline)
(fringe (list 1 3 (list 5 7) 9))
(list 1 3 5 7 9)

(newline)
(fringe (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))) )
(list 1 2 3 4 5 6 7) 