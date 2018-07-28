#lang sicp

;helpers
(define (length items)
    (define (length-iter a count)
    (if (null? a) count
            (length-iter (cdr a) (+ 1 count))))
    (length-iter items 0))

(define (list-ref items n) (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (append list1 list2) (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

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



; constructors
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

; selectors
(define (left-branch mobile)
    (list-ref mobile 0))
    
(define (right-branch mobile)
   (list-ref mobile 1) )

(define (branch-length branch)
    (list-ref branch 0))

(define (branch-structure branch)
    (list-ref branch 1))

; this definition is not mentioned in the question
; but it is useful for constructing recursive process
(define (branch-weight branch)
    (let ((structure (branch-structure branch)))
            (cond ((pair? structure) (total-weight structure))
                    (else structure)))
)

    
(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile))  (branch-weight (right-branch mobile)) )
)

(define (balanced? mobile)
    (define (branch-balance branch)
        (let ((structure (branch-structure branch)))
                (cond ((number? structure) true)
                (else (balanced? structure))))
    )

    (define (torque branch)
        (* (branch-length branch)(branch-weight branch)))

    (let ((leftB (left-branch mobile))
          (rightB (right-branch mobile)))
        (and 
            (= (torque leftB) (torque rightB))
            (branch-balance leftB) (branch-balance rightB)))
)

; question d is basically a question that tests the orthogonality of the program
; to see how well the abstraction is done.
; the only changes are the selectors
; 

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


(newline)
(display "binary mobile question\n")

 ;; Testing from 2DSharp at http://community.schemewiki.org/?sicp-ex-2.29 
(define a (make-mobile (make-branch 2 3) (make-branch 2 3))) 
(total-weight a) ;; 6 

(define d (make-mobile (make-branch 10 a) (make-branch 12 5))) 
;; Looks like: ((10 ((2 3) (2 3))) (12 5)) 
     
(balanced? d) ;; #t 

; testing from Rather Iffy at http://community.schemewiki.org/?sicp-ex-2.29
(define m1 (make-mobile 
    (make-branch 4 6) 
    (make-branch 5 
                    (make-mobile 
                    (make-branch 3 7) 
                    (make-branch 9 8))))) 

;;          4  |  5 
;;        +----+-----+ 
;;        6        3 |     9 
;;               +---+---------+ 
;;               7             8 

(total-weight m1)
;  Value: 21 

  
(define m2 (make-mobile 
    (make-branch 4 6) 
    (make-branch 2 
                 (make-mobile 
                  (make-branch 5 8) 
                  (make-branch 10 4))))) 

;;          4  | 2 
;;        +----+--+ 
;;        6    5  |    10 
;;          +-----+----------+ 
;;          8                4 

(balanced? m2) 
;Value: #t 
(balanced? m1) 
;Value: #f 

