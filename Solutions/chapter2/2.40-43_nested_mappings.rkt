#lang sicp
; helpers
; proc maps elements to lists, and then appending all lists to one list.

(define (square x)
    (* x x)
)

(define (length sequence) (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (display-list content)
    (display content)
    (newline))
    
(define (list-ref items n) (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))
    
; prime?
(define (even? x) 
    (= 0 (remainder x 2))
)

(define (divides? x d)
    (= 0 (remainder x d))
)

(define (smallest-divisor x)
    (find-divisor x 2)
)

(define (find-divisor x d)
    (cond
        ((> (square d) x) x)
        ((divides? x d) d)
        (else (find-divisor x (+ 1 d)))
    )
)

(define (prime? x)
    (= x (smallest-divisor x))
)


(define (filter predicate sequence) 
    (cond   ((null? sequence) nil)
            ((predicate (car sequence))
                (cons (car sequence) (filter predicate (cdr sequence))))
            (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence) 
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))



(define (permutations s)
    (if (null? s) ; empty set?
        (list nil) ; sequence containing empty set 
        (flatmap    (lambda (x) 
                        (map (lambda (p) (cons x p)) 
                            (permutations (remove x s)))) 
                    s)))


(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))


(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n) 
    (map make-pair-sum
        (filter prime-sum? (flatmap 
                            (lambda (i)
                                (map (lambda (j) (list i j)) 
                                    (enumerate-interval 1 (- i 1))))
                                (enumerate-interval 1 n)))))

(define (remove item sequence)
    (filter (lambda (x) (not (= x item))) sequence))


; exercises

(define (unique-pairs n)
     (flatmap 
        (lambda (i)
            (map (lambda (j) (list i j)) 
                (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (new-prime-sum-pairs n) 
    (map make-pair-sum
        (filter prime-sum? (unique-pairs n))))

(define (positive-ordered-triples n)
    (flatmap (lambda (i)
            (map (lambda (j_and_k) (cons i j_and_k)) 
                (positive-ordered-pairs n)))
            (enumerate-interval 1 n))
    )

(define (positive-ordered-pairs n)
    (flatmap (lambda (i)
            (map (lambda (j) (list i j)) 
                (enumerate-interval 1 n)))
        (enumerate-interval 1 n))
)

; generalize m orderd mutiples
; base case 1 "multiple" (enumerate-interval 1 n)
; keep n+1 multiple = 
;    (flatmap (lambda (i)
; (map (lambda (j) (list i j)) 
;     (n multiple)))
; (enumerate-interval 1 n))


(define (queens board-size) 
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board) 
            (filter
                (lambda (positions) (safe? k positions)) 
                    (flatmap
                        (lambda (rest-of-queens) 
                            (map (lambda (new-row)
                                (adjoin-position
                                    new-row k rest-of-queens))
                            (enumerate-interval 1 board-size)))
                        (queen-cols (- k 1))))))
                            
    (queen-cols board-size))

(define empty-board nil)
(define (adjoin-position new-row col placed-queens)
    (let ((new-queen (make-queen new-row col)))
        (append placed-queens (list new-queen))))
(define (safe? new-col queens)
    (define (check-iter last-queen rest-of-queens col)
        (cond ((= col new-col) true)
              ((two-queens-safe? last-queen (car rest-of-queens)) 
                (check-iter last-queen (cdr rest-of-queens) (+ 1 col)))
              (else false)
        ))
    (let ((last-queen (list-ref queens (- new-col 1))))
        (check-iter last-queen queens 1))        
)

(define (two-queens-safe? queen1 queen2)
    (let ((row1 (queen-row queen1))
            (col1 (queen-col queen1))
            (row2 (queen-row queen2))
            (col2 (queen-col queen2)))
        (not (or (= row1 row2) (= (abs (- row1 row2)) (abs (- col1 col2))))))
)
; makes a conceptual entity of queen to increase othogonality
(define (make-queen row col) (cons row col))
(define (queen-col queen) (cdr queen))
(define (queen-row queen) (car queen))
; testing
(display-list (new-prime-sum-pairs 6))
(display-list (positive-ordered-pairs 3))
(display-list (positive-ordered-triples 3))
(newline)
(display-list (queens 2))
(display-list (queens 3))
(display-list (queens 4))
; (display-list (queens 8))
(display (length (queens 8)))