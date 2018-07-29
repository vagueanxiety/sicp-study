#lang sicp
(define (square x)
    (* x x)
)

(define (enumerate-tree tree) 
    (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (filter predicate sequence) 
    (cond   ((null? sequence) nil)
            ((predicate (car sequence))
                (cons (car sequence) (filter predicate (cdr sequence))))
            (else (filter predicate (cdr sequence)))))


(define (accumulate op initial sequence) 
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))
; commented out to use the extended version
; (define (map p sequence)
;     (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2) (accumulate cons seq2 seq1))
(define (length sequence) (accumulate (lambda (x y) (+ y 1)) 0 sequence))


(define (horner-eval x coefficient-sequence) 
    (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
                0
                coefficient-sequence))


(define (count-leaves t)
    (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

; this solution is from jz at http://community.schemewiki.org/?sicp-ex-2.36
; approach: 
; get the first elements from all sequences in seqs and accumulate them
; cons them with accumulate-n of the remaining elements from all sequences 

; two helper procedures
(define (select-cars sequence) 
    (map car sequence)) 
   
(define (select-cdrs sequence) 
    (map cdr sequence)) 


; for example, let's say seqs is ((1 2 3)(4 5 6)) 
; then thte subproblem is ((2 3)(5 6))
; eventually the base case, (()()), is hit.
; which is why (if (null? (car seqs)) makes sense

(define (accumulate-n op init seqs) 
    (if (null? (car seqs))
        nil
        (cons   (accumulate op init (select-cars seqs))
                (accumulate-n op init (select-cdrs seqs)))))


; defining matrix and vector operations in terms of sequences
(define (dot-product v w)
      (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v) (map (lambda (x) (dot-product v x)) m))
(define (transpose mat) (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
            (map (lambda (x) (matrix-*-vector cols x)) m)))
            


; fold left accumulate 
(define (fold-left op initial sequence) 
    (define (iter result rest)
        (if (null? rest) result
            (iter (op result (car rest))
                    (cdr rest))))
(iter initial sequence))


(define (fold-right op initial sequence) 
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

;testing
(map square (list 2 3 5 8 3))
(append (list 1 2 3 4) (list 4 3 2 1))
(length (list 3 8 48 93 29 0))
(horner-eval 2 (list 1 3 0 5 0 1))
(count-leaves (list 1 (list 2 (list 3 4)) 5))
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n * 1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(newline)
; test cases are from jz at http://community.schemewiki.org/?sicp-ex-2.37
(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))) 
(dot-product (list 1 2 3) (list 4 5 6)) 
(display (matrix-*-vector matrix (list 2 3 4 5)))
(newline)
(display (transpose matrix))
(newline)
(display (matrix-*-matrix matrix (list (list 1 2) (list 1 2) (list 1 2) (list 1 2))))
; expected: ((10 20) (26 52) (42 84)) 
(newline)

(fold-right / 1 (list 1 2 3))
; 1/(2/(3/1))
(fold-left / 1 (list 1 2 3))
; ((1/1) / 2) / 3
(display (fold-right list nil (list 1 2 3)))
(newline)
; (1 (2 (3 nil))
(display (fold-left list nil (list 1 2 3)))
(newline)
;( (nil 1) 2) 3))

; Give a property that op should satisfy 
; to guarantee that fold-right and fold-left will produce the same values for any sequence.
; (op a b) = (op b a)