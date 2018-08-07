#lang sicp
(define (display-list content)
    (display content)
    (newline))
    
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree)) 
(define (right-branch tree) (caddr tree)) 
(define (make-tree entry left right)
    (list entry left right))
; searching 
(define (element-of-set? x set) 
    (cond   ((null? set) false)
            ((= x (entry set)) true)
            ((< x (entry set))
                (element-of-set? x (left-branch set)))
            ((> x (entry set))
                (element-of-set? x (right-branch set)))))
; inserting
(define (adjoin-set x set)
    (cond   ((null? set) (make-tree x '() '()))
            ((= x (entry set)) set)
            ((< x (entry set))
                (make-tree (entry set)
                            (adjoin-set x (left-branch set))
                            (right-branch set)))
            ((> x (entry set))
                (make-tree (entry set) 
                            (left-branch set)
                            (adjoin-set x (right-branch set))))))



(define (tree->list-1 tree) 
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree) (tree->list-1 (right-branch tree))))
    )) 

    
(define (tree->list-2 tree)
    (define (copy-to-list tree result-list) 
        (if (null? tree)
            result-list
            (copy-to-list 
                (left-branch tree) 
                (cons 
                    (entry tree) 
                    (copy-to-list (right-branch tree) result-list)))))
(copy-to-list tree '()))

; testing 
; from http://community.schemewiki.org/?sicp-ex-2.63
(define fig2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))) 
(define fig2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))) 
(define fig2-16-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))) 

(display-list (tree->list-1 fig2-16-1))
(display-list (tree->list-2 fig2-16-1))
(newline)
(display-list (tree->list-1 fig2-16-2))
(display-list (tree->list-2 fig2-16-2))
(newline)
(display-list (tree->list-1 fig2-16-3))
(display-list (tree->list-2 fig2-16-3))
(newline)

; tree->list-1
; for a balanced tree and an implementation of append that takes linear time
; T(n) = 2*T(n/2) + O(n/2) 
; T(n) = 2*(2* ... (2*O(1) + O(n'/2)) + O(n/2))), where n' is the number of nodes for the base case
; and there would logn nestings
; T(n) = O(logn * n) 
; (from the worst-case n)

; tree->list-2
; for a balanced tree and an implementation of append that takes linear time
; T(n) = 2*T(n/2) + O(1)
; T(n) = O(n) 
; assuming that the new list from cons-ing takes O(1) to be passed to the outter procedure call

(define (list->tree elements)
    (car (partial-tree elements (length elements))))
(define (partial-tree elts n) 
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elts left-size)))
                (let ((left-tree (car left-result)) 
                      (non-left-elts (cdr left-result)) 
                      (right-size (- n (+ left-size 1))))
                (let ((this-entry (car non-left-elts)) 
                      (right-result (partial-tree (cdr non-left-elts) right-size)))
                    (let ((right-tree (car right-result)) 
                          (remaining-elts (cdr right-result)))
                        (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))


(display-list (list->tree (list 1 3 5 7 9 11)))

; detailed explanation:
;http://community.schemewiki.org/?sicp-ex-2.64
; "T(n) = 2T(n ÷ 2) + Θ(1) 
; By the Master Theorem, we have a = 2, b = 2, and f(n) = Θ(1). Therefore, T(n) = Θ(n).
; The time taken by LIST->TREE for a list of length n will be the time taken by PARTIAL-TREE
; plus the time taken by LENGTH for that list. Both procedures have order of growth Θ(n), so 
; the order of growth of LIST->TREE is Θ(n)."

; references:
; https://www.saylor.org/site/wp-content/uploads/2011/06/Master-theorem.pdf

(define (tree-repre-intersection-set set1 set2)
    (let ((list1 (tree->list-2 set1))
          (list2 (tree->list-2 set2)))
        (list->tree (ordered-intersection-set list1 list2))))

(define (tree-repre-union-set set1 set2)
    (let ((list1 (tree->list-2 set1))
          (list2 (tree->list-2 set2)))
        (list->tree (ordered-union-set list1 list2))))

(define (ordered-intersection-set set1 set2) 
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2) (ordered-intersection-set (cdr set1) set2))
            ((< x2 x1) (ordered-intersection-set set1 (cdr set2)))))))


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


(define (lookup given-key set-of-records) 
    (cond   ((null? set-of-records) false)
            ((= given-key (key (entry set-of-records))) (entry set-of-records))
            ((< given-key (key (entry set-of-records)))
                (lookup given-key (left-branch set-of-records)))
            ((> given-key (key (entry set-of-records)))
                (lookup given-key (right-branch set-of-records)))))
