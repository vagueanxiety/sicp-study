#lang sicp
;helpers

(define (square x)
    (* x x)
)

(define (scale-tree tree factor) 
    (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                            (scale-tree (cdr tree) factor)))))

; another version of scale-tree   
; (define (scale-tree tree factor) 
;     (map (lambda (sub-tree)
;         (if (pair? sub-tree) 
;             (scale-tree sub-tree factor) 
;             (* sub-tree factor)))
;         tree))


(define (without-map-square-tree tree)
    (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (without-map-square-tree (car tree) )
                            (without-map-square-tree  (cdr tree) ))
        )
        )
    )


(define (map-square-tree tree)
    (map (lambda (sub-tree)
        (if (pair? sub-tree) 
            (map-square-tree sub-tree) 
            (square sub-tree))) tree ))

(define (tree-map proc tree)
    (map (lambda (sub-tree)
        (if (pair? sub-tree) 
            (tree-map proc sub-tree) 
            (proc sub-tree))) tree ))

(define (square-tree tree)
    (tree-map square tree))

(define (cube-tree tree)
    (tree-map (lambda (x) (* x x x)) tree))

(define (subsets s) 
    (if (null? s) (list nil)
        (let ((rest (subsets (cdr s))))
            (append rest (map 
                                (lambda (x) (append (list (car s)) x)) 
                                rest)))))

;testing
(map-square-tree
    (list 1
          (list 2 (list 3 4) 5)
          (list 6 7)))
;(1 (4 (9 16) 25) (36 49))

(without-map-square-tree
    (list 1
          (list 2 (list 3 4) 5)
          (list 6 7)))

(square-tree
    (list 1
          (list 2 (list 3 4) 5)
          (list 6 7)))

(list 1 (list 4 (list 9 16) 25) (list 36 49))



(cube-tree
    (list 1
          (list 2 (list 3 4) 5)
          (list 6 7)))

(newline)

(display (subsets (list 1 2 3)))
(newline)
(display (list '() (list 3) (list 2) (list 2 3) (list 1) (list 1 3) (list 1 2) (list 1 2 3)))

; explanation
; - http://community.schemewiki.org/?sicp-ex-2.32
; 
; when the program hits the base case, (list '()) will be returned (note that it is a list with only one
; element nil).
; The program then will go up in the recursion tree, and it be something like
; (append (list '()) (map ...) )
; by wishful thinking, let's say that is evalutated to  
; (append (list '()) ), which will be displayed as (())
; by observing the sample anwer, 
; the recursion will be like
; (() (3) ....) rest = (2 3)
; (() (3) (2) (2 3)...) rest = (1 2 3)
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))