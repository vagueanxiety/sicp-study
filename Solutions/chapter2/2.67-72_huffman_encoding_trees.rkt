#lang sicp

(define (display-list content)
    (display content)
    (newline))

; leaf representation and interfaces
(define (make-leaf symbol weight) (list 'leaf symbol weight)) 
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; tree representation and interfaces
(define (make-code-tree left right) 
    (list   left
            right
            (append (symbols left) (symbols right))
            (+ (weight left) (weight right))))

(define (left-branch tree) (car tree)) 
(define (right-branch tree) (cadr tree)) 
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree)) 
        (caddr tree)))
        
(define (weight tree) 
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(define (one-of-symbols? symbol symbols) 
    (cond 
        ((null? symbols) false)
        ((eq? symbol (car symbols)) true)
        (else (one-of-symbols? symbol (cdr symbols)))))

(define (in-this-branch symbol branch) 
    (one-of-symbols? symbol (symbols branch)))

; decoding procedure
(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits) 
            '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (choose-branch bit branch)
    (cond 
        ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

; encoding messages
(define (choose-bit symbol branch)
    (cond 
        ((in-this-branch symbol (left-branch branch)) 0)
        ((in-this-branch symbol (right-branch branch)) 1)
        (else (error "bad symbol: CHOOSE-BIT" symbol))))

(define (encode message tree) 
    (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
              
(define (encode-symbol symbol current-branch)
    (cond 
        ((null? current-branch) (error "no such symbol in the tree" symbol))
        ((in-this-branch symbol current-branch) 
            (cond 
                ((leaf? current-branch) '())
                (else 
                    (let ((bit (choose-bit symbol current-branch)))
                        (let ((next-branch (choose-branch bit current-branch)))
                            (cons bit (encode-symbol symbol next-branch))))
                        )))
        (else (error "no such symbol in the tree" symbol))))

; constructing the tree
(define (adjoin-set x set) 
    (cond 
        ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))    
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs) 
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set 
                (make-leaf (car pair)(cadr pair)) 
                (make-leaf-set (cdr pairs))))))
                    
(define (generate-huffman-tree pairs) 
    (successive-merge (make-leaf-set pairs)))
(define (successive-merge pairs)
    (cond 
        ((<= (length pairs) 1) (car pairs))
        (else (successive-merge 
            (adjoin-set 
                (make-code-tree (car pairs) (cadr pairs))  
                (cddr pairs))))))


(define sample-tree 
    (make-code-tree (make-leaf 'A 4)
        (make-code-tree
            (make-leaf 'B 2)
            (make-code-tree
                (make-leaf 'D 1)
                (make-leaf 'C 1)))))

(define sample-message-encoded '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-message-decoded '(A D A B B C A))
(define sample-bad-message-decoded '(B B B F G))
(display-list (decode sample-message-encoded sample-tree))
(display-list (encode sample-message-decoded sample-tree))
; (display-list (encode sample-bad-message-decoded sample-tree))
(newline)
(define test-tree (generate-huffman-tree '((A 3) (B 5) (C 6) (D 6)))) 
(display-list test-tree)
(display-list (encode '(A B C D) test-tree))
(newline)
(define test-tree-2 (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))) 
(display-list test-tree-2)
(display-list (encode '(A D A B B C A) test-tree-2))

