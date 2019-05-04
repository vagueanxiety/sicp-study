#lang sicp
;; implement cons in terms of mutators and get-new-pair
;(define (cons x y)
;(let ((new (get-new-pair)))
;(set-car! new x)
;(set-cdr! new y)
;new))

;; preconditions:
; x cannot be empty
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))



(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))


z
;(a b c d)

(cdr x)
;(b)

(define w (append! x y))

w
;(a b c d)

(cdr x)
;(b c d)

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; a cycle


(define c (make-cycle (list 'a 'b 'c)))
;(last-pair c)
;; it never terminates



;; '() === nil
(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x))) 
        (set-cdr! x y)
        (loop temp x))
      )
    )
  (loop x '()))

;;
; reverse elements in a list
; x points to the last pair in the reversed list 

(define v (list 'a 'b 'c 'd))
v
(define m (mystery v))
v
; last pair in the reversed list
m
; the reversed list
