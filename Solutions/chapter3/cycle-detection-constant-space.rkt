;; from http://community.schemewiki.org/?sicp-ex-3.19
#lang sicp
(define (contains-cycle? lst)
  (define (safe-cdr l)
    (if (pair? l)
      (cdr l)
      '()))
  (define (iter a b) ;; a is tortoise and b is hare
    (cond ((not (pair? a)) #f)
          ((not (pair? b)) #f)
          ((eq? a b) #t)
          (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))))
  (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))


; Tested with mzscheme implementation of R5RS:
(define x '(1 2 3 4 5 6 7 8))
(define y '(1 2 3 4 5 6 7 8))
(set-cdr! (cdddr (cddddr y)) (cdddr y))
(define z '(1))
(set-cdr! z z)
x ; (1 2 3 4 5 6 7 8)
y ; (1 2 3 . #0=(4 5 6 7 8 . #0#)) 
;; #0# means pointer #0= means pointee (i guess)
;
z ; #0=(1 . #0#)
(contains-cycle? x) ; #f
(contains-cycle? y) ; #t
(contains-cycle? z) ; #t
