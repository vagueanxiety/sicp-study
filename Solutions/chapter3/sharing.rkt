#lang sicp
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2
  (cons (list 'a 'b) (list 'a 'b)))

z1
(set-to-wow! z1)
z2
(set-to-wow! z2)

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

;;
; 3: normal three-pair list
; 4: three-pair list but the last pair is shared. It is both the car and cdr
; of the second to last pair
; 7: three-pair list but the car and cdr of the first pair is the second
; pair, and the car and cdr of the second pair is the third pair
; Never return at all: the cdr of the last pair points to the first pair
; http://community.schemewiki.org/?sicp-ex-3.16


(define str1 '(foo bar baz))
;(count-pairs str1)

(define v '(foo))
(define y (cons v v))
(define str2 (list y))
;(count-pairs str2)

(define m '(foo))
(define n (cons m m))
(define str3 (cons n n))
;(count-pairs str3)

(define str4 '(foo bar baz))
(set-cdr! (cddr str4) str4)
;(count-pairs str4)


(define my-count-pairs
  (let ((met '()))
    (lambda (x)
      (if (and (pair? x)
               (not (contains? met x)))
        (begin
          (set! met (cons x met))
          (+ (my-count-pairs (car x))
             (my-count-pairs (cdr x))
             1)
          )
        0
        ))))

(define (contains? list element)
  (cond ((null? list) false)
        ((eq? (car list) element) true)
        (else (contains? (cdr list) element)))
  )

(my-count-pairs str1)
(my-count-pairs str2)
(my-count-pairs str3)
(my-count-pairs str4)


(define has-cycle?
  (let ((met '()))
    (lambda (x)
      (cond ((null? x) false)
            ((not (contains? met x))
             (begin
               (set! met (cons x met))
               (has-cycle? (cdr x))
               )
             )
            (else true))

      )))


(has-cycle? str4)
(has-cycle? '())
(has-cycle? '(d d f f))


