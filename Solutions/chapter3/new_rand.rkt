#lang sicp

(define (make-my-rand random-init)
  (let ((x random-init))
    (define (generate)
      (begin (set! x (rand-update x)) x))

    (define (reset_as val)
      (begin (set! x val) x))

    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset_as)
            (else (error "Unknown request:
                         MAKE-MY-RAND" m))))
    dispatch
    )
 )

;; from https://github.com/uents/sicp/blob/master/ch3/ch3.1.scm
(define (rand-update x)
  (modulo (+ (* 13 x) 47) 97))

(define new-rand (make-my-rand 0))
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
((new-rand 'reset) 0)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
((new-rand 'reset) 11)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
((new-rand 'reset) 12)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
((new-rand 'reset) 11)
(new-rand 'generate)
(new-rand 'generate)
(new-rand 'generate)
