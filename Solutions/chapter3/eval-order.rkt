#lang sicp

(define (make-f)
  (let ((called false))
    (define (f x)
      (if (eq? called true) 0
        (begin (set! called true)
               x)
        ))
    f
    )
  )

(define f (make-f))

(f 0)
(f 1)

;(f 1)
;(f 0)


; from http://community.schemewiki.org/?sicp-ex-3.8
(define f1
  (let ((called #f))
    (lambda (x)
      (if called
        0
        (begin
          (set! called #t)
          x)))))

(f1 0)
(f1 1)

;; vs
(define f2
  (lambda (x)
    (let ((called #f))
      (if called
        0
        (begin
          (set! called #t)
          x)))))

(f2 0)
(f2 1)

;; what makes these two implementations different and why?
;; i.e. how is f1 and f2 is evaled and applied to args?
; how is define keyword related to environment?
;
