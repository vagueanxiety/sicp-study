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
; after reading sec 3.2:
; when f1 is evaluated, a procedure is applied first => a frame is created with
; arg called, then the body is evaluated which produces a procedure object
; whose env part points to the aforementioned frame, and the procedure object
; is bound to f1 in the global env
;
; let arg <- f2 arg <- ...
;         (destroyed .....)
; therefore, set will changed the let arg and the change is retained
;
;
; However, f2 is simply producing a procedure object without that extra frame
; for called. That frame is created only when f2 is called.
;
; f2 arg <- let arg <- ...
; (destroyed  ...........)
; after f2 is called the frame that contains let arg has been destroyed
;
; **since the procedure call that constructed it has terminated, and
; there are no pointers to that frame from other parts of the environment.**


