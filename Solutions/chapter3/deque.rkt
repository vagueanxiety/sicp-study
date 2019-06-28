#lang sicp
;; constructors, selectors, mutators
;

;make-deque,

;empty-deque?,

;front-deque, rear-deque,

;front-insert-deque!, rear-insert-deque!,

;front-delete-deque!, rear-delete-deque!.


(define (front-ptr deque) (deque 'front-ptr))
(define (rear-ptr deque) (deque 'rear-ptr))

(define (set-front-ptr! deque item)
  ((deque 'set-front-ptr!) item))

(define (set-rear-ptr! deque item)
  ((deque 'set-rear-ptr!) item))


(define (make-deque)
  (let ((front-ptr '() )
        (rear-ptr '() ))

    (define (set-front-ptr! item)
      (set! front-ptr item))

    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (empty-deque?)
      (and (null? front-ptr) (null? rear-ptr)))

    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'empty-deque?) empty-deque?)
            (else (error "Undefined
                         operation: deque" m))))
    dispatch))

;; interfaces/definition
(define (empty-deque? deque)
  ((deque 'empty-deque?)))

(define (front-deque deque)
  (if (empty-deque? deque)
    (error "FRONT called with an
           empty deque" deque)
    (cdar (front-ptr deque))
    ))

(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "REAR called with an
           empty deque" deque)
    (cdar (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons (cons '() item) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
            (begin
              (set-car! (car (front-ptr deque)) new-pair)
              (set-cdr! new-pair (front-ptr deque))
              (set-front-ptr! deque new-pair)
              deque)
            )
          )))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (cons '() item) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
            (begin
              (set-car! (car new-pair) (rear-ptr deque))
              (set-cdr! (rear-ptr deque) new-pair)
              (set-rear-ptr! deque new-pair)
              deque)
            )
          )))


(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with
                an empty deque" deque))
        (else
          (let ((next-pair (cdr (front-ptr deque))))
            (if (null? next-pair)
              (begin ;; deque will be empty after this op
                (set-front-ptr! deque next-pair)
                (set-rear-ptr! deque next-pair)
                deque
                )
              (begin
                (set-cdr! (front-ptr deque) '())
                (set-front-ptr! deque next-pair)
                (set-car! (car (front-ptr deque)) '())
                deque
                )
              )
            )
          )))


(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with
                an empty deque" deque))
        (else
          (let ((prev-pair (caar (rear-ptr deque))))
            (if (null? prev-pair)
              (begin ;; deque will be empty after this op
                (set-rear-ptr! deque prev-pair)
                (set-rear-ptr! deque prev-pair)
                deque
                )
              (begin
                (set-car! (car (rear-ptr deque)) '())
                (set-rear-ptr! deque prev-pair)
                (set-cdr! (rear-ptr deque) '())
                deque
                )
              )
            )
          )))


(define (print-deque q)
  (define (iter-display front-ptr)
    (if (null? front-ptr)
      (display "")
      (begin
        (display (cdar front-ptr))
        (display " ")
        (iter-display (cdr front-ptr))
        )
      )
    )

  (if (empty-deque? q)
    (begin (display '()) (display "\n"))
    (begin 
      (display "(")
      (iter-display (front-ptr q))
      (display ")")
      (display "\n")
      )
    )

  )


(define q1 (make-deque))

;; test print edge case 
(print-deque q1)

;; test emptiness
(empty-deque? q1)
(print-deque (rear-insert-deque! q1 'r1))
(empty-deque? q1)

;; test front and rear insert
(print-deque (front-insert-deque! q1 'f1))
(print-deque (rear-insert-deque! q1 'r2))

;; test front and rear delete
(print-deque (front-delete-deque! q1))
(print-deque (rear-delete-deque! q1))
(empty-deque? q1)

;; test refilling the deque
(print-deque (front-delete-deque! q1))
(print-deque (front-insert-deque! q1 'f2))
(print-deque (front-insert-deque! q1 'f3))
(print-deque (front-insert-deque! q1 'f4))
(print-deque (rear-insert-deque! q1 'r3))
(print-deque (rear-insert-deque! q1 'r4))
(print-deque (rear-insert-deque! q1 'r5))
(empty-deque? q1)

