#lang sicp
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))


(define (make-queue) (cons '() '()))


(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an
           empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue)
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

;;
; note that we didn't move the rear pointer when deleting an element from the
; queue, because of the way empty-queue is implemented.
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with
                an empty queue" queue))
        (else (set-front-ptr!
                queue
                (cdr (front-ptr queue)))
              queue)))


(define (print-queue q)
  (if (empty-queue? q)
    (begin (display '()) (display "\n"))
    (begin (display (front-ptr q)) (display "\n")))
  )



(define q1 (make-queue))


(print-queue (insert-queue! q1 'a))

(print-queue (insert-queue! q1 'b))

(print-queue (delete-queue! q1))

(print-queue (delete-queue! q1))