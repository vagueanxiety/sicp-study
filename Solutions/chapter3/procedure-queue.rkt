#lang sicp

(define (front-ptr queue) (queue 'front-ptr))
(define (rear-ptr queue) (queue 'rear-ptr))

(define (set-front-ptr! queue item)
  ((queue 'set-front-ptr!) item))

(define (set-rear-ptr! queue item)
  ((queue 'set-rear-ptr!) item))

(define (empty-queue? queue)
  ((queue 'empty-queue?)))


(define (make-queue)
  (let ((front-ptr '() )
        (rear-ptr '() ))

    (define (set-front-ptr! item)
      (set! front-ptr item))

    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (empty-queue?)
      (null? front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'empty-queue?) empty-queue?)
            (else (error "Undefined
                         operation: QUEUE" m))))

    dispatch))


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
; queue, because of the way empty-queue/insert-queue is implemented
;
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

;;
; STRUCTURE 
;
; high level interfaces: insert/delete/empty?/front/make-queue
; What defines a data structure
; What users care
;
; ----------------------------------------------------------------
;
; low level interfaces: set-rear/set-front/get-rear/get-front
;
; Note: the implementations of these low level interfaces may depend on the
; choice of representation 
;
; ----------------------------------------------------------------
;
; representations
;

(define q1 (make-queue))

(print-queue (insert-queue! q1 'a))

(print-queue (insert-queue! q1 'b))

(print-queue (delete-queue! q1))

(print-queue (delete-queue! q1))
