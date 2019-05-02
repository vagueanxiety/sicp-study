#lang sicp


(define (make-monitored f)

  (define (internal-make-monitored f count)
    (define (apply x)
      (begin (set! count
               (+ count 1))
             (f x)))

    (define (reset-count)
      (begin (set! count 0)
             count))

    (define (dispatch m)
      (cond
        ((eq? m 'how-many-calls?) count)
        ((eq? m 'reset-count) (reset-count))
        (else (apply m))))

    dispatch)

  (internal-make-monitored f 0))


(define s (make-monitored sqrt))
(s 'how-many-calls?)
(s 100)
(s 'how-many-calls?)
(s 100)
(s 100)
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
(s 100)
(s 'how-many-calls?)
(s 'how-many-calls?)
