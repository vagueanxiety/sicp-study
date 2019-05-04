(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined
                       operation: CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define x (cons 1 2))
(define z (cons x x))

(set-car! (cdr z) 17)
;;
; z is a procedure
; (cdr z) e0: z
; (z 'car) e1: 'car
; evaluate the cond body
; x is returned
; (set-car! x 17) e2: x, 17 
; evaluate its body
; (z 'set-car!) e3: 'set-car!
; procedure set-x! is returned
; set-x! 
; z is returned


(car x)
17
