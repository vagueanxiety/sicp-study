#lang sicp
(define (square x)
    (* x x)
)
(define pi 3.1415926)
    
(define (radian2degree x)
    (* (/ x pi) 180)
)

(define (degree2radian x)
    (* (/ x 180.0) pi))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


;; Ben (rectangular)

(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (radian2degree (atan (imag-part-rectangular z)
        (real-part-rectangular z))))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))


;; Alyssa (polar)

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (degree2radian (angle-polar z)))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (degree2radian (angle-polar z)))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (radian2degree (cdr z)))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r (degree2radian a))))


;; Generic selectors

(define (real-part z)
  (cond ((rectangular? z) 
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))


(define (add x y)
    (make-from-real-imag-rectangular (+ (real-part x) (real-part y)) (+ (imag-part x) (imag-part y))))

(define (negate x)
    (make-from-real-imag-rectangular (- (real-part x)) (- (imag-part x))))

; inputs and ouputs are in degree rather than radian 
(define a1 (make-from-real-imag-rectangular 17 23))
(define a2 (make-from-real-imag-rectangular -26 17))
(real-part (add a1 a2))
(imag-part (add a1 a2))


(define b2 (make-from-mag-ang-polar 13 -143))
(magnitude (add a1 b2))
(angle (add a1 b2))

(define c2 (make-from-real-imag-rectangular -26 17))
(define c1 (make-from-mag-ang-polar 4 38))
(magnitude (add c1 c2))
(angle (add c1 c2))

(define d1 (make-from-real-imag-rectangular -11 -15))
(define d2 (make-from-mag-ang-polar 11 119))
(real-part (add d1 (negate d2)))
(imag-part (add d1 (negate d2)))

(define e2 (make-from-real-imag-rectangular 13 -30))
(define e1 (make-from-mag-ang-polar 6 -11))
(magnitude (add e1 (negate e2)))
(angle (add e1 (negate e2)))