#lang sicp

; helpers
(define is-non-negative-pair 0)
(define is-negative-pair 1)
(define is-oppsite-sign-pair 2)

(define (pair-type x)
(cond ((non-negative-pair? x) is-non-negative-pair)
    ((negative-pair? x) is-negative-pair)
    (else is-oppsite-sign-pair))
)

(define (print-interval i) 
    (newline) 
    (display ": [") 
    (display (lower-bound i)) 
    (display ",") 
    (display (upper-bound i)) 
    (display "]")) 
   


; interval in problem domain (the use)
(define (add-interval x y)
(make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (old-mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
            (p3 (* (upper-bound x) (lower-bound y)))
            (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))

(define (mul-interval x y)
    (let ((xu (upper-bound x))
            (xl (lower-bound x))
            (yu (upper-bound y))
            (yl (lower-bound y))
            (x-type (pair-type x))
            (y-type (pair-type y)))
    (cond ((and (= is-oppsite-sign-pair x-type) (= is-non-negative-pair y-type)) 
                (make-interval (* xl yu) (* xu yu)))
        ((and (= is-non-negative-pair x-type) (= is-non-negative-pair y-type)) 
            (make-interval (* xl yl) (* xu yu)))
        ((and (= is-negative-pair x-type) (= is-negative-pair y-type)) 
            (make-interval (* xl yl) (* xu yu)))
        ((and (= is-negative-pair x-type) (= is-non-negative-pair y-type)) 
            (make-interval (* xl yu) (* xu yl)))
        ((and (= is-non-negative-pair x-type) (= is-oppsite-sign-pair y-type)) 
            (make-interval (* xu yl) (* xu yu)))
        ((and (= is-negative-pair x-type) (= is-oppsite-sign-pair y-type)) 
            (make-interval (* xl yu) (* xl yl)))
        ((and (= is-non-negative-pair x-type) (= is-negative-pair y-type)) 
            (make-interval (* xu yl) (* xl yu)))
        ((and (= is-oppsite-sign-pair x-type) (= is-negative-pair y-type)) 
            (make-interval (* xu yl) (* xl yl)))
        (else (old-mul-interval x y))
        )
    )                
                    
    )

(define (div-interval x y) 
    (let ((p (* (upper-bound y) (lower-bound y))))
        (if (or (< p 0) (= p 0)) 
            (display "Division by an interval that spans zero.\n")
            (mul-interval
                x
                (make-interval (/ 1.0 (upper-bound y))
                                (/ 1.0 (lower-bound y))))))
    )

(define (sub-interval x y)
    (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y))))) 

(define (interval-width x)
    (/ (- (upper-bound x) (lower-bound x)) 2.0))


;(+0 +0)
(define (non-negative-pair? x)
    (>= (lower-bound x) 0 ))

;(- -)
(define (negative-pair? x)
    (< (upper-bound x) 0 ))



; abstraction barriers (interfaces) and the underlying representation of interval
; NOTE that users HAVE TO pass lower bound and upper bound as the first and the second arguments
; Using primitive min and max can remove this restriction. See sicp solutions.
(define (make-interval a b) (cons a b))
(define (upper-bound intvl)
    (cdr intvl))
(define (lower-bound intvl)
    (car intvl))

; testing 
; NOTE that the testing program and data below is adapted from jz at http://community.schemewiki.org/?sicp-ex-2.11

  
(define (eql-interval? a b) 
    (and (= (upper-bound a) (upper-bound b)) 
         (= (lower-bound a) (lower-bound b)))) 


(define (ensure-mult-works aH aL bH bL) 
    (let ((a (make-interval aL aH)) 
          (b (make-interval bL bH)))
    (newline)
    (newline)
    (print-interval a)
    (print-interval b)
    (newline) 
    (if (eql-interval? (old-mul-interval a b) 
                       (mul-interval a b)) 
        true 
        (error "new mult returns different value!"  
               a
               (pair-type a)
               b
               (pair-type b) 
               (old-mul-interval a b) 
               (mul-interval a b))))) 


  
(ensure-mult-works  +10 +10   +10 +10) 
(ensure-mult-works  +10 +10   +10 +00) 
(ensure-mult-works  +10 +10   +00 +00) 
(ensure-mult-works  +10 +10   +10 -10) 
(ensure-mult-works  +10 +10   +00 -10) 
(ensure-mult-works  +10 +10   -10 -10) 
    
(ensure-mult-works  +10 +00   +10 +10) 
(ensure-mult-works  +10 +00   +10 +00 ) 
(ensure-mult-works  +10 +00   +00 +00) 
(ensure-mult-works  +10 +00   +10 -10) 
(ensure-mult-works  +10 +00   +00 -10) 
(ensure-mult-works  +10 +00   -10 -10) 
    
(ensure-mult-works  +00 +00   +10 +10) 
(ensure-mult-works  +00 +00   +10 +00) 
(ensure-mult-works  +00 +00   +00 +00) 
(ensure-mult-works  +00 +00   +10 -10) 
(ensure-mult-works  +00 +00   +00 -10) 
(ensure-mult-works  +00 +00   -10 -10) 
    
(ensure-mult-works  +10 -10   +10 +10) 
(ensure-mult-works  +10 -10   +10 +00) 
(ensure-mult-works  +10 -10   +00 +00) 
(ensure-mult-works  +10 -10   +10 -10) 
(ensure-mult-works  +10 -10   +00 -10) 
(ensure-mult-works  +10 -10   -10 -10) 
    
(ensure-mult-works  +00 -10   +10 +10) 
(ensure-mult-works  +00 -10   +10 +00) 
(ensure-mult-works  +00 -10   +00 +00) 
(ensure-mult-works  +00 -10   +10 -10) 
(ensure-mult-works  +00 -10   +00 -10) 
(ensure-mult-works  +00 -10   -10 -10) 
    
(ensure-mult-works  -10 -10   +10 +10) 
(ensure-mult-works  -10 -10   +10 +00) 
(ensure-mult-works  -10 -10   +00 +00) 
(ensure-mult-works  -10 -10   +10 -10) 
(ensure-mult-works  -10 -10   +00 -10) 
(ensure-mult-works  -10 -10   -10 -10) 