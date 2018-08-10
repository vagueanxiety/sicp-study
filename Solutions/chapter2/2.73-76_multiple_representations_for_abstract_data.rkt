#lang sicp
(define (accumulate op initial sequence) 
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (attach-tag type-tag contents) 
    (cons type-tag contents))
(define (install-polar-package)
    ;; internal procedures
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z) (* (magnitude z) (cos (angle z)))) 
    (define (imag-part z) (* (magnitude z) (sin (angle z)))) 
    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y)))
            (atan y x)))
    ;; interface to the rest of the system
    (define (tag x) (attach-tag 'polar x)) 
    (put 'real-part '(polar) real-part) 
    (put 'imag-part '(polar) imag-part) 
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y)))) 
    (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a)))) 'done)


(define (install-rectangular-package)
    ;; internal procedures
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y)) 
    (define (magnitude z)
        (sqrt (+ (square (real-part z))
                 (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a))))
    ;; interface to the rest of the system
    (define (tag x) (attach-tag 'rectangular x)) 
    (put 'real-part '(rectangular) real-part) 
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
        (lambda (r a) (tag (make-from-mag-ang r a)))) 'done)

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags))) 
            (if proc
                (apply proc (map contents args))
                (error "No method for these types: APPLY-GENERIC" (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z)) 
(define (imag-part z) (apply-generic 'imag-part z)) 
(define (magnitude z) (apply-generic 'magnitude z)) 
(define (angle z) (apply-generic 'angle z))


;2.73
;a. Because in this case, the type tags are arithmetic operators.
;b and c. adapted from http://community.schemewiki.org/?sicp-ex-2.73

(define (install-sum-package parameters)
    (define (inner-make-sum a1 a2) (cons a1 a2)) 
    (define (addend s) (car s)) 
    (define (augend s) (cdr s)) 
    (define (deriv-sum s var) 
        (make-sum (deriv (addend s) var) (deriv (augend s) var))) 
    
    ;; interface to the rest of the system
    (define (tag x) (attach-tag '+ x)) 
    (put 'deriv '+ deriv-sum) 
    (put 'make-sum '+ 
            (lambda (x y) (tag (inner-make-sum x y)))) 
    'done) 
    
(define (install-product-package)
    ;; internal procedures
    (define (multiplier p) (car p))
    (define (multiplicand p) (cdr p)) 
    (define (inner-make-product m1 m2) (cons a1 a2))

    (define (deriv-product p var)
        (make-sum 
            (make-product (multiplier p) (deriv (multiplicand p) var)) 
            (make-product (deriv (multiplier p) var) (multiplicand p))))

    ;; interface to the rest of the system
    (define (tag x) (attach-tag '* x)) 
    (put 'deriv '* deriv-product) 
    (put 'make-product '*
        (lambda (m1 m2) (tag (inner-make-product m1 m2)))) 'done)

(define (install-exponentiation-package)
    ;; internal procedures
    (define (base e) (car e))
    (define (exponent e) (cdr e)) 
    (define (inner-make-exponentiation base exponent) (cons base exponent))

    (define (deriv-exponentiation e var)
        (make-product
                (make-product (exponent e) 
                (make-exponentiation (base e) (make-sum (exponent e) -1))) 
                        (deriv (base exp) var)))

    ;; interface to the rest of the system
    (define (tag x) (attach-tag '** x)) 
    (put 'deriv '** deriv-exponentiation) 
    (put 'make-exponentiation '**
        (lambda (m1 m2) (tag (inner-make-exponentiation m1 m2)))) 'done)

(define (make-sum x y) 
    ((get 'make-sum '+) x y)) )
(define (make-product x y) 
    ((get 'make-product '*) x y)) )
(define (make-exponentiation x y) 
    ((get 'make-exponentiation '**) x y)) )

;d
; change the order of the operator symbol and inner procedure name when adding new entries


;2.74
; get-record
; supplying all-records, which is a single file tagged by something that 
; identifies a division, and 'way-to-getting-record, which will returns a procedure for a specified division 
; that takes in an employee's name and returns his or her tagged record.

; get-salary
; supplying a record of a specified employee, which is tagged by something that identifies a division
; , and 'way-to-getting-salary.
 
; a similar solution is written by palatin at http://community.schemewiki.org/?sicp-ex-2.74
; since division is the "bad ass" that makes operation different, we could set up the table such
; that "operations" are different division, "types" are higher-order procedure (way-to...), entries are 
; the actual selectors in a specified division
; The new company needs to install their own package of all selectors. 