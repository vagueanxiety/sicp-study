#lang sicp

(define (display-list content)
    (display content)
    (newline))

(define (deriv exp var) 
    (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0)) 
        ((sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
        ((product? exp)
            (make-sum
            (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                                    (multiplicand exp))))
        ((exponentiation? exp)
            (make-product (make-product (exponent exp) 
                                        (make-exponentiation (base exp) (make-sum (exponent exp) -1))) 
                          (deriv (base exp) var)))
        (else (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))




(define (sum? x) 
    (cond 
        ((null? x) false)
        ((eq? (car x) '+ ) true)
        (else (sum? (cdr x)))))
        
(define (addend s) 
    (define (iter s)
        (cond 
            ((eq? (car s) '+ ) '())
            (else (cons (car s) (iter (cdr s))))))

    (if (eq? (cadr s) '+ ) 
            (car s)
            (iter s)))
            
(define (augend s) 
    (cond 
        ((eq? (car s) '+ ) 
            (if (null? (cddr s)) 
                (cadr s) 
                (cdr s)))
        (else (augend (cdr s)))))

; my solution only works for sum and product with two args
; a better solution is written by sgm at http://community.schemewiki.org/?sicp-ex-2.58
; but sgm's solution does simplify terms in cases such as:
; (display-list (deriv '(* x 4 5) 'x))
; but that is make-product's problem
; we may change make-product and make-sum to recursively simplify terms rather than only at 
; the surface level
        
(define (make-sum a1 a2) 
    (cond ((=number? a1 0) a2) 
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((or (product? a2) (exponentiation? a2)) (append (list a1 '+) a2))
        ((or (product? a1) (exponentiation? a1)) (append a1 (list '+ a2)))
        (else (list a1 '+ a2))))

(define (product? x) (and (pair? x) (eq? (cadr x) '* ) (null? (cdddr x))))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2)) 
            (else (list m1 '* m2))))



(define (exponentiation? x) (and (pair? x) (eq? (cadr x) '**)))
(define (base s) (car s))
(define (exponent s) (caddr s))
(define (make-exponentiation base exponent) 
    (cond ((=number? exponent 0) 1) 
        ((=number? exponent 1) base)
        (else (list base '** exponent))))





(display-list (make-exponentiation 3 4))
(display-list (make-exponentiation 3 1))
(display-list (make-exponentiation 3 0))

(newline)
(base (make-exponentiation 3 4))
(exponent (make-exponentiation 3 4))
(exponentiation? (make-exponentiation 3 4))

(newline)
(display-list (deriv '(x ** 5) 'x))
(display-list (deriv '(x ** 0) 'x))
(display-list (deriv '(x ** 1) 'x))

(newline)
(display-list (deriv '(y ** 8) 'x))
(display-list (deriv '(x ** y) 'x))
(display-list (deriv '((x + 2) ** 2) 'x))
(display-list (deriv '((x * 2) ** 2) 'x))

; new constructors for question b
(newline)
(display-list (make-sum (make-product 8 'x) 'y))
(display-list (make-sum (make-exponentiation 'x 8) 'y))
(display-list (make-sum (make-product 8 'x) (make-product 8 'x)))
(display-list (make-sum 'y (make-product 8 'x)))
(display-list (make-sum 'y (make-exponentiation 'x 8)))


; new selectors for question b
(newline)
(display-list (augend (make-sum (make-product 8 'x) 'y)))
(display-list (augend (make-sum (make-exponentiation 'x 8) 'y)))
(display-list (augend (make-sum 'y (make-product 8 'x))))
(display-list (augend (make-sum 'y (make-exponentiation 'x 8))))
(display-list (augend (make-sum (make-product 8 'x) (make-product 8 'x))))
(display-list (augend '(8 * x + x * 8)))
(display-list (augend '(x + x * 8)))
(display-list (augend '(x ** 8 + x * 8)))

(newline)
(display-list (addend '(8 * x + x * 8)))
(display-list (addend '(x + x * 8)))
(display-list (addend '(x ** 8 + x * 8)))

(newline)
(display-list (sum? '(8 * x + x * 8)))
(display-list (sum? '(x + x * 8)))
(display-list (sum? '(x ** 8 + x * 8)))

(newline)
(display-list (product? '(8 * x + x * 8)))
(display-list (product? '(x + x * 8)))
(display-list (product? '(x ** 8 + x * 8)))
(display-list (product? '(x * 8)))

(newline)
(display-list (deriv '(x + 3 * (x + y + 2)) 'x ))
(display-list (deriv '((8 * x) + 3 * (x + y + 2)) 'x ))
(display-list (deriv '(8 * x + 3 * (x + y + 2)) 'x ))
(display-list (deriv '(8 * x + y) 'x )  )
(display-list  (deriv '(8 * x) 'x ) )
(display-list  (deriv '((x * y) * (x + 3)) 'x ) )
(display-list  (deriv '(x * (y * (x + 3))) 'x) )