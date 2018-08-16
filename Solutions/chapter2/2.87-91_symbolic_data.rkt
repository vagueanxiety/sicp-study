#lang sicp
;; complete code in this section
;;;SECTION 2.5.3

;;; ALL procedures in 2.5.3 except make-polynomial
;;; should be inserted in install-polynomial-package, as indicated

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

;; *incomplete* skeleton of package
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;;[procedures same-variable? and variable? from section 2.3.2]

  ;; representation of terms and term lists
  ;;[procedures adjoin-term ... coeff from text below]

  ;;(define (add-poly p1 p2) ... )
  ;;[procedures used by add-poly]

  ;;(define (mul-poly p1 p2) ... )
  ;;[procedures used by mul-poly]

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))


;; Representing term lists

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))


;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


; 87
; in polynomial package
(put '=zero? 'polynomial (lambda (p) (empty-termlist? (term-list p))))

; 88
; generic negation operations for all types
(define (negation x y) (apply-generic 'negation x y))
; in complex package
(put 'negation 'complex (lambda (c) (tag (make-from-real-imag (- (real-part c)) (- (imag-part c))))))

; in ordinary number package
(put 'negation 'scheme-number (lambda (n) -n))

; in rational package
(put 'negation 'rational (lambda (r) (tag (make-rat (- (numer r))(denom r)))))

; in polynomial package 
(put 'negation 'polynomial 
    (lambda (p) (tag 
        (make-polynomial (variable p) 
            (map (lambda (term) (make-term (order term) (negation (coeff term)))) 
                (term-list p))))))
      


(put 'subtract '(polynomial polynomial) 
    (lambda (p1 p2) (
        (add p1 (negation p2))
    )))

;; Representing dense term lists

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (the-empty-termlist) '())
(define (empty-termlist? term-list) (null? term-list))
(define (rest-terms term-list) (cdr term-list))

(define (first-term term-list) 
    (make-term (length (rest-terms term-list)) (car term-list)))
; this solution is written by hattivat at https://web.archive.org/web/20171020083232/http://community.schemewiki.org:80/?sicp-ex-2.89
(define (adjoin-term term term-list)
    (cond ((=zero? term) term-list)   ; need to add =zero? in term package
        ((=equ? (order term) (length term-list)) (cons (coeff term) term-list)) 
        (else (adjoin-term term (cons 0 term-list))))) ; need to add zeros in between 




; 90 generic operations on polynomial representations
(define (make-term order coeff) (attach-tag 'term (list order coeff)))
(define (order term) (car (contents term)))
(define (coeff term) (cadr (contents term)))

(define (install-dense-poly-package)
    ; internal procedures
    (define (the-empty-termlist) '())
    (define (empty-termlist? term-list) (null? term-list))

    (define (rest-terms term-list) (cdr term-list))
    (define (first-term term-list) 
        (make-term (length (rest-terms term-list)) (car term-list)))
    (define (adjoin-term term term-list)
        (cond ((=zero? term) term-list)   ; need to add =zero? in term package
            ((=equ? (order term) (length term-list)) (cons (coeff term) term-list)) 
            (else (adjoin-term term (cons 0 term-list)))))
    
    ;; interface to the rest of the system
    (define (tag x) (attach-tag 'dense-poly x)) 
    (put 'first-term 'dense-poly first-term) 
    (put 'rest-terms 'dense-poly rest-terms)
    (put 'adjoin-term '(term dense-poly) (lambda (x y) (tag (adjoin-term x y))))
    (put 'empty-termlist? 'dense-poly empty-termlist?) 'done)




(define (install-sparse-poly-package)
    ;; internal procedures
    (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

    (define (the-empty-termlist) '())
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))

    ;; interface to the rest of the system
    (define (tag x) (attach-tag 'sparse-poly x)) 
    (put 'first-term 'sparse-poly first-term) 
    (put 'rest-terms 'sparse-poly rest-terms)
    (put 'adjoin-term '(term sparse-poly) (lambda (x y) (tag (adjoin-term x y))))
    (put 'empty-termlist? 'sparse-poly empty-termlist?) 'done)


(define (first-term term-list) (apply-generic 'first-term term-list)) 
(define (rest-terms term-list) (apply-generic 'rest-terms term-list)) 
(define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list)) 
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list)) 



;; EXERCISE 2.91

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     ;compute rest of result recursively
                        (div-terms 
                            (add-terms 
                                L1 
                                   (map 
                                    (lambda (term) (make-term (order term) (negation (coeff term)))) 
                                    (mul-terms 
                                        L2 
                                        (adjoin-term (make-term new-o new-c) the-empty-termlist)))) 
                        L2)
                     ))
                ;form complete result
                (let ((quotient-list (car rest-of-result)) 
                    (remainder-list (cadr rest-of-result))
                    (quotient-term (make-term new-o new-c)))
                    (list (adjoin-term quotient-term quotient-list) remainder-list)) 
                ))))))
(define (div-poly dividend divisor)
    (if (same-variable? (variable dividend) (variable divisor))
        (let ((quotient-remainder-termlist (div-terms (term-list dividend) (term-list divisor))))
            (list 
                (make-poly (variable dividend) (car quotient-remainder-termlist)) 
                (make-poly (variable dividend) (cadr quotient-remainder-termlist))))
        (error "Two polies do not have the same variable" (list dividend divisor))))



        (list (variable dividend) (div-terms (term-list dividend) (term-list divisor)))