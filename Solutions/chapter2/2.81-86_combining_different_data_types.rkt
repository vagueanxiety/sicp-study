#lang sicp

(define (filter predicate sequence) 
    (cond   ((null? sequence) nil)
            ((predicate (car sequence))
                (cons (car sequence) (filter predicate (cdr sequence))))
            (else (filter predicate (cdr sequence)))))


;;;SECTION 2.5.2

;; to be included in the complex package
;: (define (add-complex-to-schemenum z x)
;:   (make-from-real-imag (+ (real-part z) x)
;:                        (imag-part z)))
;: 
;: (put 'add '(complex scheme-number)
;:      (lambda (z x) (tag (add-complex-to-schemenum z x))))


;; Coercion

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;(put-coercion 'scheme-number 'complex scheme-number->complex)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))
;81
;a. an infinite loop on (apply-generic op a1 a2)
;b. apply-generic works correctly, because it will return an error if an operation is not found for a type  
;c. add another condition after the second if statement, so that an error will be returned if proc is nil and 
; two args are of the same type. 
; this solution is written by meteorgan at http://community.schemewiki.org/?sicp-ex-2.81
(define (apply-generic op . args) 
    (define (no-method type-tags) 
      (error "No method for these types" 
        (list op type-tags))) 
   
    (let ((type-tags (map type-tag args))) 
      (let ((proc (get op type-tags))) 
        (if proc 
            (apply proc (map contents args)) 
            (if (= (length args) 2) 
                (let ((type1 (car type-tags)) 
                      (type2 (cadr type-tags)) 
                      (a1 (car args)) 
                      (a2 (cadr args))) 
                  (if (eq? type1 type2) 
                    (no-method type-tags) 
                    (let ((t1->t2 (get-coercion type1 type2)) 
                          (t2->t1 (get-coercion type2 type1)) 
                          (a1 (car args)) 
                          (a2 (cadr args))) 
                      (cond (t1->t2 
                             (apply-generic op (t1->t2 a1) a2)) 
                            (t2->t1 
                             (apply-generic op a1 (t2->t1 a2))) 
                            (else (no-method type-tags)))))) 
                (no-method type-tags)))))) 

;82
(define (apply-generic op . args) 
    (define (same-type? type-tags)
        (if (null? type-tags)
            false
            (let ((number-of-tags (length type-tags)) (head (car type-tags)))
                (= number-of-tags (length (filter (lambda (x) (eq? head x)) type-tags))))))
    
    (define (no-method type-tags) 
      (error "No method for these types" 
        (list op type-tags))) 

    (define (coerce-to-x x) 
        (lambda (y) (
            (cond 
                ((eq? x y) (lambda (x) x))
                (else (get-coercion y x))))))

    (define (map-to-coercions type-tags type) (map (coerce-to-x type) type-tags))
    (define (apply-coersions coercions coercees)
        (cond ((null? coercions) '())
            (else (cons 
                ((car coercions) (car coercees)) 
                (apply-coercions (cdr coercions) (cdr coercees))))))

    (define (coercing-args-and-apply args type-tags op)
        (define (try-iter rest-of-type-tags)
            (if (null? rest-of-type-tags) 
                    (no-method type-tags)
                    (let ((type (car rest-of-type-tags)))
                        (let ((coercions (map-to-coercions type-tags type)))
                            (if (> (length (filter null? coercions)) 0)
                                (try-iter (cdr rest-of-type-tags))
                                (let ((converted-args (apply-coercions coercions args)))
                                    (apply apply-generic (cons op converted-args)))) 
                            ))))
        (try-iter type-tags))
   
    (let ((type-tags (map type-tag args))) 
      (let ((proc (get op type-tags))) 
        (if proc 
            (apply proc (map contents args))
            (if (same-type? type-tags)
                    (no-method type-tags)
                    (coercing-args-and-apply args type-tags op)) 
            )))) 
; this strategy is not general in the case where all types are subtypes of another type 


; 83
(define (raise x) (apply-generic 'raise x))

; in integer package
(define (raise-integer-to-rational x) (make-rational x 1))
(put 'raise 'integer raise-integer-to-rational)

; in rational number package
(define (raise-rational-to-real x) (make-real (/ (numer x) (denom x))))
(put 'raise 'rational raise-rational-to-real) 
; assume that make-real has this kind of constructor

; in real number package
(define (raise-real-to-complex x) (make-complex-from-real-imag x 0))
(put 'raise 'real raise-real-to-complex) 


; another version of raise written by meteorgan at http://community.schemewiki.org/?sicp-ex-2.84
; raise s into t, if success, return s; else return #f 
(define (raise-into s t) 
(let ((s-type (type-tag s)) 
        (t-type (type-tag t))) 
    (cond ((equal? s-type t-type) s) 
    ((get 'raise (list s-type))
            (raise-into ((get 'raise (list s-type)) (contents s)) t)) 
        (else #f)))) 



; 84 note that this is not tested
(define (apply-generic op . args) 
    (define (compare-level a1 a2)
        (- (level a1) (level a2)))

    ; level starts at 0, and level 0 is higher than level 1.
    (define (level arg)
        (let ((type (type-tag arg))
              (content (contents arg)))
              (let ((proc (get 'raise type)))
                  (if (null? proc)
                    0                
                    (+ 1 (level (proc content)))))
            ))
    (define (same-type? type-tags)
        (if (null? type-tags)
            false
            (let ((number-of-tags (length type-tags)) (head (car type-tags)))
                (= number-of-tags (length (filter (lambda (x) (eq? head x)) type-tags))))))
    
    (define (no-method type-tags) 
      (error "No method for these types" 
        (list op type-tags))) 

; assuming there are only two arguments and 
; they are in the same "tower".
    (let ((type-tags (map type-tag args))) 
      (let ((proc (get op type-tags))) 
        (if proc 
            (apply proc (map contents args))
            (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                    (let ((level-diff (compare-level a1 a2)))
                        (cond ((= 0 level-diff) (no-method type-tags))
                            ((> level-diff 0) (apply-generic op (raise a1) a2))
                            ((< level-diff 0) (apply-generic op a1 (raise a2))))))
              (error "No method for these types"
                     (list op type-tags)))))))




;85
; project
(define (project x) (apply-generic 'project x))

; in integer package
(define (project-rational-to-integer x) (make-scheme-number (round (/ (numer x) (denom x)))))
(put 'project 'rational project-rational-to-integer)

; in rational number package
; adapted from meteorgan's package
(define (project-real-to-rational x) 
    (let ((rat (rationalize (inexact->exact x) 1/100))) 
        (make-rational (numerator rat) (denominator rat))))

(put 'project 'real project-real-to-rational) 
; assume that make-real has this kind of constructor
; dont know how to project real to rational 

; in real number package
(define (project-complex-to-real x) (make-real (real-part x)))
(put 'project 'complex project-complex-to-real) 



; equality
; already implemented in 77-80


; drop
; a drop procedure that drops an object as far as possible
(define (drop obj)
    (let ((type (type-tag obj)) (content (contents obj)))
        (let ((proc (get 'project type)))
            (if (null? proc) 
                obj
                (let ((projected-obj (proc content)))
                    (let ((raised-projected-obj (raise projected-obj)))
                        (if (equ? raised-projected-obj obj) 
                            (drop raised-projected-obj)
                            obj))))))) 

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


; 87
; change +-*/ to generic counterparts
; define sin and cos for not only ordinary numbers but also 
; rational numbers
;  (put 'sine 'rational (lambda (x) (sin (/ (numer x) (denom x)))))  
;  (put 'cosine 'rational (lambda (x) (cos (/ (numer x) (denom x)))))