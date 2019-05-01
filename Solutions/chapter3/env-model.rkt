(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;;
; procedure factorial is created with text and a pointer to the global factorial 
; When factorial is applied to n, a new frame is created with argument n.
; Then depends on the if condition, new (different) frames will be created to computer
; factorial n-1



(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product 
                   counter 
                   max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;;
; factorial: a frame is created wtih arg n and it points to the global env (the
; env in which the procedure is created).
; Then the body is evaluated based the env.
; fact-iter: similar to the recursive factorial? (could be optimized to reuse
; the frame i guess)
;





(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))))


(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))


;;
; a frame is created with initial-amount 
; then the let expression gets evaluated
; evaluating let expression => application of a procedure (anynomous? unbound?)
;
; creating new frames with bindings of formal parameters and arguments
; evaluating the body based on the newly created environment
; since the body itself is a lambda expression, evaluation will produce a
; procedure object, whose env contains those formal params and args
;
; finally the procedure object is bound to W1 in the global environment
;
;
;
; global <- E1: initial amount <- E2: balance <- W1 env pointer
; global <- E3: initial amount <- E4: balance <- W2 env pointer
;
; different from the non-let version:
; an extra frame is used
; set! affects only balance but not initial amount
; 
;
;


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance 
                        amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                        MAKE-ACCOUNT" 
                       m))))
  dispatch)

(define acc (make-account 50))
;;
; 1. frame A created for balance = 50, the body of make-account is eval'ed
; 2. internal procedure objects are bound to frame A
; 3. dispatch is returned and it is bound to acc in the global env 

((acc 'deposit) 40)
;90
; 1. frame B is created with m = 'deposit, subordinate to frame A, and 
; then the body of dispatch is eval'ed
; 2. procedure deposit is returned and frame B is destroyed, then it is applied to 40
; 3. frame C is created with amount = 40, subordinate to frame A
; 4. the body of deposit is eval'ed, and balance in frame A is changed

((acc 'withdraw) 60)
;30
; similar to the above



(define acc2 (make-account 100))
;http://community.schemewiki.org/?sicp-ex-3.11
