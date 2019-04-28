#lang sicp


(define (internal-make-account balance password-list threshold count)
  (define (contains? list element)
    (cond ((null? list) false)
          ((eq? (car list) element) true)
          (else (contains? (cdr list) element)))
    )

  (define (add-new-password local-password-list new-password)
    (set! password-list (cons new-password local-password-list)))

  (define (access-legal? p)
    (contains? password-list p))

  (define (add-joint-access new-password)
    (add-new-password password-list new-password))

  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance
               (- balance amount))
             balance)
      "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (access-forbidden x)
    (begin (set! count (+ count 1))
           (if (> count threshold)
             (call-the-cops)
             (error "Incorrect Password" x))))

  (define (call-the-cops)
    "Cops are coming!!")

  (define (dispatch p m)
    (if (access-legal? p)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'joint) add-joint-access)
            (else (error "Unknown request:
                         MAKE-ACCOUNT" m)))
      access-forbidden))

                         dispatch)

(define (make-account balance password)
  (internal-make-account balance (cons password nil) 7 0))


;; joint account: balance changes reflect all joint accounts
;; adding additional accesses rather than new accounts
;; data structure to hold all possible passwords
(define (make-joint account old-password new-password)
  (begin ((account old-password 'joint) new-password)
         account)
  )

(define acc (make-account 100 'secret-password))
(define joint-acc (make-joint acc 'secret-password 'another-secret-password))

;; issue: mapping between username and password
((acc 'secret-password 'withdraw) 110)
((joint-acc 'another-secret-password 'withdraw) 110)

((acc 'secret-password 'withdraw) 10)
((joint-acc 'another-secret-password 'deposit) 50)
((joint-acc 'another-secret-password 'deposit) 50)
((acc 'secret-password 'withdraw) 10)
((joint-acc 'secret-password 'deposit) 50) ; issue?
((acc 'secret-password 'withdraw) 10)

((joint-acc 'incorrect-password 'deposit) 50)

;
;(define another-joint-acc (make-joint acc 'incorrect-password 'password))
;
;(define l (list 1 2 3 4 5))
;(contains? l 1)
;(contains? l 2)
;(contains? l 3)
;(contains? l 4)
;(contains? l 5)
;(contains? l 6)
;(contains? l 7)
