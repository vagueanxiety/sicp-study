#lang sicp

(define (internal-make-account balance password threshold count)
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
             "Incorrect password")))

  (define (call-the-cops)
    "Cops are coming!!")

  (define (dispatch p m)
    (if (eq? p password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request:
                         MAKE-ACCOUNT" m)))
      access-forbidden))

                         dispatch)

(define (make-account balance password)
  (internal-make-account balance password 7 0))

(define acc
  (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 110)
((acc 'secret-password 'withdraw) 40)
((acc 'secret-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)

