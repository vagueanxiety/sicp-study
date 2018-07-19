#lang sicp

(define (gcd a b) (if (= b 0)
      a
      (gcd b (remainder a b))))


(gcd 206 40)

; normal order evaluation 


; (gcd 206 40)
; 40 != 0
; (gcd 40 (remainder 206 40))
; remainder = 6; one remainder operation

; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; remainder = 4; two remainder operations

; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; remainder = 2; four remainder operations

; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder ...))
; remainder = 0 ; seven remainder operations
; b = 0 => evalute a; four remainder operations
; 18 remainder operations in total



; applicative order evaluation
; (gcd 206 40)
; (gcd 40 (remainder 206 40)) => (gcd 40 6)
; (gcd 6 (remainder 40 6)) => (gcd 6 4)
; (gcd 4 (remainder 6 4)) => (gcd 4 2)
; (gcd 2 (remainder 4 2)) => (gcd 2 0)
; => complete!
; four remainder operations

; the general formula for counting remainder invocations 
; http://community.schemewiki.org/?sicp-ex-1.20