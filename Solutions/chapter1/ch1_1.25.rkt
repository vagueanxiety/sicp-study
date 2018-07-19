#lang sicp

(define (expmod base exp m)
    (remainder (fast-expt base exp) m)
)

; Would this procedure serve as well for our fast prime tester? Explain.

; probably not, because arithmetic operations (computing remainder in this case) 
; on sufficiently large numbers are not constant in time. Instead, the runtime
; grows with the size of number


; Why does this snippet need to deal with large numbers, while the previous one does not?

; from http://community.schemewiki.org/?sicp-ex-1.25:
; The remainder operation inside the original expmod implementation, keeps 
; the numbers being squared less than the number tested for primality m. 
; fast-expt however squares huge numbers of a^m size.

; One (bad) analogy would be the difference between snapping 100 chopsticks one at a time
; or snapping all of them together.

