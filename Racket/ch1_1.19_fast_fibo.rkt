#lang sicp

; this solution is from http://community.schemewiki.org/?sicp-ex-1.19
; the relation between p' and q' can be easily obtained by plugging the 
; expressions of p and q into the definitions of p and q


; one analogy to this fast-fib would be walking. Think of changing q and p as changing pace.
; every time the pace is doubled (two transformations are combined into one), the
; number of steps needed to get to a destination is halved (the count is divided by two).

; note that "pace" can be doubled only when count is even


  
 (define (fib n) 
   (fib-iter 1 0 0 1 n)) 
 (define (fib-iter a b p q count) 
   (cond ((= count 0) b) 
         ((even? count) 
          (fib-iter a 
                    b 
                    (+ (square p) (square q)) 
                    (+ (* 2 p q) (square q)) 
                    (/ count 2))) 
         (else (fib-iter (+ (* b q) (* a q) (* a p)) 
                         (+ (* b p) (* a q)) 
                         p 
                         q 
                         (- count 1))))) 
  
 (define (square x) (* x x)) 