; space:
; the worse case scenario (when the recursion tree is longest) would be 
; using the coin with the smallest denomination all the way down 
; (i.e. max depth = amount / the smallest denomination). Therefore, space = O(n)


; time (number of nodes in the tree):
; see gollum0's comments in http://community.schemewiki.org/?sicp-ex-1.14


; tree diagram: http://sicp.readthedocs.io/en/latest/_images/14.png

; implementation

#lang sicp

 (define (count-change amount) 
   (cc amount 5)) 
 (define (cc amount kinds-of-coins) 
   (cond ((= amount 0) 1) 
         ((or (< amount 0) (= kinds-of-coins 0)) 0) 
         (else (+ (cc amount 
                      (- kinds-of-coins 1)) 
                  (cc (- amount 
                         (first-denomination kinds-of-coins)) 
                      kinds-of-coins)))))  
 (define (first-denomination kinds-of-coins) 
   (cond ((= kinds-of-coins 1) 50) 
         ((= kinds-of-coins 2) 25) 
         ((= kinds-of-coins 3) 10) 
         ((= kinds-of-coins 4) 5) 
         ((= kinds-of-coins 5) 1))) 