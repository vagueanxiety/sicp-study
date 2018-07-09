#lang sicp
(define (expmod base exp m) (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
(else
(remainder (* base
                       (expmod base (- exp 1) m))
                    m))))

; Eva's claim: you have transformed the Θ(logn) process into a Θ(n) process. Explain.

; This snippet will generate a recursion tree instead of a linear recursion (like a list with length logn) 
; runtime of a recursion tree is proportioanl to the number of nodes in the tree

; the worse case (Big O) would be that the recursion tree is a **perfect** binary tree (given a fixed height)
; number of nodes in a perfect binary tree with height h is 2^(h+1) -1 (obtained from the sum of geometric series)

; in this case the height of the tree is (logn), because the height does not change, and it is the branching that makes it a tree.
; Therefore the number of nodes 2 ^ (logn) = O(n)

; :( i feel like the point for doing this type of question is to get a sense of how
; complexity changes, not to write down a rigorous math proof. So i will just leave as it is, although BigO and Theta are different.


; The only takeaway here is the relationship (log/expt) between the number of nodes and the height of a perfect binary tree