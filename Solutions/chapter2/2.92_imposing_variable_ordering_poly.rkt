; By imposing an ordering on variables, extend the polynomial package 
; so that addition and multiplication of polynomials works for polynomials
; in different variables. (This is not easy!)

; thoughts on this problem
; a tower of variables
; combining "different" polynomials by repeatedly raising the polynomial that has the lower principal variable
; the crux of this problem is to implement the raise procedure for polynomial

; variable-ordering: a list specifying which specifies the priorities of variables
; highest-priority-variable: find out the variable with the highest priority by traversing the recursive structure of polynomials

; rearranging-terms: rearrage terms based on a specified variable
    ; we might define a procedure that transform a term to a polynomial with the higher variable
        ; if the term (subterm) itself is not a poly or it does not contains the higher variable, treat it as a const 
        ; product of outter variables become the coeff of the term of the new polynomials
    ; and then we apply this procedure to all terms of a polynomial and sum up the resulting polynomials