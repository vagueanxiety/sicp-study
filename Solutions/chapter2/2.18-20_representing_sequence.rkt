#lang sicp

; helpers
(define (length items)
    (define (length-iter a count)
    (if (null? a) count
            (length-iter (cdr a) (+ 1 count))))
    (length-iter items 0))

(define (list-ref items n) (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (append list1 list2) (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

; this solution is from Daniel-Amariei at http://community.schemewiki.org/?sicp-ex-2.17
; 1. last pair should returns a list, not just a particular element
 
(define (last-pair L) 
    (if (null? (cdr L)) 
        L 
        (last-pair (cdr L)))) 

(define (reverse items)
    (define (iter l t)
        (if (null? l) 
            t
            (iter (cdr l) (cons (car l) t)) 
        ))
    (iter items nil)                    
)    
    
 
;recursive solution from karthikk at http://community.schemewiki.org/?sicp-ex-2.18
(define (r-reverse lat) 
    (if (null? lat) 
        '() 
        (append (r-reverse (cdr lat)) (list (car lat))))) 

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (except-first-denomination coin-values)
    (cdr coin-values))
(define (first-denomination coin-values)
    (car coin-values))
(define (no-more? coin-values)
    (= 0 (length coin-values)))


(define (cc amount coin-values) 

    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0) 
          (else
            (+ (cc amount
                (except-first-denomination
                    coin-values))
            (cc (- amount
                    (first-denomination
                    coin-values))
                coin-values))))) 


; Does the order of the list coin-values affect the answer produced by cc? Why or why not?
; No
; From Rptx at http://community.schemewiki.org/?sicp-ex-2.19
; The order of the coins does not affect the result. Becuase the procedure computes all possible 
; combinations. But it does affect the speed of the computation. If you start with the lower
; valued coins, it'll take much longer.
(define (list-filter pass? items)
    (define (iter l)        
        (cond   ((null? l) nil)
                ((pass? (car l)) (cons (car l) (iter (cdr l))))
                (else  (iter (cdr l))))     
    )
    (iter items)     
)


(define (same-parity x . y)
    (let ((l (cons x y)))
        (if (even? x)
        (list-filter even? l)
        (list-filter odd? l))
    ))

; testing
(last-pair (list 23 72 149 34))
(list 23 72 149 34)
(reverse (list 23 72 149 34))
(r-reverse (list 23 72 149 34))
(reverse (list 1))
(cc 100 us-coins)
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 8 10 12) 
