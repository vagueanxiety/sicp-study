#lang sicp


(define (flipped-pairs painter)
    (let ((painter2 (beside painter (flip-vert painter))))
        (below painter2 painter2)))

(define (right-split painter n) 
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
                (beside painter (below smaller smaller)))))

(define (corner-split painter n) 
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
            (let ((top-left (beside up up))
                (bottom-right (below right right)) 
                (corner (corner-split painter (- n 1))))
                
                (beside (below painter top-left) (below bottom-right corner))))))

(define (square-limit painter n)
    (let ((quarter (corner-split painter n)))
        (let ((half (beside (flip-horiz quarter) quarter))) 
            (below (flip-vert half) half))))


(define (square-of-four tl tr bl br) 
    (lambda (painter)
        (let ((top (beside (tl painter) (tr painter))) 
            (bottom (beside (bl painter) (br painter))))
                (below bottom top))))
        
(define (frame-coord-map frame) 
    (lambda (v)
        (add-vect
            (origin-frame frame)
            (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                      (scale-vect (ycor-vect v) (edge2-frame frame))))))


(define (make-vect xcor ycor)
    (cons xcor ycor))

(define (ycor-vect vect)
    (cdr vect))

(define (xcor-vect vect)
    (car vect))

(define (sub-vect v1 v2)
    (let ((new-xcor (- (xcor-vect v1) (xcor-vect v2)))
            (new-ycor (- (ycor-vect v1) (ycor-vect v2))))
        (make-vect new-xcor new-ycor)))
    
(define (add-vect v1 v2)
    (let ((new-xcor (+ (xcor-vect v1) (xcor-vect v2)))
            (new-ycor (+ (ycor-vect v1) (ycor-vect v2))))
        (make-vect new-xcor new-ycor)))

(define (scale-vect vect factor)
    (let ((new-xcor (* (xcor-vect vect) factor))
            (new-ycor (* (ycor-vect vect) factor)))
        (make-vect new-xcor new-ycor)))
            
(define (make-frame origin edge1 edge2) 
    (list origin edge1 edge2))
(define (edge1-frame frame)
    (cadr frame))
(define (edge2-frame frame)
    (caddr frame))
(define (origin-frame frame)
    (car frame))



(define (make-frame-alternative origin edge1 edge2) 
    (cons origin (cons edge1 edge2)))
(define (edge1-frame-alternative frame)
    (cadr frame))
(define (edge2-frame-alternative frame)
    (cddr frame))
(define (origin-frame-alternative frame)
    (car frame))


(define (segments->painter segment-list) 
    (lambda (frame)
        (for-each
            (lambda (segment)
                    (draw-line
                        ((frame-coord-map frame)
                        (start-segment segment))
                        ((frame-coord-map frame)
                        (end-segment segment))))
                    segment-list)))
; exercises
(define (up-split painter n) 
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
                (below painter (beside smaller smaller)))))

            
(define (split divide smaller-divide)
    (new-split (painter n)
        (if (= n 0)
            painter
            (let ((smaller (new-split painter (- n 1))))
                    (divide painter (smaller-divide smaller smaller))))
    )
)
    

(define right-split (split beside below)) 
(define up-split (split below beside))

(define (make-segment start end)
    (cons start end))
(define (start-segment seg)
    (car seg))
(define (end-segment seg)
    (cdr seg))

(define top-right (make-vect 1 1))
(define bottom-right (make-vect 1 0))
(define top-left (make-vect 0 1))
(define bottom-left (make-vect 0 0))

(define top-mid (make-vect 0.5 1))
(define bottom-mid (make-vect 0.5 0))
(define left-mid (make-vect 0 0.5))
(define right-mid (make-vect 1 0.5))

; passing the segment lists below to segments->painter will produce corresponding painters
(define outline-segment-list (list (make-segment bottom-left top-left) (make-segment bottom-left bottom-right) 
                                    (make-segment bottom-right top-right) (make-segment top-left top-right))
(define diamond-shape-segment-list (list (make-segment bottom-mid left-mid) (make-segment left-mid top-mid) 
                                    (make-segment top-mid right-mid) (make-segment right-mid bottom-mid))
(define x-shape-segment-list (list (make-segment bottom-left top-right) (make-segment bottom-right top-left))
; (define wave-segment-list ())
; Ask george for the list :P

; this solutions is from Sophia G at http://community.schemewiki.org/?sicp-ex-2.49
; (define wave 
;     (segments->painter (list 
;                         (make-segment (make-vect .25 0) (make-vect .35 .5)) 
;                         (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
;                         (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
;                         (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
;                         (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
;                         (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
;                         (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
;                         (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
;                         (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
;                         (make-segment (make-vect .35 .85) (make-vect .4 1)) 
;                         (make-segment (make-vect .4 1) (make-vect .6 1)) 
;                         (make-segment (make-vect .6 1) (make-vect .65 .85)) 
;                         (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
;                         (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
;                         (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
;                         (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
;                         (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
;                         (make-segment (make-vect .6 .45) (make-vect .75 0)) 
;                         (make-segment (make-vect .75 0) (make-vect .6 0)) 
;                         (make-segment (make-vect .6 0) (make-vect .5 .3)) 
;                         (make-segment (make-vect .5 .3) (make-vect .4 0)) 
;                         (make-segment (make-vect .4 0) (make-vect .25 0)) 
;                         ))) 
  ;George! 
