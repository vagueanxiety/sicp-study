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