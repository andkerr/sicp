#lang racket

(define (square x) (* x x))
(define (hypot x y)
  (sqrt (+ (square x) (square y))))

(define (make-segment start end)
  (cons start end))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (components-segment s)
  (make-point (- (x-point (end-segment s))
                 (x-point (start-segment s)))
              (- (y-point (end-segment s))
                 (y-point (start-segment s)))))
(define (magnitude-segment s)
  (let ((comps (components-segment s)))
    (hypot (car comps) (cdr comps))))

(define (dot-segment s1 s2)
  (dot (components-segment s1)
       (components-segment s2)))

(define (dot v1 v2)
  (let ((x1 (car v1))
        (x2 (car v2))
        (y1 (cdr v1))
        (y2 (cdr v2)))
    (sqrt (+ (* x1 x2) (* y1 y2)))))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (equal-point p1 p2)
  (and (= (x-point p1) (x-point p2))
       (= (y-point p1) (y-point p2))))
 
(define (make-rectangle seg1 seg2)
  (if (not (= (dot-segment seg1 seg2) 0))
      (error "MAKE-RECTANGLE -- non-perpendicular line segments"
             seg1
             seg2)
      (let ((p1 (start-segment seg1))
            (p2 (end-segment seg1))
            (p3 (start-segment seg2))
            (p4 (end-segment seg2)))
        (cond ((equal-point p1 p3) #f)
              ((equal-point p1 p4) #f)
              ((equal-point p2 p3) #f)
              ((equal-point p2 p4) #f)
              (else (error ("MAKE-RECTANGLE -- non-intersecting line segments"
                            seg1
                            seg2)))))))

; Representation 2 - scale, translate, rotate unit square

(define (make-rectangle-v2 w h rads orig) #f)

(define a (make-segment (make-point 2 2)
                        (make-point 5 -2)))
(define b (make-segment (make-point 2 2)
                        (make-point 4 4)))
(define c (make-segment (make-point 2 2)
                        (make-point 4 0)))