#lang sicp

(define (neg? x) (< x 0))
(define (same-sign? x y)
  (or (not (or (neg? x) (neg? y)))
      (and (neg? x) (neg? y))))

(define (average x y) (/ (+ x y) 2))

; Demo - Rational number arithmetic

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Exercise 2.1 - A better make-rat
;
; Implement make-rat so that it returns a
; numerator and denominator reduce to lowest
; terms, and normalized such that if the rational
; number is negative, the numerator contains the
; "negative" integer value.
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond
      ((same-sign? n d) (cons (/ (abs n) g) (/ (abs d) g)))
      ((neg? n) (cons (/ n g) (/ d g)))
      (else (cons (/ (- n) g) (/ (- d) g))))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

; Exercise 2.2 - Line Segments

(define (midpoint seg)
  (let ((a (start-segment seg))
        (b (end-segment seg)))
    (make-point (average (x-point a) (x-point b))
                (average (y-point a) (y-point b)))))

(define (make-segment start end)
  (cons start end))

(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

; 2.3 - Rectangles

; representation 1 (axis-aligned only...)
(define (area rect)
  (* (width rect) (height rect)))

(define (perimeter rect)
  (+ (* 2 (width rect))
     (* 2 (height rect))))

(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (width r)
  (abs-diff x-point (car r) (cdr r)))

(define (height r)
  (abs-diff y-point (car r) (cdr r)))

(define (abs-diff f x y)
  (abs (- (f x) (f y))))