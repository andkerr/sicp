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

; 2.4 - An alternate implementation of pairs
;
; (define (cons x y)
;   (lambda (m) (m x y)))
;
; (define (car z)
;   (z (lambda (p q) p)))
;
; (define (cdr z)
;   (z (lambda (p q) q)))
;
; Here, cons constructs a pair in the form of a
; procedure that accepts a function (m) and applies
; it to its two elements. m is a "retriever" function
; which, by receiving the pair's elements as arguments,
; it able to return either, thus simulating the behaviour
; of car and cdr.

; 2.5 - "Arithmetic" integer pairs

(define (cons-int x y)
  (* (expt 2 x) (expt 3 y)))

(define (car-int z)
  (if (= (remainder z 2) 0)
      (+ 1 (car-int (/ z 2)))
      0))

(define (cdr-int z)
  (if (= (remainder z 3) 0)
      (+ 1 (cdr-int (/ z 3)))
      0))

; 2.6 - Church numerals (hmmm)

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; Demo + exercises - Interval arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (empty? y)
      (error "Cannot divide by interval spanning zero -- " y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; 2.7 - completing the implementation

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; 2.8 - interval subtraction

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

; 2.10 - checking for division by zero

(define (width-interval a)
  (/ (- (upper-bound a)
        (lower-bound a))
     2))

(define (empty? a) (= (width-interval a) 0))

; 2.11 - optimizing mul-interval
;
; Suppose we have two intervals [a, b] and [c, d].
; There are 16 possible arrangments of polarity (-/+)
; between the 4 endpoints. Some of these are invalid
; by the requirement a <= b for any interval [a, b]
; (i.e. no interval can have a positive lower bound and
; a negative upper bound). After eliminating these,
; 9 arrangements remain:
;
; [-,-] [-,-] *
; [-,-] [-,+] *
; [-,-] [+,+] *
; [-,+] [-,-]
; [-,+] [-,+] *
; [-,+] [+,+] *
; [+,+] [-,-]
; [+,+] [-,+]
; [+,+] [+,+] *
;
; (* - sorted interval pairs)
;
; We can actually narrow these cases further by first
; sorting the intervals. Then, the 6 possibilities are:
;
; [-,-] [-,-]
; [-,-] [-,+]
; [-,-] [+,+]
; [-,+] [-,+]
; [-,+] [+,+]
; [+,+] [+,+]
;
; In all but the case [-,+] [-,+], we can find
; the products among endpoints directly by comparing
; a, b, c, and d, such that we only need to do
; two multiplications:
;
; [-,-] [-,-] -> [bd, ac]
; [-,-] [-,+] -> [ad, ac]
; [-,-] [+,+] -> [ad, bc]
; [-,+] [-,-] -> [bc, ac]
; [-,+] [-,+] -> <comnpare all 4 products here>
; [-,+] [+,+] -> [ad, bd]
; [+,+] [-,-] -> [bc, ad]
; [+,+] [-,+] -> [bc, bd]
; [+,+] [+,+] -> [ac, bd]

(define (less-interval x y)
  (if (= (lower-bound x) (lower-bound y))
      (< (upper-bound x) (upper-bound y))
      (< (lower-bound x) (lower-bound y))))

(define (min-interval x y)
  (if (less-interval y x) y x))

(define (max-interval x y)
  (if (less-interval y x) x y))

(define (mul-interval-fast x y)
  (let ((x (min-interval x y))
        (y (max-interval x y)))
    (let ((a (lower-bound x))
          (b (upper-bound x))
          (c (lower-bound y))
          (d (upper-bound y)))
      (cond
        ((and (neg? a) (neg? b) (neg? c) (neg? d))
         (make-interval (* b d) (* a c)))
        ((and (neg? a) (neg? b) (neg? c) (not (neg? d)))
         (make-interval (* a d) (* a c)))
        ((and (neg? a) (neg? b) (not (neg? c)) (not (neg? d)))
         (make-interval (* a d) (* b c)))
        ((and (neg? a) (not (neg? b)) (neg? c) (not (neg? d)))
         (let ((p1 (* a c))
               (p2 (* a d))
               (p3 (* b c))
               (p4 (* b d)))
           (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
        ((and (neg? a) (not (neg? b)) (not (neg? c)) (not (neg? d)))
         (make-interval (* a d) (* b d)))
        ((and (not (neg? a)) (not (neg? b)) (not (neg? c)) (not (neg? d)))
         (make-interval (* a c) (* b d)))))))
        

; 2.12 - intervals as midpoint + tolerance

(define (make-center-percent c p)
  (let ((width (abs (* c p))))
    (make-interval (- c width) (+ c width))))

(define (center a)
  (/ (+ (upper-bound a)
        (lower-bound a))
     2))

(define (percent a)
  (/ (width a) (abs (center a))))

; 2.13 thru 2.16 - various mathematics exercises about intervals

; === Hierarchical Structures ===

; Lists

; 2.17 - Last Pair

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; Demo - Appending to a list

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; 2.18 - Reversing a list
;
; Can this definition be formulated recursively?

(define (reverse l)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l) (cons (car l) result))))
  (iter l (list)))

; 2.19 - Making count-change flexible

; recall the demonstration from Chapter 1
(define (count-change amount coins)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (no-more? coins)) 0)
    (else (+ (count-change amount
              (except-first coins))
             (count-change (- amount
                              (first-denomination coins))
                           coins)))))

(define no-more? null?)
(define first-denomination car)
(define except-first cdr)

; 2.20 - Variable numbers of arguments

(define (same-parity x . y)
  (cond
    ((null? y) nil)
    ((same-parity? x (car y)) (cons (car y)
                                    (same-parity x (cdr y)))) ; <-- this calls same-parity with 2 arguments, no good!
    (else (same-parity x (cdr y)))))

(define (same-parity? x y)
  (display x)
  (newline)
  (display y)
  (newline)
  (= (remainder x 2) (remainder y 2)))

(same-parity 1 2 3 4 5 6 7)