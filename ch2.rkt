#lang racket

(require test-engine/racket-tests)

(define (fib n)
  (define (iter a b i)
    (if (= i 0)
        a
        (iter b (+ a b) (sub1 i))))
  (iter 0 1 n))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (square x) (* x x))

(define (neg? x) (< x 0))
(define (same-sign? x y)
  (or (not (or (neg? x) (neg? y)))
      (and (neg? x) (neg? y))))

(define (average x y) (/ (+ x y) 2))

(define (divides? a b)
  (= 0 (remainder b a)))

(define (smallest-divisor-fast n)
  (define (find-divisor test)
    (cond
      ((> (square test) n) n)
      ((divides? test n) test)
      (else (find-divisor (+ n 2)))))
  (if (even? n)
      2
      (find-divisor 3)))

(define (prime? n)
  (= (smallest-divisor-fast n) n))

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
      (cons (car list1)
            (append (cdr list1) list2))))

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

(define us-coins '(50 25 10 5 1))

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
  (define (same-parity? x y)
    (= (remainder x 2) (remainder y 2)))
  (define (iter l)
    (cond
      ((null? l) '())
      ((same-parity? x (car l)) (cons (car l)
                                      (iter (cdr l))))
      (else (iter (cdr l)))))
  (cons x (iter y)))

; 2.21 - The map() primitive

(define (square-list-1 items)
  (if (null? items)
      `()
      (cons (square (car items))
            (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

; 2.22 - for-each

(define (for-each f items)
  (cond
    ((not (null? items))
     (f (car items))
     (for-each f (cdr items)))))

; 2.27 - deep-reverse

(define (deep-reverse items)
  (define (iter x result)
    (cond
      ((null? x) result)
      ((not (pair? x)) x)
      (else
       (iter (cdr x) (cons (deep-reverse (car x))
                           result)))))
  (iter items `()))

(define (deep-reverse-v2 items)
  (cond
    ((null? items) '())
    ((not (pair? items)) items)
    (else (reverse (cons (deep-reverse (car items))
                         (deep-reverse (cdr items)))))))

; 2.28 - fringe() (enumerating a tree's leaves)

(define (fringe x)
  (cond
    ((null? x) `())
    ((not (pair? x)) (list x))
    (else (append (fringe (car x))
                  (fringe (cdr x))))))

; 2.29 - Implementing a "binary mobile"

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch-mobile m) (car m))
(define (right-branch-mobile m) (cadr m))
(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))

(define (total-weight mobile)
  (cond
    ((null? mobile) 0)
    ((not (pair? mobile)) mobile)
    (else
     (+ (total-weight (branch-structure (left-branch mobile)))
        (total-weight (branch-structure (right-branch mobile)))))))

(define (balanced-mobile? m)
  (define (branch-torque b)
    (* (branch-length b) (total-weight (branch-structure b))))
  (if (or (null? m) (not (pair? m)))
      #t
      (let ((l (left-branch-mobile m))
            (r (right-branch-mobile m)))
        (if (not (= (branch-torque l) (branch-torque r)))
            #f
            (and (balanced-mobile? (branch-structure l))
                 (balanced-mobile? (branch-structure r)))))))

; 2.30 - square-tree()

; a direct solution
(define (square-tree tree)
  (cond
    ((null? tree) `())
    ((not (pair? tree)) (square tree))
    (else (cons (square-tree (car tree))
                (square-tree (cdr tree))))))

; a solution using map
(define (square-tree-map tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree-map subtree)
             (square subtree)))
       tree))

(define t (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

; 2.31 - A general tree-map

(define (tree-map unary-op tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map unary-op subtree)
             (unary-op subtree)))
       tree))

; 2.32 - Generating power sets

(define (subsets s)
  (if (null? s)
      (list `())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset)
                            (cons (car s) subset))
                          rest)))))

; Demo - Generalized sequence operations

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; 2.33 - Basic list ops as accumulations

(define (map-acc p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) `() sequence))

(check-expect (map-acc sub1 '(1 2 3 4 5)) '(0 1 2 3 4))

(define (append-acc seq1 seq2)
  (accumulate cons seq2 seq1))

(check-expect (append-acc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

(define (length-acc sequence)
  (accumulate (lambda (_ y) (+ 1 y)) 0 sequence))

(check-expect (length-acc '(1 2 3 4)) 4)
(check-expect (length-acc '()) 0)


; 2.34 - Horner's Rule

(define (horner-eval x coefficients)
  (accumulate (lambda (coeff higher-terms)
                (+ coeff (* x higher-terms)))
              0
              coefficients))

(check-expect (horner-eval 2 '(1 3 0 5 0 1)) 79)

; 2.35 - count-leaves via accumulate

; (define (count-leaves t)
;   (cond
;     ((null? t) 0)
;     ((not (pair? t)) 1)
;     (else (+ (count-leaves (car t))
;              (count-leaves (cdr t)))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) 1) (fringe t))))

; 2.36 - accumulate-n (working with multiple sequences

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      `()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; 2.37 - basic matrix operations

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (accumulate + 0 (map * row v)))
       m))

; ((1 2 3)     ((1 4 7)
;  (4 5 6)  ->  (2 5 8)
;  (7 8 9))     (3 6 9))
(define (transpose mat)
  (accumulate-n cons `() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))

; 2.38 - right-fold vs. left-fold

(define right-fold accumulate)

(define (left-fold op init sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
    (iter init sequence))

; 2.39 - redefining reverse()

(define (reverse-right sequence)
  (right-fold (lambda (x acc)
                (append acc (list x)))
              `()
              sequence))

(define (reverse-left sequence)
  (left-fold (lambda (acc x)
               (cons x acc))
             `()
             sequence))

; 2.40 - Unique Pairs (a.k.a. using nested mappings)

(define (flatmap proc seq)
  (accumulate append `() (map proc seq)))

(define (enumerate a b)
  (if (> a b)
      `()
      (cons a
            (enumerate (inc a) b))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate (inc i) n)))
   (enumerate 1 n)))

(check-expect (unique-pairs 0) '())
(check-expect (unique-pairs 1) '())
(check-expect (unique-pairs 4) '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))

(define (filter pred? seq)
  (cond
    ((null? seq) `())
    ((pred? (car seq)) (cons (car seq)
                             (filter pred? (cdr seq))))
    (else (filter pred? (cdr seq)))))

(define (prime-sum-pairs n)
  (define (pair-sum p) (accumulate + 0 p))
  (define (prime-sum? p) (prime? (pair-sum p)))
  (define (make-pair-sum p)
    (list (car p) (cadr p) (pair-sum p)))
  (map
   make-pair-sum
   (filter
    prime-sum?
    (unique-pairs 1 n))))

; 2.41 - Ordered triples having sum

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate (add1 j) n)))
                      (enumerate (add1 i) n)))
           (enumerate 1 n)))

(define (triples-sum n s)
  (filter
   (lambda (t) (= (accumulate + 0 t) s))
   (unique-triples n)))

(check-expect (triples-sum 6 -1) '())
(check-expect (triples-sum 6 10) '((1 3 6) (1 4 5) (2 3 5)))

; even more fun, distinct k-tuples of positive integers not greater than n

; the unique k-tuples of integers between i and n (inclusive) can be enumerated
; by prepending j to each of the unique (k - 1)-tuples of integers between j + 1 and n,
; for i <= j <= n

; if k is greater than (n - i + 1), return the empty list

(define (unique-tuples n k)
  (define (unique-from i k)
    (cond
      ((> k (add1 (- n i))) '())
      ((= k (add1 (- n i))) (list (enumerate i n)))
      (else
        (flatmap (lambda (j)
                   (map (lambda (tuple)
                          (cons i tuple))
                        (unique-from (add1 j) (sub1 k))))
                 (enumerate i n)))))
  (flatmap (lambda (i)
             (unique-from i k))
           (enumerate 1 n)))

(check-expect (unique-tuples 6 3) (unique-triples 6))
(check-expect (unique-tuples 10 2) (unique-pairs 10))
(check-expect (unique-tuples 4 1) '((1) (2) (3) (4)))
(check-expect (unique-tuples 6 0) '())
(check-expect (unique-tuples -1 4) '())

; Extended Example - a picture language

; 2.46 - Representing vectors

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w))
             (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w))
             (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; 2.47 - Representing frames

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))

; 2.48 - Representing line segments
;
; (see exercise 2.2)

; 2.54 - Testing for equality (or so they would have us believe...)

(define (my-equal? a b)
  (cond
    ((and (symbol? a) (symbol? b)) (eq? a b))
    ((and (list? a) (list? b))
     (cond
       ((and (null? a) (null? b)) true)
       ((or (null? a) (null? b)) false)
       ((not (equal? (car a) (car b))) false)
       (else (equal? (cdr a) (cdr b)))))
    (else false)))

(check-expect (my-equal? '(this is a list) '(this is a list)) #t)
(check-expect (my-equal? '(this is a list) '(this (is a) list)) #f)

; Demo - Symbolic Differentiation

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum (make-product (multiplier expr)
                                 (deriv (multiplicand expr) var))
                   (make-product (deriv (multiplier expr) var)
                                 (multiplicand expr))))
        ((exponentiation? expr)
         (make-product
          (make-product (exponent expr)
                        (make-exponentiation (base expr)
                                             (dec (exponent expr))))
          (deriv (base expr) var)))
        (else (error "Unrecognized expression type -- DERIV " expr))))

(define (variable? x) (symbol? x))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (is-zero? x) (=number? x 0))
(define (is-one? x) (=number? x 1))

(define (none? pred? lst)
  (= (memf pred? lst) #f))

(define (any? pred? lst)
  (not (= (memf pred? lst) #f)))

(define (all? pred? lst)
  (= (memf (negate pred?) lst) #f))

; sum constructors / selectors
(define (make-sum x y)
  (cond ((and (number? x) (number? y)) (+ x y))
        ((and (is-zero? x) y))
        ((and (is-zero? y) x))
        (else (list '+ x y))))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend x) (cadr x))
(define (augend x)
  (let ((rest (cddr x)))
    (if (null? (cdr rest))
        (car rest)
        (cons '+ rest))))

; product constructors / selectors
(define (make-product x y)
  (cond ((and (number? x) (number? y)) (* x y))
        ((or (is-zero? x) (is-zero? y)) 0)
        ((is-one? x) y)
        ((is-one? y) x)
        (else (list '* x y))))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier x) (cadr x))
(define (multiplicand x)
  (let ((rest (cddr x)))
    (if (null? (cdr rest))
        (car rest)
        (cons '* rest))))

; exponent constructors / selectors
(define (make-exponentiation x y)
  (cond
    ((=number? y 0) 1)
    ((=number? y 1) x)
    (else (list '** x y))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

; 2.57 - Arbitrary number of terms in sums and products

(define expr '(* x y (+ x 3)))

(check-expect (deriv '(* x y (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))
(check-expect (deriv '(+ x x x x) 'x) 4)
(check-expect (deriv '(* 1 1 1 1) 'x) 0)
(check-expect (deriv '(* 4 x) 'x) 4)

(deriv '(* x y (+ x 3)) 'x)

; Demo - Representing Sets

; ...as unordered lists (no duplicates)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((null? set1) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

; 2.60 - ...as unordered lists (with duplicates)
;
; element-of-set? could become quicker in the average case
; adjoin-set becomes a constant time operation, and
; union-set a linear time one (just append the sets to one
; another). intersection-set becomes quicker on average due
; to the performance improvement in element-of-set?

; 2.61 - ...as ordered lists

(define (element-of-set-ord? x set)
  (cond ((null? set) false)
        ((< x (car set))
         (element-of-set-ord? x (cdr set)))
        (else (= x (car set)))))

(define (adjoin-set-ord x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set))
         set)
        (else (cons (car set) (adjoin-set-ord x (cdr set))))))

(define (intersection-set-ord set1 set2)
  (if (or (null? set1) (null? set2))
      '()
       (let ((x (car set1)) (y (car set2)))
         (cond ((< x y)
                (intersection-set-ord (cdr set1)
                                      set2))
               ((= x y)
                (cons x
                      (intersection-set-ord (cdr set1)
                                            (cdr set2))))
               (else (intersection-set-ord set1
                                           (cdr set2)))))))

; 2.62
(define (union-set-ord set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x (car set1)) (y (car set2)))
                (cond ((< x y)
                       (cons x (union-set-ord (cdr set1) set2)))
                      ((= x y)
                       (cons x (union-set-ord (cdr set1) (cdr set2))))
                      (else
                       (cons y (union-set-ord set1 (cdr set2)))))))))

; ...as binary trees

(define (datum node) (car node))
(define (left node) (cadr node))
(define (right node) (caddr node))

(define (element-of-set-tree? x set)
  (if (null? set)
      false
      (let ((d (datum set)))
        (cond ((< x d) (element-of-set-tree? x (left set)))
              ((> x d) (element-of-set-tree? x (right set)))
              (else true)))))

(define (adjoin-set-tree x set)
  (if (null? set)
      (list x '() '())
      (let ((d (datum set)))
        (cond ((< x d) (list d
                             (adjoin-set-tree x (left set))
                             (right set)))
              ((> x d) (list d
                             (left set)
                             (adjoin-set-tree x (right set))))
              (else set)))))

; Demo / 2.63 - converting trees to lists

(define (tree->list1 tree)
  (if (null? tree)
      '()
      (append (tree->list1 (left tree))
              (cons (datum tree)
                    (tree->list1 (right tree))))))

(define (tree->list2 tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left tree)
                      (cons (datum tree)
                            (copy-to-list (right tree)
                                          result)))))
  (copy-to-list tree '()))

; some sample trees
(define tree1
  (list 7
        (list 3
              (list 1 '() '())
              (list 5 '() '()))
        (list 9
              '()
              (list 11 '() '()))))

(define tree2
  (list 3
        (list 1 '() '())
        (list 7
              (list 5 '() '())
              (list 9
                    '()
                    (list 11 '() '())))))

(define tree3
  (list 5
        (list 3
              (list 1 '() '())
              '())
        (list 9
              (list 7 '() '())
              (list 11 '() '()))))

; Demo / 2.64 - Converting ordered lists to trees

(define (list->tree seq)
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (dec n) 2)))
          (let ((left-result (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n (+ left-size 1))))
              (let ((this-elt (car non-left-elts))
                    (right-result (partial-tree (cdr non-left-elts)
                                                right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (list this-elt left-tree right-tree)
                        remaining-elts))))))))
  (partial-tree seq (length seq)))

; 2.65 - intersection and union ("of trees")

(define (intersection-set-tree set1 set2)
  (list->tree (intersection-set-ord (tree->list2 set1)
                                    (tree->list2 set2))))

(define (union-set-tree set1 set2)
  (list->tree (union-set-ord (tree->list2 set1)
                             (tree->list2 set2))))

; 2.66 - lookup-tree == element-of-set-tree, no?

; === Huffman Coding Trees ===

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? x) (eq? (car x) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (decode bits tree)
  (define (choose-branch bit tree)
    (cond ((= bit 0) (left-branch tree))
          ((= bit 1) (right-branch tree))
          (else (error "bad bit -- CHOOSE BRANCH" bit))))
  (define (decode-1 bits curr)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) curr)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-tree-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-tree-set x (cdr set))))))

; Demo / 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-encoding '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; 2.68 - Encoding a message to bits

(define (encode message tree)
  (define (encode-1 sym branch) ; what to do if tree is empty??
    (cond ((leaf? branch) '())
          ((element-of-set? sym
                            (symbols (left-branch branch)))
           (cons 0 (encode-1 sym (left-branch branch))))
          ((element-of-set? sym
                            (symbols (right-branch branch)))
           (cons 1 (encode-1 sym (right-branch branch))))
          (else (error "bad symbol -- ENCODE-1" sym))))
  (if (null? message)
      '()
      (append (encode-1 (car message) tree)
              (encode (cdr message) tree))))

; 2.69 - Generating a tree

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge trees)
  (cond
    ((null? trees) '())
    ((null? (cdr trees)) (car trees))
    (else (successive-merge
           (adjoin-tree-set (make-code-tree (car trees) (cadr trees))
                            (cddr trees))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-tree-set (make-leaf (car pair)
                                    (cadr pair))
                         (make-leaf-set (cdr pairs))))))

(define sample-pairs
  (list '(A 2)
        '(BOOM 1)
        '(GET 2)
        '(JOB 2)
        '(NA 16)
        '(SHA 3)
        '(YIP 9)
        '(WAH 1)))

(define sample-pairs-2
  (list '(A 8)
        '(B 3)
        '(C 1)
        '(D 1)
        '(E 1)
        '(F 1)
        '(G 1)
        '(H 1)))

(define sample-pairs-3
  (list '(A 4)
        '(B 2)
        '(D 1)
        '(C 1)))

(define sample-pairs-4
  (list '(FOO 2)
        '(BAR 29)
        '(BAZ 4)
        '(QUX 1)))

(define sample-message
  '(GET A JOB
    SHA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(check-expect (length (encode sample-message
                              (generate-huffman-tree sample-pairs)))
              82)
(check-expect (decode (encode sample-message
                              (generate-huffman-tree sample-pairs))
                      (generate-huffman-tree sample-pairs))
              sample-message)

(test)
