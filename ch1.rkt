#lang sicp

; Helpers
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (average x y) (/ (+ x y) 2))
(define (reciprocal x) (/ 1 x))

; Demo - Fibonacci Numbers
; Fib(n) = 0 , if n = 0
;        = 1 , if n = 1
;        = Fib(n - 1) + Fib(n - 1), otherwise
(define (fib-rec n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    ((+ (fib-rec (- n 1))
        (fib-rec (- n 2))))))

(define (fib-iter n)
  (define (iter x y n)
    (if (= n 0)
        x
        (iter y (+ x y) (dec n))))
  (iter 0 1 n))

; Exercise 1.11
; f(n) = n                               , for n < 3
;      = f(n - 1) + 2f(n - 2) + 3f(n - 3), otherwise

(define (f-rec n)
  (if (< n 3)
      n
      (+
       (* 1 (f-rec (- n 1)))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (iter x y z count)
    (if (= count 0)
        x
        (iter y
              z
              (+ (* 3 x) (* 2 y) (* 1 z))
              (dec count))))
  (iter 0 1 2 n))

; Exercise 1.12 - Pascal's Triangle

(define (binom-coeff x y)
  (if (or (= y 0) (= x y))
      1
      (+ (binom-coeff (dec x) y)
         (binom-coeff (dec x) (dec y)))))

; Demo - Fast Exponentiation (Recursive)
;
; b^n = (b^n/2)^2,   if n is even
;     = b * b^(n-1), if n is odd

(define (fast-expt b n)
  (cond
    ((= n 0) 1)
    ((even? n) (square (fast-expt b (halve n))))
    (else (* b (fast-expt b (dec n))))))

; 1.16 - Fast Exponentiation (Iterative)
;
; Hint: [b^(n/2)]^2 = [b^2]^(n/2) = b^n

(define (fast-expt-iter b n)
  (define (iter a b n)
    (cond
      ((= n 0) a)
      ((even? n) (iter a (square b) (halve n)))
      (else (iter (* a b) b (dec n)))))
  (iter 1 b n))

; 1.17 - "Fast" Multiplication (Recursive)

(define (mul-rec x y)
  (if (= y 0)
      0
      (+ x (mul-rec x (dec y)))))

; 1.18 - "Fast" Multiplication (Iterative)
;        a.k.a. Russian Peasant Multiplication
; Hint: [x(y/2)]*2 = [2x](y/2) = xy

(define (mul-fast x y)
  (define (iter a x y)
    (cond
      ((= y 0) a)
      ((even? y) (iter a (double x) (halve y)))
      (else (iter (+ a x) x (dec y)))))
  (iter 0 x y))

; How are 1.16 ("Fast" Exponentiation) and
; 1.17 ("Fast" Multiplication) related? Both
; execute a given op (^ and *, resp.) in terms of
; a "simpler" (? less powerful? what's a good word)
; op (* and +, resp.) in a logarithmic number
; of steps, thanks to some clever tricks
; (square and double, resp.), which each allow an
; "atomic" way to apply the primitive op twice.
(define (fast-expt-gen x y op ident) ; a = identity element, apply a = (a op x) y times, return a
  (define (iter a x y)
    (cond
      ((= y 0) a)
      ((even? y) (iter a (op x x) (halve y)))
      (else (iter (op a x) x (dec y)))))
  (iter ident x y))

; 1.21 - Smallest Divisor (> 1)

(define (smallest-divisor n)
  (define (find-divisor test)
    (cond
      ((> (square test) n) n)
      ((divides? test n) test)
      (else (find-divisor (inc test)))))
  (find-divisor 2))

; (smallest-divisor   199) ; =  199
; (smallest-divisor  1999) ; = 1999
; (smallest-divisor 19999) ; =    7

; 1.22 - Prime Timing

(define (prime? n)
  (= (smallest-divisor-fast n) n))


(define (timed-prime-test n)
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  
  (define (do-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
  
  (newline)
  (display n)
  (do-test n (runtime)))


(define (search-for-primes a b)
  (define (iter a)
    (cond
      ((< a b)
        (timed-prime-test a)
        (iter (+ a 2)))))
  
  (iter (if (even? a) (inc a) a)))

; using naive smallest divisor...
;
; primes >    1000 - 1009, 1013, 1019, ...          (av. time =  ~8 units)
;        >   10000 - 10007, 10009, 10037, ...       (av. time = ~24 units)
;        >  100000 - 100003, 100019, 100043, ...    (av. time = ~25 units)
;        > 1000000 - 1000003, 1000033, 1000037, ... (av. time = ~75 units)

; 1.23 - A faster smallest-divisor algorithm

(define (next x)
  (if (= x 2)
      (inc x)
      (+ x 2)))


(define (smallest-divisor-fast n)
  (define (find-divisor test)
    (cond
      ((> (square test) n) n)
      ((divides? test n) test)
      (else (find-divisor (+ n 2)))))
  (if (even? n)
      2
      (find-divisor 3)))

; Demo - Generalized Sum
;
; with this formula to sum terms from a to b, we can...
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; sum integers...
(define (sum-ints a b)
  (sum identity a inc b))

; sum cubes...
(define (sum-cubes a b)
  (sum cube a inc b))

; compute integrals (naively)...
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum (+ a (/ dx 2.0)) add-dx b)
     dx))

; ...and so on.

; 1.29 - Simpson's Rule

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (cond
      ((or (= k 0) (= k n)) (y k))
      ((even? k) (* 2 (y k)))
      (else (* 4 (y k)))))
  
  (* (sum term 0 inc n)
     (/ h 3.0)))
     
; 1.30 - Making sum() iterative

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; 1.31 - Generalized product() procedure

; recursive
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

; iterative
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product-iter identity 1 inc n))

; approximating pi using
;
; (pi/4) = (2 * 4 * 4 * 6 * 6 * 8 * ...) / (3 * 3 * 5 * 5 * 7 * 7 * ...)

(define (plus-two x) (+ x 2))

(define (pi-approx n)
  (* (/ (product square 4 plus-two (+ 4 (* 2 n)))
        (product square 3 plus-two (+ 3 (* 2 n))))
     8.0))

(define (pi-approx-v2 n)
  (define (term n)
    (/ (+ 2.0 (* (floor (/ (+ n 1) 2)) 2))
       (+ 3.0 (* (floor (/ n 2)) 2))))
  (* (product term 0 inc (- n 1))
     4))

; 1.32 - A general accumulate() procedure

; recursive
(define (accumulate op init term a next b)
  (if (> a b)
      init
      (op (term a)
          (accumulate op init term (next a) next b))))

; iterative
(define (accum-iter op init term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (op (term a) result))))
  (iter a init))

; using this definition, sum() and product()
; can be written like so...
(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

; 1.33  - accumulate() with a filter

; recursive
(define (filtered-accumulate op init use? term a next b)
  (if (> a b)
      init
      (op (if (use? a) (term a) init)
          (filtered-accumulate op init use? term (next a) next b))))

; iterative
(define (filtered-accumulate-iter op init use? term a next b)
  (define (iter a result)
    (cond
      ((> a b) result)
      ((use? a) (iter (next a) (op (term a) result)))
      (else (iter (next a) result))))
  (iter a init))
        

; summing sqaures of primes
(define (sum-square-prime a b)
  (filtered-accumulate + 0 prime? square a inc b))

; product of positive integers less than n
; that are relatively prime to n
(define (rel-prime? a b)
  (= (gcd a b) 1))

(define (product-relatively-prime n)
  (define (rel-prime-n? x) (rel-prime? x n))
  (filtered-accumulate * 1 rel-prime-n? identity 0 inc (dec n)))

; 1.34

; suppose we have a function f that accepts a unary procedure
; and applies it the value 2...
(define (f g)
  (g 2))

; then, the expression (f f) expands to
; (f 2)
; (2 2)
; which results in a type mistach of sorts.

; Demo- Finding fixed points of a function
(define tolerance 0.000001)
(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    ;(display guess)
    ;(newline)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
    (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; 1.35 - Golden Ratio
;
; The Golden Ratio (1.618033...) is a fixed point
; of the transformation x -> 1 + 1/x, very cool.
; (define golden-ratio
;  (fixed-point (lambda (x) (average x (+ 1 (/ 1 x))))
;                1.0))

; 1.36 - Solving x^x = 1000

; without average damping
; (fixed-point (lambda (x) (/ (log 1000) (log x)))
;              4)

; with average damping
; (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
 ;             4)

; 1.37 - A continued fraction procedure

; recursive
(define (cont-frac n d k)
  (define (compute i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (compute (inc i))))))
  (compute 1))

; iterative
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (dec i) (/ (n i) (+ (d i) result)))))
  (iter k 0))

; n = 14 is accurate to 4 decimal places, very neat
(define (golden-ratio-cf k)
  (reciprocal (cont-frac (lambda (i) 1.0)
                         (lambda (i) 1.0)
                         k)))

; 1.38 - e as a continued fraction

(define (e-cf n)
  (+ (cont-frac (lambda (_) 1.0)
                (lambda (i)
                  (if (= (remainder i 3) 2)
                      (* 2 (+ (floor (/ i 3)) 1))
                      1))
                n)
     2))

(define PI 3.1415926535897932384626433)
; 1.39 - tan(x) as a continued fraction
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* 2 i) 1))
             k))

; Demo - Returning a procedure as a value
(define (average-damp f)
  (lambda (x) (average x (f x))))

; Demo - Newton's method
;
; If g(x) is a differentiable function, a solution
; g(x) = 0 will be a fixed point of the function
; f(x) = x - g(x) / Dg(x), where Dg(x) is the derivative
; of g evaluated at x.

(define dx 0.00001)

; given a procedure that returns the derivative
; of a function...
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

; and a procedure that compute the function whose
; fixed point we need to find in order to compute
; roots of g via Newton's method...
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

; we can define a procedure to find carry out Newton's
; method on a function like so
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; 1.40 - Cubic roots via Newton's Method

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

; e.g. (newtons-method (cubic 1 1 1) 1) ~ -1

; 1.41 - Apply a procedure twice
(define (twice op) ; op is a unary function
  (lambda (x) (op (op x))))

; what does (((twice (twice twice)) inc) 5) evaluate to?

; 1.42 - A procedure to compose unary functions

(define (compose f g)
  (lambda (x) (f (g x))))

; 1.43 - Repeated application

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

; 1.44 - Smoothing a function

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (nth-smoothed f n)
  ((repeated smooth n) f))

; 1.45 - Nth roots
;
; 1 avg. damp - n = 1, 2, 3
; 2 avg. damp - n = 4, 5, 6, 7
; 3 avg. damp - n = 8, 9, 10, 11, 12, 13, 14, 15 ???

(define (nth-root x n)
  (let ((d (inexact->exact (floor (log n 2)))))
    (fixed-point ((repeated average-damp d)
                  (lambda (y) (/ x
                                 (expt y (- n 1)))))
                 1.0)))

; 1.46 - Iterated improvement

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (x) (iter x)))

(define (sqrt-iter-gen x)
  ((iterative-improve
    (lambda (guess) (close-enough? (square guess) x))
    (lambda (guess) (average guess (/ x guess))))
   1.0))

(define (fixed-point-iter-gen f first-guess)
  ((iterative-improve
    (lambda (guess) (close-enough? guess (f guess)))
    f)
   first-guess))