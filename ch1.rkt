#lang sicp

; Helpers
(define (square x) (* x x))
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (divides? a b)
  (= (remainder b a) 0))

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
      (+
       (binom-coeff (dec x) y)
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

(smallest-divisor   199)
(smallest-divisor  1999)
(smallest-divisor 19999)

; 1.22 - Prime Timing

(define (prime? n)
  (= (smallest-divisor n) n))


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
  