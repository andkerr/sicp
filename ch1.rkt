#lang sicp

; Exercise 1.11
; f(n) = n, for n < 3, and = f(n - 1) + 2f(n - 2) + 3f(n - 3), otherwise
;
; f(0) = 0
; f(1) = 1
; f(2) = 2
; f(3) = 1* 2 + 2* 1 + 3* 0 =   4
; f(4) = 1* 4 + 2* 2 + 3* 1 =  11
; f(5) = 1*11 + 2* 4 + 3* 2 =  25
; f(6) = 1*25 + 2*11 + 3* 4 =  59
; f(7) = 1*59 + 2*25 + 3*11 = 142

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

; Fast Exponentiation
;
; b^n = (b^n/2)^2,   if n is even
;     = b * b^(n-1), if n is odd
(define (square x) (* x x))

(define (fast-expt b n)
  (cond
    ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (dec n))))))

; 1.16 - Linear Iterative Fast Exponentiation
;
; Hint: [b^(n/2)]^2 = [b^2]^(n/2) = b^n

(define (fast-expt-iter b n)
  (define (iter a b n)
    (cond
      ((= n 0) a)
      ((even? n) (iter a (square b) (/ n 2)))
      (else (iter (* a b) b (dec n)))))
  (iter 1 b n))