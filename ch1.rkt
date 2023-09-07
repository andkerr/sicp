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
        z
        (iter y
              z
              (+ (* 3 x) (* 2 y) (* 1 z))
              (dec count))))
  (if (< n 3)
      n
      (iter 0 1 2 (- n 2))))