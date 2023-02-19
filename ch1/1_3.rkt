#lang sicp

; Exercise 1.3
;
; Define a procedure that takes three numbers as arguments and returns
; the sum of the squares of the two larger numbers.

(define (sum-of-squares-largest x y z)
  (cond ((= x (min x y z)) (sum-of-squares y z))
        ((= y (min x y z)) (sum-of-squares x z))
        (else (sum-of-squares x y))))

(define (min a b c)
  (cond ((and (< a b) (< a c)) a)
        ((and (< b a) (< b c)) b)
        (else c)))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (square x) (* x x))


; some simple tests

(define (test actual expected)
  (if (= actual expected)
    "PASSED!"
    "FAILED!"))

"(sum-of-squares-largest 1 2 3), expected = 13"
(test (sum-of-squares-largest 1 2 3) 13)

"(sum-of-squares-largest 3 2 1), expected = 13"
(test (sum-of-squares-largest 3 2 1) 13)

"(sum-of-squares-largest -5 -3 -4), expected = 25"
(test (sum-of-squares-largest -5 -3 -4) 25)

"(sum-of-squares-largest 4 4 3), expected = 32"
(test (sum-of-squares-largest 4 4 3) 32)
