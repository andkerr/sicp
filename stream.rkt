#lang racket

(define (memo-proc f)
  (let ((has-run? false)
        (result false))
    (define (proc . args)
      (when (not has-run?)
        (set! has-run? true)
        (set! result (apply f args)))
      result)
    proc))

(define (stream-map proc argstream . argstreams)
  (define (do argstreams)
    (if (stream-empty? (car argstreams))
        '()
        (stream-cons
         (apply proc (map stream-first argstreams))
         (do (map stream-rest argstreams)))))
  (do (cons argstream argstreams)))

(define (stream-enumerate-interval a b)
  (if (> a b)
      '()
      (stream-cons
       a
       (stream-enumerate-interval (add1 a) b))))

(define (add-streams x y)
  (stream-map + x y))

(define (mul-streams x y)
  (stream-map * x y))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (divisible? x n)
  (= (remainder x n) 0))

(define (sieve s)
  (stream-cons
   (stream-first s)
   (sieve
    (stream-filter
     (lambda (x) (not (divisible? x (stream-first s))))
     (stream-rest s)))))

(define (partial-sums s)
  (define (iter acc s)
    (if (stream-empty? s)
        '()
        (let ((acc (+ acc (stream-first s))))
          (stream-cons acc (iter acc (stream-rest s))))))
  (iter 0 s))

(define (ints-from x)
  (stream-cons x (ints-from (add1 x))))

;; cool examples
(define primes (sieve (ints-from 2)))
(define powtwo (stream-cons 1 (add-streams powtwo powtwo)))
(define factorials (stream-cons 1 (mul-streams (ints-from 2) factorials)))

;; 3.56

(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))
               (s2car (stream-first s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car (merge (stream-rest s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car (merge s1 (stream-rest s2))))
                 (else
                  (stream-cons s1car
                               (merge (stream-rest s1)
                                      (stream-rest s2)))))))))

;; let S be the stream of positive integers with no prime factors other than
;; 2, 3, and 5
(define S (stream-cons 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

;; 3.57 - yield successive digits of the decimal expansion of num / den in the
;;        given radix.
(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; helper object -- return successive values from a stream

(define (mkiter s)
  (define (consume)
    (let ((val (stream-first s)))
      (set! s (stream-rest s))
      val))
  (define (dispatch m)
    (cond
      ((eq? m 'consume) consume)
      ((eq? m 'get) (lambda () s))
      (else (error "ITER -- unknown request" m))))
  dispatch)

(define (consume iter)
  ((iter 'consume)))

(define (get iter)
  ((iter 'get)))

;; 3.59, 3.60 - (infinite) power series as streams

(define (integrate-series s)
  (define (terms i s)
    (stream-cons (/ (stream-first s) i)
                 (terms (add1 i) (stream-rest s))))
  (terms 1 s))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

;; I don't understand how these work...I'm amazed...

(define sin-series
  (stream-cons 0 (integrate-series cos-series)))

(define cos-series
  (stream-cons 1 (scale-stream (integrate-series sin-series) -1)))

;; 3.60 - Multiplying Series

(define (add-series s1 s2)
  (add-streams s1 s2))

;; Cauchy Product - the terms of the product of two infinite series
;;                  a_i and b_i are given by...
;; c_0 = a_0b_0
;; c_1 = a_0b_1 + a_1b_0
;; c_2 = a_0b_2 + a_1b_1 + a_2b_0
;; ...
;; (it's convoluted...)
(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (add-streams (scale-stream (stream-rest s2) (stream-first s1))
                            (mul-series (stream-rest s1) s2))))

(define pyth-ident (add-series (mul-series sin-series sin-series)
                               (mul-series cos-series cos-series)))

;; 3.61 - Inverting a unit series

;; Compute the series X such that s * X = 1
(define (invert-unit-series s)
  (if (not (= (stream-first s) 1))
      (error "INVERT-UNIT-SERIES -- Expected a series with constant term equal to 1, got " (stream-first s))
      (stream-cons (stream-first s)
                   (scale-stream (mul-series (stream-rest s)
                                             (invert-unit-series s))
                                 -1))))

;; 3.62 - Dividing power series

(define (normalize-series s)
  (if (= (stream-first s) 0)
      (error "DIV-SERIES -- Stream must have a non-zero constant term, got " (stream-first s))
      (scale-stream s (/ 1 (stream-first s)))))

(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series (normalize-series s2))))

(define tan-series (div-series sin-series cos-series))
