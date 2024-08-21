#lang racket

(require "multi-table.rkt")

(define (memoize f)
  (let ((m (make-table)))
    (lambda (x)
      (let ((prev (lookup m x)))
        (if prev
            prev
            (let ((res (f x)))
              (insert! m res x)
              res))))))

(define mfib
  (memoize (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else
            (+ (mfib (- n 1))
               (mfib (- n 2))))))))

;; TODO: Can you modify the memoize procedure to automatically replace
;;       recursive calls to the original procedure with calls to the memoized
;;       one (perhaps using macros)? Currently, (memoize fib) does not return
;;       a procedure that computes F_n in O(n) time, since the memoized
;;       procedure stills calls into (unmemoized) fib to compute subresults.
