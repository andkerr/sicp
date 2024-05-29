#lang racket/base

(require "table.rkt")

(require test-engine/racket-tests)

(provide get
         put
         get-coercion
         put-coercion
         attach-tag
         type-tag
         contents
         apply-generic)

(define op-table (make-table))
; regular operations are indexed on (op, types)
(define get (op-table 'lookup-proc))
(define put (op-table 'insert-proc!))
; coercions are indexed on (src-type, dst-type)
(define get-coercion get)
(define put-coercion put)

(define (attach-tag tag contents)
  (cons tag contents))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))
(define (same-type? x y)
  (eq? (type-tag x) (type-tag y)))

;; Generic Data-directed Arithmetic Operations
;;
;; it all flows from here...
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (apply-generic op . args)
  (display op)
  (display " ")
  (display args)
  (display "\n")
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (and (= (length args) 2) (apply same-type? args))
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (arg1 (car args))
                (arg2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2 (apply-generic op (t1->t2 arg1) arg2))
                    (t2->t1 (apply-generic op arg1 (t2->t1 arg2)))
                    (else (error
                            "No method for these types -- APPLY-GENERIC"
                            (list op type-tags))))))
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags)))))))

(check-expect (magnitude (make-complex-from-real-imag 3 4)) 5)
(check-expect (add -1 9) 8)
(check-expect (equ? (make-rational 2 4) (make-rational 6 12)) #t)
(check-expect (equ? (make-complex-from-real-imag 5 0)
                    (make-complex-from-mag-ang 5 0))
              #t)
(check-expect (=zero? 0) #t)
(check-expect (=zero? (make-rational 0 100)) #t)
(check-expect (=zero? (make-complex-from-mag-ang 0 0)) #t)
