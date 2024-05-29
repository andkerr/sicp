#lang racket

;; helpers

(require test-engine/racket-tests)
(require "table.rkt")

(define (square x) (* x x))

(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define op-table (make-table))
(define put (op-table 'insert-proc!))
(define get (op-table 'lookup-proc))

;; Data-directed programming: an _additive_ system for multiple different
;; object representations to use / supply the same set of procedures.

;; tagged data

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))

;; Our generic "interface". These are the operations that can be supported
;; by types installed in the system now or later.

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; ...for example, here are two self-contained complex number represenations.

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'install-rectangular-package->done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'install-polar-package->done)

;; Here's an example of procedures that a client might write making use of this
;; system. Say, they decide to write both (real,imaginary) and (magnitude,angle)
;; complex number constructors, returning a "rectangular" representation in the
;; first and a "polar" representation in the latter.

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; Exercise 2.73 - Data-directed derivatives

;; given the following deriv prodcedure (re-worked from section 2.3.2)...

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; write the procedures for derivativs of sums and products, and the auxillary
;; code required to install them in the table used by the complex number program

(define (install-deriv-package)
  ;; internal procedures
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (define (make-sum x y)
    (cond ((and (number? x) (number? y)) (+ x y))
          ((and (is-zero? x) y))
          ((and (is-zero? y) x))
          (else (list '+ x y))))
  (define (addend x) (car x))
  (define (augend x)
    (let ((rest (cdr x)))
      (if (null? (cdr rest))
        (car rest)
        (cons '+ rest))))

  (define (deriv-product operands var)
    (make-sum (make-product (multiplier operands)
                            (deriv (multiplicand operands) var))
              (make-product (deriv (multiplier operands) var)
                            (multiplicand operands))))
  (define (make-product x y)
    (cond ((and (number? x) (number? y)) (* x y))
          ((or (is-zero? x) (is-zero? y)) 0)
          ((is-one? x) y)
          ((is-one? y) x)
          (else (list '* x y))))
  (define (multiplier x) (car x))
  (define (multiplicand x)
    (let ((rest (cdr x)))
      (if (null? (cdr rest))
        (car rest)
        (cons '* rest))))

  (define (deriv-exponentiation operands var)
    (make-product
      (make-product (exponent operands)
                    (make-exponentiation (base operands)
                                         (sub1 (exponent operands))))
      (deriv (base operands) var)))
  (define (make-exponentiation x y)
    (cond
      ((is-zero? y) 1)
      ((is-one? y) x)
      (else (list '** x y))))
  (define (base x) (car x))
  (define (exponent x) (cadr x))

  ;; internal helpers
  (define (is-zero? x) (=number? x 0))
  (define (is-one? x) (=number? x 1))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation)
  'install-deriv-package->done)

;; Exercise 2.74

(define (install-division-foo)
  (define (tag x)
    (attach-tag 'foo x))
  (define (make-personnel-file)
    ;; Possibly deserialize something from disc here. A personnel file's contents
    ;; shouldn't have to be known at runtime, right?
    ;;
    ;; tag it with division 'foo
    (tag #t))
  (define (get-record employee file)
    ;; lookup dependent on division-specific file structure
    (tag #t)) ;; should records be tagged too??
  (define (get-salary record)
    0)
  ;; interface
  (put 'get-record 'foo get-record)
  (put 'get-salary 'foo get-salary)
  'done)

(define (get-record employee file)
  ((get 'get-record (division file)) employee file))
(define (get-salary record)
  #f)
(define (find-record employee files)
  #f)

;; Exercise 2.75 - complex numbers in the "message passing" style

;; (define (make-from-real-imag x y)
;;   (define (dispatch op)
;;     (cond ((eq? op 'real-part) x)
;;           ((eq? op 'imag-part) y)
;;           ((eq? op 'magnitude)
;;            (sqrt (+ (square x) (square y))))
;;           ((eq? op 'angle) (atan y x))
;;           (else
;;             (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
;;   dispatch)
;;
;; (define (make-from-mag-ang m a)
;;   (define (dispatch op)
;;     (cond ((eq? op 'real-part) (* m (cos a)))
;;           ((eq? op 'imag-part) (* m (sin a)))
;;           ((eq? op 'magnitude) m)
;;           ((eq? op 'angle) a)
;;           (else
;;             (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
;;   dispatch))

(install-rectangular-package)
(install-polar-package)
(install-deriv-package)

(check-expect (deriv '(* x y (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))
(check-expect (deriv '(+ x x x x) 'x) 4)
(check-expect (deriv '(* 1 1 1 1) 'x) 0)
(check-expect (deriv '(* 4 x) 'x) 4)
(check-expect (deriv '(** x 2) 'x) '(* 2 x))
