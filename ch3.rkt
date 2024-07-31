#lang racket
(require compatibility/mlist) ;; we'll we doing plenty of mutating soon...

;; 3.1
(define (make-accumulator sum)
  (lambda (amt)
    (begin (set! sum (+ sum amt))
           sum)))

;; 3.2
(define (make-monitored f)
  (let ((cnt 0))
    (define (mf x)
      (cond ((eq? x 'reset-count) (begin (set! cnt 0) cnt))
            ((eq? x 'how-many-calls?) cnt)
            (else (set! cnt (add1 cnt))
                  (f x))))

    mf))

;; 3.3 / 3.4

(define (make-protected obj password)
  (let ((consecutive-misattempts 0))
    (define (check-password test-password)
      (if (eq? test-password password)
          (begin
            (set! consecutive-misattempts 0)
            #t)
          (begin
            (set! consecutive-misattempts (add1 consecutive-misattempts))
            (if (< consecutive-misattempts 8)
                (error "Incorrect password")
                (error "Eight or more incorrect password attempts -- calling the cops")))))
    (define (dispatch m p)
      (check-password p)
      (if (eq? m 'make-protected-get)
          obj
          (obj m)))
    dispatch))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  (make-protected dispatch password))

;; 3.7
;;
;; To allow the same account to be accessed by multiple (identifier, password)
;; pairs, I decided to move the authorization code to its own object that can
;; wrap the same account more than once. Now that its separated, it can be
;; used to create other objects whose operations are password-protected too.
;; Neat (to me)!
(define (make-joint account curr-password new-password)
  (make-protected (account 'make-protected-get curr-password) new-password))

;; 3.5 - Monte Carlo integration
(define (random-in-range low high)
  (+ low (* (random) (- high low))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (if (= trials-remaining 0)
        (/ trials-passed trials)
        (iter (sub1 trials-remaining) (+ trials-passed (if (experiment) 1 0)))))
  (iter trials 0))

(define (integrate P x1 y1 x2 y2 trials)
  (define (do-trial)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (let ((area (* (- x2 x1) (- y2 y1))))
    (* area (monte-carlo trials do-trial))))

(define (in-unit-circle? x y)
  (<= (+ (* x x) (* y y)) 1))

(define (estimate-pi trials)
  (exact->inexact (integrate in-unit-circle? -1 -1 1 1 trials)))

;; 3.8
;;
;; Here's a function that is _not_ regular, i.e. calling it on equal arguments
;; is not guaranteed to produce equal results. For example, for non-zero
;; inputs it alternates between return the input and returning 0.
(define f
  (let ((m #f))
    (lambda (x)
      (set! m (not m))
      (* x (if m 1 0)))))

;; This can be used to test the order that operators evaluate subexpressions.
;; If the expression below returns 0, (f 0) was evaluated first. If it returns
;; 1, (f 1) was evaulated first.
(define (test-eval-order)
  (display (+ (f 0) (f 1)))
  (display "\n")
  (display (+ (f 1) (f 0)))
  (display "\n"))

;; 3.14
;;
;; This mystery procedure reverses a mutable list by repeatedly setting the
;; second element of the first pair to the list of elements "reversed so far".
;; When passed a list via a named variable, the variable will point only to
;; to the first pair afterwards, with the rest of the list seemingly stripped
;; away (see the variable v in the mystery-test procedure below). In this
;; sense, the mystery procedure does not reverse its argument in-place. The
;; fully reversed list can only be accessed if the return value is bound to
;; some parameter.
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))

(define (mystery-test)
  (define v (mlist 1 2 3))
  (define w (mystery v))
  (display v)
  (display "\n")
  (display w)
  (display "\n"))

(define x '(a b))
(define z1 (cons x x))
(define z2 (cons '(a b) '(a b)))

;; 3.16
;;
;; This is a "buggy" version of a procedure to count the number of distinct
;; pairs in a list structure. It doesn't account for the possibility that
;; pairs may be shared under the hood (e.g. what if the car and cdr of a pair
;; are the same pair?), and possibly over count certain pairs.
(define (count-pairs-bad x)
  (if (not (pair? x)) ;; cycle -> BOOM!
      0
      (+ (count-pairs-bad (car x))
         (count-pairs-bad (cdr x))
         1)))

;; 3.17
;;
;; This version of count-pairs accounts for shared objects by maintaining a
;; list of already-seen pairs while traversing the provided structure. I don't
;; think it's possible to make the list a direct argument of the traversal
;; subroutine count (i.e. we need to mutate a globally visible list instead),
;; since objects shared between "branches" wouldn't be accounted for (the
;; alread-seen list would really be a stack).
(define (count-pairs-good x)
  (let ((seen '()))
    (define (update-seen p)
      (if (memq p seen)
          #f
          (begin
            (set! seen (cons p seen))
            #t)))
    (define (count x)
      (if (not (pair? x))
          0
          (let ((new-pair? (update-seen x)))
            (+ (count (car x))
               (count (cdr x))
               (if new-pair? 1 0)))))
  (count x)))

;; 3.18 / 3.19 - Finding cycles in a (linked) list, blah blah
(define (has-cycle? seq)
  (define (find-cycle slow fast first?)
    (cond
      ((or (null? fast) (null? (cdr fast))) #f)
      ((and (not first?) (eq? slow fast)) #t)
      (else (find-cycle (cdr slow) (cdr (cdr fast)) #f))))
  (find-cycle seq seq #t))
