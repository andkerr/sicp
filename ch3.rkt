#lang racket

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
