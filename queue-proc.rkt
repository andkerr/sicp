#lang racket
(require compatibility/mlist)

;; A procedural queue implementation

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?) (null? front-ptr))
    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue")
          (mcar front-ptr)))
    (define (insert! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               (print))
              (else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               (print)))))
    (define (delete!)
      (cond ((empty?)
             (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (mcdr front-ptr))
             (print))))
    (define (print)
      (mlist->list front-ptr))
    (define (dispatch m)
      (cond
        ((eq? m 'empty?) empty)
        ((eq? m 'front) front)
        ((eq? m 'insert!) insert!)
        ((eq? m 'delete!) delete!)
        ((eq? m 'print) print)
        (else (error "QUEUE -- unrecognized operation" m))))
    dispatch))

(define (empty-queue? queue)
  ((queue 'empty)))

(define (front-queue queue)
  ((queue 'front)))

(define (insert-queue! queue item)
  ((queue 'insert!) item))

(define (delete-queue! queue)
  ((queue 'delete!)))

(define (print-queue queue)
  ((queue 'print)))

(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(insert-queue! q 'c)
