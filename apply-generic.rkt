#lang racket

(require "table.rkt")

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

(define (same-type? args)
  (all-equal? (map type-tag args)))
(define (all-equal? lst)
  (= (length (remove-duplicates lst)) 1))

(define (apply-generic op . args)
  ; (display op)
  ; (display " ")
  ; (display args)
  ; (display "\n")
  (define (try-coercion type-tags)
    (if (null? type-tags)
        #f
        (let ((dst-type (car type-tags)))
          (let ((coercions (map (lambda (arg)
                                  (get-coercion (type-tag arg) dst-type))
                                args)))
            (if (member #f coercions)
                (try-coercion (cdr type-tags))
                (map (lambda (arg coerce) (coerce arg))
                     args
                     coercions))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (not (same-type? args))
              (let ((new-args (try-coercion type-tags)))
                (if new-args
                    (apply-generic op new-args)
                    (error
                     "No method for these types -- APPLY-GENERIC"
                     (list op type-tags))))
              (error
               "No method for these types -- APPLY-GENERIC"
               (list op type-tags)))))))
