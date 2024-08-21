#lang racket

(require compatibility/mlist)

(provide make-table
         lookup
         insert!)

;; Tables are constructed from lists of (key, value)
;; pairs. They may be nested arbitrarily, in which
;; case the first element of a sublist designates
;; a subtable's key.
;; [
;;   "*table*"
;;
;;   [
;;     "add"
;;
;;     ('(complex complex) add-complex)
;;     ('(rational rational) add-rational)
;;   ]
;;
;;   [
;;     "sub"
;;     ('(complex complex) add-complex)
;;     ('(rational rational) add-rational)
;;   ]
;; ]

(define (print-table t)
  (define w 2)
  (define (print-subtable t d)
    (mfor-each
(lambda (x)
                 (let ((k (mcar x))
                       (v (mcdr x)))
                   (display (make-string (* d w) #\space))
                   (display k)
                   (display ":")
                   (if (not (mlist? v))
                       (begin
                         (display " ")
                         (display v)
                         (display "\n"))
                       (begin
                         (display "\n")
                         (print-subtable v (add1 d))))))
               t))
  (print-subtable t 0))

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup keys)
      (define (lookup-one keys table)
        (let ((key (car keys))
              (rest (cdr keys))
              (records (mcdr table)))
        (let ((record (massoc key records)))
          (if (not record)
              false
                (if (null? rest)
                    (mcdr record)
                    (lookup-one rest record))))))
      (lookup-one keys local-table))
    (define (insert! value keys)
      (define (insert-one! keys table)
        (let ((key (car keys))
              (rest (cdr keys))
              (records (mcdr table)))
          (let ((record (massoc key records)))
            (if (null? rest)
                (if record
                    (set-mcdr! record value)
                    (set-mcdr! table (mcons (mcons key value) records)))
                (cond
                  ((not record)
                   (begin
                     (set-mcdr! table (mcons (mlist key) records))
                     (insert-one! rest (mcar (mcdr table)))))
                  ((mlist? record)
                   (insert-one! rest record))
                  ((mpair? record)
                   (begin
                     (set-mcdr! record '())
                     (insert-one! rest record)))
                  (else (error "INSERT -- unrecognized record type"
                               record)))))))
      (insert-one! keys local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (lookup table key . keys)
  ((table 'lookup) (cons key keys)))

(define (insert! table value key . keys)
  ((table 'insert!) value (cons key keys)))
