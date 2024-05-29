#lang racket/base

(require compatibility/mlist)

(provide make-table
         lookup
         insert!)

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                   (mcons key-2 value))
                              (mcdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define (lookup key-1 key-2 table)
  ((table 'lookup-proc) key-1 key-2))
(define (insert! key-1 key-2 value table)
  ((table 'insert-proc!) key-1 key-2 value))
