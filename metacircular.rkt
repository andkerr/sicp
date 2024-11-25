#lang racket

(require compatibility/mlist)
(require racket/exn)

;;; The Metacircular Evaluator

;;; Eval

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and (clauses exp) env))
        ((or? exp) (eval-or (clauses exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply-meta (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;; Apply

(define (apply-meta procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; Conditionals

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env)) ;; What does 'true?' mean here?
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; Logical AND and OR

(define (eval-and clauses env)
  (if (null? clauses)
      true
      ((lambda (val)
        (if (not (true? val))
            false
            (if (null? (cdr clauses))
                val
                (eval-and (cdr clauses)))))
       (eval (car clauses)))))

(define (eval-or clauses env)
  (if (null? clauses)
      false
      ((lambda (val)
        (if (true? val)
            val
            (eval-or (cdr clauses))))
       (eval (car clauses)))))

;; Sequences

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; Assignments and definitions

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;; Syntax Definitions
;;;
;;; By defining itself in terms of the following "helper"
;;; routines, the metacircular evaluator above is syntax
;;; independent.

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (caddr exp)))) ; body

;; Ex. 4.16 - giving internal defines simultaneous scope
;;
;; This transformation allows the evaluator to generate "declared but
;; undefined" errors, rather than "undeclared variable" errors when mutually
;; recursive defines attempt to use one another's values "early" (I think...)
;;
;; (lambda <vars>
;;   (define u <e1>)
;;   (define v <e2>)
;;   <e3>)
;;
;; ...becomes
;;
;; (lambda <vars>
;;   (let ((u '*unassigned*)
;;         (v '*unassigned*))
;;     (set! u <e1>)
;;     (set! v <e2>)
;;     <e3>))
(define (scan-out-defines body)
  false)

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters (cons body '()))))

(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))
(define (named-let? exp)
  (and (tagged-list? exp 'let)
       (symbol? (cadr exp))))
(define (let-procname exp)
  (if (named-let? exp)
      (cadr exp)
      null))
(define (let-bindings exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))
(define (let-body exp)
  (if (named-let? exp)
      (cadddr exp)
      (caddr exp)))
(define (let-vars exp)
  (map car (let-bindings exp)))
(define (let-exps exp)
  (map cadr (let-bindings exp)))

(define (let->combination exp)
  (if (not (named-let? exp))
      (cons (make-lambda (let-vars exp) (let-body exp))
            (let-exps exp))
      (list
       'begin
       (list 'define (cadr exp) (make-lambda (let-vars exp) (let-body exp)))
       (cons (cadr exp) (let-exps exp)))))
(define (let*->nested-lets exp)
  (define (iter bindings)
    (if (null? bindings)
        (let-body exp)
        (list 'let
              (list (car bindings))
              (iter (cdr bindings)))))
  (iter (let-bindings exp)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;; Testing for application expressions this way forces them to come last in
;; the case statement that dispatches on expression type in the core of our
;; metacircular evaluator. Since a variety of expression types are represented
;; as S-expressions (i.e. lists) in our syntax, testing for application
;; anywhere else risks mis-classifying expressions' types and dispatching them
;; to the wrong evaluation rule (see Exercise 4.2).
;;

;; Should we want to recognize application expressions before other types of
;; S-expressions, we would need to introduce a keyword/operator to explicitly
;; indicate them, rather than merely defining them as S-expression that
;; satisfy no other category (see Exercise 4.2 also).
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (clauses exp) (cdr exp))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;; Evaluator Data Structures

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; Frames

(define (make-frame variables values)
  (list->mlist (map mcons variables values)))
(define (frame-variables frame)
  (mlist->list (mmap mcar frame)))
(define (frame-values frame)
  (mlist->list (mmap mcdr frame)))
(define (add-binding-to-frame! var val frame)
  (set-mcdr! frame (mcons (mcar frame) (mcdr frame)))
  (set-mcar! frame (mcons var val)))

(define f (make-frame (list 'a 'b 'c) (list 1 2 3)))

;; Operations on Environments

(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((binding (massoc var (first-frame env))))
        (if binding
            (let ((val (mcdr binding)))
              (if (eq? val '*unassigned*)
                  (error "Variable is declared but not yet assigned -- " var)
                  val))
            (lookup-variable-value
             var
             (enclosing-environment env))))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (massoc var frame)))
    (if binding
        (set-mcdr! binding val)
        (add-binding-to-frame! var val frame))))


(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let* ((frame (first-frame env))
             (binding (massoc var frame)))
        (if binding
            (set-mcdr! binding val)
            (set-variable-value!
             var
             val
             (enclosing-environment env))))))

;;; Primitive Procedures

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        ;; (list 'map map) ;; this doesn't work, see Ex. 4.14
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define apply-in-underlying-scheme apply) ;; see footnote 17
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



;;; Running the Evaluator

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (with-handlers ([exn:fail? (lambda (e)
                               (displayln (exn->string e))
                               (driver-loop))])
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (let ((output (eval input the-global-environment)))
        (announce-output output-prompt)
        (user-print output)))
    (driver-loop)))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>))
      (display object)))

;; here we go...

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
