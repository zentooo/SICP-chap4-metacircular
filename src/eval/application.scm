(if (and (global-variable-bound? (current-module) 'test-private) test-private)
  (define-module eval.application (export apply operands operator list-of-values))
  (define-module eval.application (export-all)))
(select-module eval.application)

(define (apply procedure arguments)
  (cond [(primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure)))]
         [else (error "Unknown procedure type -- APPLY" procedure)]))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body) (caddr p))
(define (procedure-environment) (cadddr p))


(define (operator exp) (car exp))

(define (operand exp) (cdr exp))

(define (no-operands? exp) (null? (cdr exp)))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(provide "eval.application")
