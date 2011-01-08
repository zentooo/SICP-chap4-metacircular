(if (and (global-variable-bound? (current-module) 'test-private) test-private)
  (define-module eval (export eval))
  (define-module eval (export-all)))
(select-module eval)

(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(if? exp) (eval-if exp env)]
        [(and? exp) (eval-and exp env)]
        [(or? exp) (eval-or exp env)]
        [(lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env)]
        [(let? exp) (eval (let->combination exp) env)]
        [(let*? exp) (eval (let*->nested-lets exp) env)]
        [(begin? exp) (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (eval (cond->if exp env))]
        [(application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env))]
        [else (error "Unknown expression type -- EVAL" exp)]))

(use eval.common)

(define (self-evaluating? exp)
  (cond [(number? exp) #t]
        [(string? exp) #t]
        [else #f]))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(use eval.quote)

(define (assignment? exp) (tagged-list? exp 'set!))
(use eval.assignment)

(define (definition? exp) (tagged-list? exp 'define))
(use eval.definition)

(define (if? exp) (tagged-list? exp 'if))
(use eval.if)

(define (and? exp) (tagged-list? exp 'and))
(use eval.and)

(define (or? exp) (tagged-list? exp 'or))
(use eval.or)

(define (lambda? exp) (tagged-list? exp 'lambda))
(use eval.lambda)

(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))
(use eval.let)

(define (begin? exp) (tagged-list? exp 'begin))
(use eval.begin)

(define (cond? exp) (tagged-list? exp 'cond))
(use eval.cond)

(define (application? exp) (pair? exp))
(use eval.application)

(provide "eval")
