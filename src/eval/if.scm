(if (and (global-variable-bound? (current-module) 'test-private) test-private)
  (define-module eval.if (export-all))
  (define-module eval.if (export eval-if)))
(select-module eval.if)

(use eval.common)

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))


(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    #f))

(provide "eval.if")
