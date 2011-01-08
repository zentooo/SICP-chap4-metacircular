(if (and (global-variable-bound? (current-module) 'test-private) test-private)
  (define-module eval.definition (export-all))
  (define-module eval.definition (export eval-definition)))
(select-module eval.definition)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(provide "eval.definition")
