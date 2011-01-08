(if (and (global-variable-bound? (current-module) 'test-private) test-private)
  (define-module eval.lambda (export-all))
  (define-module eval.lambda (export lambda-parameters lambda-body)))
(select-module eval.lambda)

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(provide "eval.lambda")
