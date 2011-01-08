(if (and (global-variable-bound? (current-module) 'test-private) test-private)
  (define-module eval.assignment (export-all))
  (define-module eval.assignment (export eval-assignment)))
(select-module eval.assignment)

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(provide "eval.assignment")
