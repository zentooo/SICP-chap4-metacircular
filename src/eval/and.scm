(if (and (global-variable-bound? (current-module) 'test-private) test-private)
  (define-module eval.and (export eval-and))
  (define-module eval.and (export-all)))
(select-module eval.and)

(use eval.common)

(define (eval-and exp env)
  (if (last-exp? exp)
    (eval exp env)
    (if (eval (first-exp exp env))
      (eval-and (rest-exps exp) env)
      #f)))

(provide "eval.and")
