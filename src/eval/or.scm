(define-module eval.or (export eval-or))
(select-module eval.or)

(use eval.common)

(define (eval-or exp env)
  (if (last-exp? exp)
    (eval exp env)
    (if (eval (first-exp exp env))
      #t
      (eval-or (rest-exps exp) env)
      )))

(provide "eval.or")
