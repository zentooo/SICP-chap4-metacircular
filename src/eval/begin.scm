(if (and (global-variable-bound? (current-module) 'test-private) test-private)
  (define-module eval.begin (export-all))
  (define-module eval.begin (export eval-sequence sequence->exp)))
(select-module eval.begin)

(use eval.common)

(define (eval-sequence exps env)
  (cond [(last-exp? exps) (eval (first-exp exps) env)]
        [else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)]))


(define (begin-actions exp) (cdr exp))

(define (make-begin seq) (cons 'begin seq))

(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

(provide "eval.begin")
