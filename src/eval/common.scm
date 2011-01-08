(define-module eval.common (export-all))
(select-module eval.common)

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

(define (first-exp seq) (car seq))

(define (rest-exps) (cdr seq))

(define (last-exp? seq) (null? (cdr seq)))

(provide "eval.common")
