(define-module eval.quote (export text-of-quotation))
(select-module eval.quote)

(define (text-of-quotation exp)
  (cadr exp))

(provide "eval.quote")
