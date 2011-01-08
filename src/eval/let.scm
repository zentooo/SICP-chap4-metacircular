(if (and (global-variable-bound? (current-module) 'test-private) test-private)
  (define-module eval.let (export-all))
  (define-module eval.let (export let->combination let*->nested-lets)))
(select-module eval.let)

(define (let-bindings exp)
  (cadr exp))

(define (let-body exp)
  (caddr exp))

(define (let->combination let-clause)
  (let ((bindings (let-bindings let-clause)) (body (let-body let-clause)))
    `((lambda ,(map car bindings) ,body) ,@(map cadr bindings))))

(define (let*->nested-lets let*-clause)
  (let ((bindings (let-bindings let*-clause)) (body (let-body let*-clause)))
    (fold-right (lambda (x y) `(let (,x) ,y)) body bindings)))

(provide "eval.let")
