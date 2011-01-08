(if (and (global-variable-bound? (current-module) 'test-private) test-private)
  (define-module eval.env (export setup-environment))
  (define-module eval.env (export-all)))
(select-module eval.env)

(use srfi-1)

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars) (env-loop (enclosing-environment env))]
            [(eq? var (car vars)) (car vals)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let1 frame (first-frame env)
        (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars) (env-loop (enclosing-environment env))]
            [(eq? var (car vars)) (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let1 frame (first-frame env)
        (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let1 frame (first-frame env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond [(null? vars) (add-binding-to-frame! var val frame)]
              [(eq? var (car vars)) (set-car! vals val)]
              [else (scan (cdr vars) (cdr vals))]))
      (scan (frame-variables frame) (frame-values frame)))
    (env-loop env)))

(define (make-unbound! var env)
  (let1 frame (first-frame env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond [(null? vars) (error "Unbound variable" var)]
              [(eq? var (car vars))
                 (begin
                   (set-car! frame (delete! (car vars) vars))
                   (set-cdr! frame (delete! (car vals) vals)))]
              [else (scan (cdr vars) (cdr vals))]))
      (scan (frame-variables frame) (frame-values frame)))
    (env-loop env)))

(define primitive-procedures
  `((car ,car) (cdr ,cdr) (cons ,cons) (null? ,null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (procedure) `(primitive ,(cadr procedure))) primitive-procedures))

(define (setup-environment)
  (let1 initial-env (extend-environment (primitive-procedure-names) (primitive-procedure-objects) the-empty-environment)
   (define-variable! '#t #t initial-env)
   (define-variable! '#f #f initial-env)
   initial-env))

(provide "eval.env")
