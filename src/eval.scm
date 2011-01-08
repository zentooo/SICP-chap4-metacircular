(if test-private
  (define-module eval (export-all))
  (define-module eval (export eval)))
(select-module eval)

(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(if? exp) (eval-if exp env)]
        [(and? exp) (eval-and exp env)]
        [(or? exp) (eval-or exp env)]
        [(lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env)]
        [(let? exp) (eval (let->conbination exp) env)]
        [(let*? exp) (eval (let*->nested-lets exp) env)]
        [(begin? exp) (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (eval (cond->if exp env))]
        [(application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env))]
        [else (error "Unknown expression type -- EVAL" exp)]))


; common util
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

(define (first-exp seq) (car seq))

(define (rest-exps) (cdr seq))

(define (last-exp? seq) (null? (cdr seq)))


; self-evaluating?
(define (self-evaluating? exp)
  (cond [(number? exp) #t]
        [(string? exp) #t]
        [else #f]))


; variable?
(define (variable? exp)
  (symbol? exp))


; quoted?
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))


; assignment?
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)


; definition?
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


; if?
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


; and?
(define (and? exp) (tagged-list? exp 'and))
(define (eval-and exp env)
  (if (last-exp? exp)
    (eval exp env)
    (if (eval (first-exp exp env))
      (eval-and (rest-exps exp) env)
      #f)))


; or?
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
  (if (last-exp? exp)
    (eval exp env)
    (if (eval (first-exp exp env))
      #t
      (eval-or (rest-exps exp) env)
      )))


; lambda?
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))


; let?
(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))

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


; begin?
(define (eval-sequence exps env)
  (cond [(last-exp? exps) (eval (first-exp exps) env)]
        [else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)]))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

(define (make-begin seq) (cons 'begin seq))


; cond?
(define (cond? exp) (tagged-list? exp 'cond))

(define (extractor? exp) (eq? exp '=>))

(define (extractor-clause? clause)
  (extractor? (cadr clause)))

(define (extractor-target-function clause)
  (caddr clause))

(define (process-extractor clause)
  (eval ((extractor-target-function clause) (cond-predicate clause))))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    #f
    (let ((first (car clauses)) (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause not last -- COND->IF" clauses))
        (make-if (cond-predicate first)
                 (if (extractor-clause? first)
                   (process-extractor first)
                   (sequence->exp (cond-actions first)))
                 (expand-clauses rest))))))


; application?
(define (apply procedure arguments)
  (cond [(primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure)))]
         [else (error "Unknown procedure type -- APPLY" procedure)]))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operand exp) (cdr exp))

(define (no-operands? exp) (null? (cdr exp)))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))


(provide "eval")
