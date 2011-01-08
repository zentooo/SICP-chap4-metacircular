(if (and (global-variable-bound? (current-module) 'test-private) test-private)
  (define-module eval.cond (export-all))
  (define-module eval.cond (export cond->if)))
(select-module eval.cond)

(use eval.begin)

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

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

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

(provide "eval.cond")
