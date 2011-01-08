(use gauche.test)
(test-start "eval module")

(add-load-path "../src/")
(define test-private #t)

(use eval)
(test-section "self-evaluating")
(test* "self-evaluating-string" #t (self-evaluating? "string"))
(test* "self-evaluating-number" #t (self-evaluating? 1))
(test* "self-evaluating-number" #t (self-evaluating? 1))
(test* "self-evaluating-definition" #f (self-evaluating? (list 1 2 3)))

(use eval.common)
(test-section "tagged-list?")
(test* "tagged-list?-true" #t (tagged-list? '(tag 0 1 2) 'tag))
(test* "tagged-list?-othertag" #f (tagged-list? '(gat 0 1 2) 'tag))
(test* "tagged-list?-notag" #f (tagged-list? '(0 1 2) 'tag))

(use eval.quote)
(test-section "quoted?")
(test* "quoted?-true" #t (quoted? '(quote 0 1 2)))
(test* "quoted?-false" #f (quoted? '(tag 0 1 2)))

(use eval.assignment)
(test-section "assignment?")
(test* "assignment?-true" #t (assignment? '(set! 0 1 2)))
(test* "assignment?-false" #f (assignment? '(tag! 0 1 2)))

(use eval.definition)
(test-section "definition?")
(test* "definition?-true" #t (definition? '(define hoge 1)))
(test* "definition?-false" #f (definition? '(list 0 1 2)))

(test* "definition-variable-nonfunction" 'foo (definition-variable '(define foo bar)))
(test* "definition-variable-function" 'foo (definition-variable '(define (foo bar baz))))

(use eval.lambda)
(test-section "lambda?")
(test* "lambda?-true" #t (lambda? '(lambda 1 2 3)))
(test* "lambda?-false" #f (lambda? '(foo 1 2 3)))
(test* "lambda-parameters" 'foo (lambda-parameters '(lambda foo bar)))
(test* "lambda-body" '(bar) (lambda-body '(lambda foo bar)))

(use eval.let)
(test-section "let?")
(test* "let-bindings" '((var1 exp1) (var2 exp2)) (let-bindings '(let ((var1 exp1) (var2 exp2)) (list 1 2 3))))
(test* "let-body" '(list 1 2 3) (let-body '(let ((var1 exp1) (var2 exp2)) (list 1 2 3))))
(test* "let->combination" '((lambda (var1 var2) (list 1 2 3)) exp1 exp2) (let->combination '(let ((var1 exp1) (var2 exp2)) (list 1 2 3))))
(test* "let*->nested-lets" '(let ((x 1)) (let ((y x)) (list 1 2 3))) (let*->nested-lets '(let* ((x 1) (y x)) (list 1 2 3))))

(use eval.if)
(test-section "if?")
(test* "if?-true" #t (if? '(if 1 2 3)))
(test* "if?-false" #f (if? '(foo 1 2 3)))
(test* "if-predicate" 'foo (if-predicate '(if foo bar)))
(test* "if-consequent" 'bar (if-consequent '(if foo bar)))
(test* "if-alternative-exist" 'baz (if-alternative '(if foo bar baz)))
(test* "if-alternative-nonexist" #f (if-alternative '(if foo bar)))

(use eval.begin)
(test-section "begin?")
(test* "begin?" #t (begin? '(begin foo bar baz)))
(test* "begin-actions" '(foo bar baz) (begin-actions '(begin foo bar baz)))
(test* "last-exp?" #t (last-exp? '(foo)))
(test* "sequence->exp-null" '() (sequence->exp '()))
(test* "sequence->exp-one" 'foo (sequence->exp '(foo)))
(test* "sequence->exp-begin" #t (begin? (sequence->exp '(foo bar baz))))

(use eval.cond)
(test-section "cond?")
(test* "cond?" #t (cond? '(cond foo bar baz)))
(test* "cond-else-clause?" #t (cond-else-clause? '(else foo bar baz)))
(test* "cond->if-null" #f (cond->if '(cond)))
(test* "cond->if-else" #t (cond->if '(cond [else #t])))
(test* "cond->if-else-error" (test-error) (cond->if '(cond
                                             [else #t]
                                             [else #f])))
(test* "cond->if" #t (if? (cond->if '(cond
                                           [(null? 'a) (list 1 2 3)]
                                           [else #f]))))

(test-section "cond-extractor-form")
(test* "extractor-clause?" #t (extractor-clause? '((assoc b ((a 1) (b 2))) => cadr)))
(test* "extractor-target-function" 'cadr (extractor-target-function '((assoc b ((a 1) (b 2))) => cadr)))

(use eval.env)
(test-section "env")

(define sample-environment1 (extend-environment '(var1 var2) '(val1 val2) '()))
(test* "extend-environment1" '(((var1 var2) val1 val2)) sample-environment1)

(define sample-environment2 (extend-environment '(var3 var4) '(val3 val4) sample-environment1))
(test* "extend-environment2" '(((var3 var4) val3 val4)  ((var1 var2) val1 val2)) sample-environment2)

(test* "lookup-variable-value-top" 'val3 (lookup-variable-value 'var3 sample-environment2))
(test* "lookup-variable-value-next" 'val1 (lookup-variable-value 'var1 sample-environment2))

(set-variable-value! 'var3 'valx sample-environment2)
(test* "set-variable-value-top" 'valx (lookup-variable-value 'var3 sample-environment2))
(set-variable-value! 'var3 'val3 sample-environment2)

(set-variable-value! 'var1 'valx sample-environment2)
(test* "set-variable-value-next" 'valx (lookup-variable-value 'var1 sample-environment2))
(set-variable-value! 'var1 'val1 sample-environment2)

(define-variable! 'var5 'val5 sample-environment2)
(test* "define-variable!" 'val5 (lookup-variable-value 'var5 sample-environment2))

(make-unbound! 'var5 sample-environment2)
(test* "make-unbound!" (test-error) (lookup-variable-value 'var5 sample-environment2))

(define initial-environment (setup-environment))
(test* "initial-environment-1" #t (lookup-variable-value '#t initial-environment))
(test* "initial-environment-3" `(primitive ,car) (lookup-variable-value 'car initial-environment))

(test-end)
