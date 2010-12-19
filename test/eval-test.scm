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

(test-section "tagged-list?")
(test* "tagged-list?-true" #t (tagged-list? '(tag 0 1 2) 'tag))
(test* "tagged-list?-othertag" #f (tagged-list? '(gat 0 1 2) 'tag))
(test* "tagged-list?-notag" #f (tagged-list? '(0 1 2) 'tag))

(test-section "quoted?")
(test* "quoted?-true" #t (quoted? '(quote 0 1 2)))
(test* "quoted?-false" #f (quoted? '(tag 0 1 2)))

(test-section "assignment?")
(test* "assignment?-true" #t (assignment? '(set! 0 1 2)))
(test* "assignment?-false" #f (assignment? '(tag! 0 1 2)))

(test-section "definition?")
(test* "definition?-true" #t (definition? '(define hoge 1)))
(test* "definition?-false" #f (definition? '(list 0 1 2)))

(test* "definition-variable-nonfunction" 'foo (definition-variable '(define foo bar)))
(test* "definition-variable-function" 'foo (definition-variable '(define (foo bar baz))))

(test-section "lambda?")
(test* "lambda?-true" #t (lambda? '(lambda 1 2 3)))
(test* "lambda?-false" #f (lambda? '(foo 1 2 3)))
(test* "lambda-parameters" 'foo (lambda-parameters '(lambda foo bar)))
(test* "lambda-body" '(bar) (lambda-body '(lambda foo bar)))

(test-section "if?")
(test* "if?-true" #t (if? '(if 1 2 3)))
(test* "if?-false" #f (if? '(foo 1 2 3)))
(test* "if-predicate" 'foo (if-predicate '(if foo bar)))
(test* "if-consequent" 'bar (if-consequent '(if foo bar)))
(test* "if-alternative-exist" 'baz (if-alternative '(if foo bar baz)))
(test* "if-alternative-nonexist" #f (if-alternative '(if foo bar)))

(test-section "begin?")
(test* "begin?" #t (begin? '(begin foo bar baz)))
(test* "begin-actions" '(foo bar baz) (begin-actions '(begin foo bar baz)))
(test* "last-exp?" #t (last-exp? '(foo)))
(test* "sequence->exp-null" '() (sequence->exp '()))
(test* "sequence->exp-one" 'foo (sequence->exp '(foo)))
(test* "sequence->exp-begin" #t (begin? (sequence->exp '(foo bar baz))))

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

(test-end)
