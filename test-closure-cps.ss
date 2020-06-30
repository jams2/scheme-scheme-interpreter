(include "test-runner.ss")
(include "closure-cps.ss")

(run-tests
 [test-case-equal? "numbers are self evaluating"
		   5
		   (evaluate 5 (primitive-env))]
 [test-case-equal? "variables are assigned"
		   5
		   (evaluate '(begin (define x 5)
				     x)
			     (primitive-env))]
 [test-case-equal? "sequences are evaluated"
		   3
		   (evaluate '(begin 5
				     4
				     3)
			     (primitive-env))]
 [test-case-equal? "variables are mutated with set!"
		   5
		   (evaluate '(begin (define x 4)
				     (set! x 5)
				     x)
			     (primitive-env))]
 [test-case "lambdas generate a tagged list representation"
	    (lambda (expected actual) (eq? (car actual) expected))
	    'procedure
	    (evaluate '(lambda (x) x) (primitive-env))]
 [test-case-equal? "if statements are evaluated"
		   5
		   (evaluate '(if true
				  5
				  6)
			     (primitive-env))]
 [test-case-equal? "let expressions are evaluated"
		   5
		   (evaluate '(let ([x 5]) x) (primitive-env))]
 [test-case-equal? "primitive applications are evaluated"
		   5
		   (evaluate '(+ 1 2 2) (primitive-env))]
 [test-case-equal? "primitive ops with single arg are evaluated"
		   -1
		   (evaluate '(- 1) (primitive-env))]
 )

(include "test-analysing-interpreter.ss")

(time (evaluate '((lambda (i)
		    ((lambda (y) ((y y) i))
		     (lambda (f)
		       (lambda (n)
			 (if (< n 2) n
			     (+ ((f f) (- n 1))
				((f f) (- n 2))))))))
		  30)
		(primitive-env)))
