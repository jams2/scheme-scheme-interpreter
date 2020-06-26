(include "analysing-interpreter.ss")
(include "test-runner.ss")

(run-tests
 [test-case-equal? "numbers are self-evaluating"
		   1
		   (evaluate 1 (primitive-env))]
 [test-case-equal? "floating point numbers are self-evaluating"
		   3.145
		   (evaluate 3.145 (primitive-env))]
 [test-case-equal? "numbers with exponents are self-evaluating"
		   3e9
		   (evaluate 3e9 (primitive-env))]
 [test-case-equal? "quoted exprs return the expr"
		   'x
		   (evaluate (quote 'x) (primitive-env))]
 [test-case-equal? "variables are looked up in the env"
		   5
		   (evaluate 'x (append '(((x . 5))) (primitive-env)))]
 [test-raises 'set-variable!
	      '(x)
	      (evaluate '(set! x 5) (primitive-env))]
 [test-case-equal? "begin forms are evaluated sequentially"
		   5
		   (evaluate '(begin (define x 5) (define y 6) x) (primitive-env))]
 [test-raises 'gen-sequence
	      '(())
	      (evaluate '(begin) (primitive-env))]
 [test-case "lambda expressions evaluate to a 'procedure tagged list"
	    (lambda (expected actual) (eq? (car actual) expected))
	    'procedure
	    (evaluate '(lambda (x) x) (primitive-env))]
 [test-case-equal? "left left identity function is evaluated"
		   5
		   (evaluate '((lambda (x) x) 5) (primitive-env))]
 [test-case-equal? "primitive + is applied"
		   7
		   (evaluate '(+ 3 4) (primitive-env))]
 [test-case-equal? "primitive / is applied"
		   (/ 3 7)
		   (evaluate '(/ 3 7) (primitive-env))]
 [test-case-equal? "successful if predicate evaluates only the consequent"
		   6
		   (evaluate '(begin (define x 6)
				     (if true
					 x
					 (begin (set! x 7)
						x)))
			     (primitive-env))]
 [test-case-equal? "failed if predicate evaluates only the alternative"
		   6
		   (evaluate '(begin (define x 6)
				     (if false
					 (begin (set! x 7) x)
					 x))
			     (primitive-env))]
 [test-case-equal? "if forms return false if the predicate fails and their is no alt"
		   #f
		   (evaluate '(if false true) (primitive-env))]
 [test-case-equal? "a definition and then call of fibonacci is correctly evaluated"
		   8
		   (evaluate '(begin
				(define fib
				  (lambda (n)
				    (if (< n 2)
					n
					(+ (fib (- n 1))
					   (fib (- n 2))))))
				(fib 6))
			     (primitive-env))]
 [test-case-equal? "let forms are evaluated"
		   8
		   (evaluate '(let ([x 5])
				(let ([y 3])
				  (+ x y)))
			     (primitive-env))]
 [test-case-equal? "let forms have lexical scope"
		   11
		   (evaluate '(let ([x 5]
				    [y 6])
				(let ([x 3])
				  (set! x 10))
				(+ x y))
			     (primitive-env))]
 )
