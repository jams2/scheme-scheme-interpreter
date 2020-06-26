(include "analysing-interpreter.ss")

(define-syntax run-tests
  (syntax-rules ()
    [(_ t1 ...)
     (let ([passed 0]
	   [failed 0]
	   [start (current-time)])
       (newline)
       (if t1
	   (set! passed (+ 1 passed))
	   (set! failed (+ 1 failed)))
       ...
       (display (format "\nRan ~d tests in ~7fs:\n\n\t- ~3d PASSED\n\t- ~3d FAILED\n"
			(+ passed failed)
			(ns->s (- (time-nanosecond (current-time))
				  (time-nanosecond start)))
			passed
			failed)))]))

(define ns->s
  (lambda (ns) (/ ns 1e9)))

(define-syntax test-case
  (syntax-rules ()
    [(_ desc eq-proc expected expr)
     (let ([actual expr])
       (if (eq-proc expected actual)
	   (begin (display (format "- [x] ~s\n" desc))
		  #t)
	   (begin
	     (display (format "- [ ] ~s\n\texpected: ~s\n\tgot:      ~s\n"
			      desc
			      expected
			      actual))
	     #f)))]))

(define-syntax test-raises
  (syntax-rules ()
    [(_ who irritants expr)
     (call/cc
      (lambda (k)
	(with-exception-handler
	    (lambda (c)
	      (if (and (equal? (condition-who c) who)
		       (equal? (condition-irritants c) irritants))
		  (begin
		    (display (format "- [x] ~s raises exception with irritants ~s\n"
				     who
				     irritants))
		    (k #t))
		  (begin
		    (display (format "- [ ] unexpected condition!\n\twho: ~s\n\tirr: ~s\n"
				     (condition-who c)
				     (condition-irritants c)))
		    (k #f))))
	  (lambda () expr))))]))

(run-tests
 [test-case "numbers are self-evaluating"
	    equal?
	    1
	    (evaluate 1 (primitive-env))]
 [test-case "floating point numbers are self-evaluating"
	    equal?
	    3.145
	    (evaluate 3.145 (primitive-env))]
 [test-case "numbers with exponents are self-evaluating"
	    equal?
	    3e9
	    (evaluate 3e9 (primitive-env))]
 [test-case "quoted exprs return the expr"
	    equal?
	    'x
	    (evaluate (quote 'x) (primitive-env))]
 [test-case "variables are looked up in the env"
	    equal?
	    5
	    (evaluate 'x (append '(((x . 5))) (primitive-env)))]
 [test-raises 'set-variable!
	      '(x)
	      (evaluate '(set! x 5) (primitive-env))]
 [test-case "begin forms are evaluated sequentially"
	    equal?
	    5
	    (evaluate '(begin (define x 5) (define y 6) x) (primitive-env))]
 [test-raises 'gen-sequence
	      '(())
	      (evaluate '(begin) (primitive-env))]
 [test-case "lambda expressions evaluate to a 'procedure tagged list"
	    (lambda (expected actual) (eq? (car actual) expected))
	    'procedure
	    (evaluate '(lambda (x) x) (primitive-env))]
 [test-case "left left identity function is evaluated"
	    equal?
	    5
	    (evaluate '((lambda (x) x) 5) (primitive-env))]
 [test-case "primitive + is applied"
	    equal?
	    7
	    (evaluate '(+ 3 4) (primitive-env))]
 [test-case "primitive / is applied"
	    equal?
	    (/ 3 7)
	    (evaluate '(/ 3 7) (primitive-env))]
 [test-case "successful if predicate evaluates only the consequent"
	    equal?
	    6
	    (evaluate '(begin (define x 6)
			      (if true
				  x
				  (begin (set! x 7)
					 x)))
		      (primitive-env))]
 [test-case "failed if predicate evaluates only the alternative"
	    equal?
	    6
	    (evaluate '(begin (define x 6)
			      (if false
				  (begin (set! x 7) x)
				  x))
		      (primitive-env))]
 [test-case "if forms return false if the predicate fails and their is no alt"
	    equal?
	    #f
	    (evaluate '(if false true) (primitive-env))]
 )
