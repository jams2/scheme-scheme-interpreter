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
     (let [(actual expr)]
       (if (eq-proc actual expected)
	   (display (format "- [x] ~s\n" desc))
	   (display (format "- [ ] ~s\n\texpected: ~s\n\tgot:      ~s"
			    desc
			    expected
			    actual))))]))

(run-tests
 [test-case "numbers are self-evaluating"
	    equal?
	    1
	    (evaluate 1 (primitive-env))]
 [test-case "quoted exprs return the expr"
	    equal?
	    'x
	    (evaluate (quote 'x) (primitive-env))]
 [test-case "variables are looked up in the env"
	    equal?
	    5
	    (evaluate 'x (append '(((x . 5))) (primitive-env)))])
