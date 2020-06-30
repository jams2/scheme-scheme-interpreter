(include "sicp.ss")

(define-syntax run-tests
  (syntax-rules ()
    [(_ t1 ...)
     (let ([passed 0]
	   [failed 0]
	   [start (current-time)])
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

(define test-transformer
  (lambda (desc proc expr expected)
    (let ([actual (call/cc
		   (lambda (k)
		     (with-exception-handler (lambda (c)
					       (display (format "- [!] ~s:\n" desc))
					       ((base-exception-handler) c))
		       (lambda () (apply proc (list expr))))))])
      (if (equal? expected actual)
	  (begin
	    (display (format "- [x] ~s\n" desc))
	    #t)
	  (begin
	    (display (format "- [ ] ~s FAILED -\n\texpected ~s\n\tgot      ~s\n"
			     desc
			     expected
			     actual))
	    #f)))))

(define with-initial-env
  (lambda (description expr expected)
    (let ([env (setup-env)])
      (test-eval description expr expected env))))

(define with-empty-env
  (lambda (description expr expected)
    (test-eval description expr expected the-empty-env)))

(define with-frame
  (lambda (frame description expr expected)
    (let ([env (cons frame (setup-env))])
      (test-eval description expr expected env))))

(define with-error
  (lambda (who desc expr)
    (call/cc
     (lambda (k)
       (let ([handler
	      (lambda (c)
		(let ([c-who (condition-who c)])
		  (if (eq? c-who who)
		      (begin (display (format "- [x] ~s\n" desc))
			     (k #t))
		      (begin
			(display (format "- [ ] ~s FAILED -\n\texpected ~s\n\tgot ~s\n"
					 desc
					 who
					 c-who))
			(k #f)))))])
	 (with-exception-handler handler
	   (lambda ()
	     (evaluate expr (setup-env))
	     (display (format "- [ ] ~s FAILED - exception not raised\n"
			      desc))
	     #f)))))))

(define test-eval
  (lambda (description expr expected env)
    (let ([actual (call/cc
		   (lambda (k)
		     (with-exception-handler (lambda (c)
					       (display (format "- [!] ~s:\n" description))
					       ((base-exception-handler) c))
		       (lambda () (evaluate expr env)))))])
      (if (not (equal? expected actual))
	  (begin
	    (display (format "- [ ] ~s FAILED -\n\texpected ~s\n\tgot ~s\n"
			     description
			     expected
			     actual))
	    #f)
	  (begin
	    (display (format "- [x] ~s\n" description))
	    #t)))))

(define test-env
  (lambda (description expr expected env-proc)
    (let ([env (setup-env)])
      (evaluate expr env)
      (let ([env-result (env-proc env)])
	(if (equal? expected env-result)
	    (begin
	      (display (format "- [x] ~s\n" description))
	      #t)
	    (begin
	      (display (format "- [ ] ~s FAILED -\n\texpected ~s\n\tgot ~s\n"
			       description
			       expected
			       env-result))
	      #f))))))

(run-tests
 (with-initial-env "numbers are self-evaluating" '1 1)
 (with-initial-env "strings are self-evaluating" "hello world" "hello world")
 (with-frame '((x . 5)) "variables return their bound value" 'x 5)
 (with-error 'lookup-variable "evaluating an unbound variable causes an exception" 'y)
 (test-env "'define statements create a binding of var to val in the current scope"
	   '(define x 5)
	   5
	   (lambda (env) (cdaar env)))
 (with-empty-env "evaluating a lambda returns a 'procedure-tagged list"
		 '(lambda (x) (+ x y) (display y))
		 '(procedure (x) ((+ x y) (display y)) ()))
 (test-env "'define with a lambda evaluates the lambda and binds it to the variable"
	   '(define proc (lambda (x) (+ x y) (display y)))
	   'procedure
	   (lambda (env) (cadaar env)))
 (test-env "variable definitions evaluate their values before binding"
	   '(define x (+ 5 11))
	   '(x . 16)
	   (lambda (env) (caar env)))
 (test-env "variable definitions evaluate their values before binding, left left lambda"
	   '(define x ((lambda (x y) (+ x y)) 5 11))
	   '(x . 16)
	   (lambda (env) (caar env)))
 (test-env "'define updates an existing binding if exists"
	   '(begin (define x 5) (define x 6))
	   #t
	   (lambda (env)
	     (let ([first-frame (car env)])
	       (and (equal? (car first-frame) '(x . 6))
		    (not (equal? (car first-frame) (cadr first-frame)))))))
 (with-initial-env "primitive procedures are evaluated scheme" '(+ 1 2) 3)
 (with-initial-env "unary primitives are evaluated by scheme" '(- 2) -2)
 (with-initial-env "left left lambdas are immediately evaluated and applied"
		   '((lambda (x) x) 11)
		   11)
 (with-frame '((x . (procedure (x) (x) ,(setup-env))))
	     "defined procedures are applied to their operands"
	     '(x 11)
	     11)
 (with-empty-env "quoted expressions return the expression" ''x 'x)
 (with-empty-env "quoted lists return the list" ''(x y z) '(x y z))
 (with-initial-env "'begin expressions eval all subexpressions, returning the last result"
		   '(begin (+ 1 2) (+ 3 4) (+ 5 6))
		   11)
 (test-env "'set! expressions mutate an existing variable in current scope"
	   '(begin (define x 5) (set! x 11))
	   #t
	   (lambda (env)
	     (let ([first-frame (car env)])
	       (and (eq? (cdar first-frame) 11)
		    (not (equal? (car first-frame) (cadr first-frame)))))))
 (with-error 'set-variable!
	     "'set! on an undefined var throws an error"
	     '(set! x 5))
 (with-initial-env "'if forms evaluate their consequent if the predicate passes"
		   '(if (+ 1 2) true false)
		   #t)
 (with-initial-env "'if forms return false if the predicate fails and there is no alt"
		   '(if false true)
		   #f)
 (with-initial-env "'let forms are evaluated"
		   '(let ((x 5) (y 6)) (+ x x) (+ x y))
		   11)
 (with-frame '((x . 5))
	     "'if forms do not evaluate their consequent if the predicate fails"
	     '(if false (set! x 6) x)
	     5)
 (test-transformer "case forms are transformed to let"
		   transform-case
		   '(case (x)
		      ((y) (+ 1 2))
		      ((x) (+ 3 2) (- 1 4))
		      (else x))
		   '(let ([k (x)])
		      (if (eqv? k y)
			  (+ 1 2)
			  (if (eqv? k x) (begin (+ 3 2) (- 1 4)) x))))
 (test-transformer "let forms are transformed to left left lambdas"
		   transform-let
		   '(let ((x 5) (y 6)) (+ x y) (* x x))
		   '((lambda (x y) (+ x y) (* x x)) 5 6))
 (test-transformer "cond forms are transformed to series of if forms"
		   transform-cond
		   '(cond ((null? x) '())
			  ((eq? x 'y) x)
			  (else y))
		   '(if (null? x)
			'()
			(if (eq? x 'y) x y)))
 (test-transformer "let* forms are transformed to nested l l lambdas"
		   transform-let*
		   '(let* ((x 3)
			   (y (+ x 2))
			   (z (+ x y 5)))
		      (* x z))
		   '((lambda (x)
		       ((lambda (y)
			  ((lambda (z)
			     (* x z))
			   (+ x y 5)))
			(+ x 2)))
		     3))
 (with-initial-env "'let* forms are evaluated"
		   '(let* ((x 3)
			   (y (+ x 2))
			   (z (+ x y 5)))
		      (* x z))
		   39)
 (with-initial-env "named let forms are evaluated"
		   '(let fact ((n 5) (total 1))
		      (if (= 0 n)
			  total
			  (fact (- n 1) (* total n))))
		   120)
 (with-initial-env "letrec handles mutually recursive procedures in its bindings"
		   '(letrec ([even? (lambda (n)
				      (if (= 0 n)
					  true
					  (odd? (- n 1))))]
			     [odd? (lambda (n)
				     (if (= 0 n)
					 false
					 (even? (- n 1))))])
		      (even? 5))
		   #f)
 (test-transformer "letrec forms transform to let with simultaneous scope"
		   transform-letrec
		   '(letrec ([even? (lambda (n)
				      (if (= 0 n)
					  true
					  (odd? (- n 1))))]
			     [odd? (lambda (n)
				     (if (= 0 n)
					 true
					 (even? (- n 1))))])
		      (even? 5))
		   '(let ([even? '*unassigned*]
			  [odd? '*unassigned*])
		      (set! even? (lambda (n)
				    (if (= 0 n)
					true
					(odd? (- n 1)))))
		      (set! odd? (lambda (n)
				   (if (= 0 n)
				       true
				       (even? (- n 1)))))
		      (even? 5)))
 (test-transformer "named-let forms are transformed to letrec"
		   transform-named-let
		   '(let fact ((n 5) (total 1))
		      (if (= 0 n)
			  1
			  (* n (fact (- n 1)))))
		   '((letrec ([fact (lambda (n total)
				      (if (= 0 n)
					  1
					  (* n (fact (- n 1)))))])
		       fact)
		     5
		     1))
 (with-initial-env "cond clauses with arrow forms are evaluated"
		   '(cond ((= (+ 1 1) 1) 1)
			  ((+ 1 1) => -)
			  (else 2))
		   -2)
 (with-initial-env "and forms short-circuit"
		   '(begin
		      (define x 0)
		      (and (set! x 1) (set! x 2) false (set! x 3))
		      x)
		   2)
 (with-initial-env "and forms return true if no clauses"
		   '(and)
		   'true)
 (with-initial-env "and forms return the value of the last expression if all true"
		   '(and 1 2 true 3 'final)
		   'final)
 (with-initial-env "or forms return the first value that evaluates to true"
		   '(or false false 3)
		   3)
 (with-initial-env "or forms return false if no clauses"
		   '(or)
		   'false)
 (with-initial-env "or forms return false if all expressions evaluate to false"
		   '(or false false false)
		   'false)
 (test-transformer "internal definitions are hoisted"
		   internal-definitions->let
		   '((define even? (lambda (n)
				     (if (= 0 n)
					 true
					 (odd? (- n 1)))))
		     (define odd? (lambda (n)
				    (if (= 0 n)
					false
					(even? (- n 1)))))
		     (even? 5))
		   '(let ([even? '*unassigned*]
			  [odd? '*unassigned*])
		      (set! even? (lambda (n)
				    (if (= 0 n)
					true
					(odd? (- n 1)))))
		      (set! odd? (lambda (n)
				   (if (= 0 n)
				       false
				       (even? (- n 1)))))
		      (even? 5)))
 (with-initial-env "nested lets with assignments do not violate lexical scope"
		   '(let ([y 5])
		      (let ([x (* 5 y)])
			(set! y 6)
			x))
		   25)
 (with-initial-env "combinator fib is evaluated"
		   '((lambda (n)
		       ((lambda (fib)
			  (fib fib n))
			(lambda (f n)
			  (cond ((= n 1) 1)
				((= n 0) 0)
				(else (+ (f f (- n 1))
					 (f f (- n 2))))))))
		     6)
		   8)
 (with-initial-env "combinator mutually recursive even? is evaluated"
		   '((lambda (x)
		      ((lambda (even? odd?)
			 (even? even? odd? x))
		       (lambda (ev? od? n)
			 (if (= n 0) true (od? ev? od? (- n 1))))
		       (lambda (ev? od? n)
			 (if (= n 0) false (ev? ev? od? (- n 1))))))
		     5)
		     #f)
 )
