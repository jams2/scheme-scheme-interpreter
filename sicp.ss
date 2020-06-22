(define the-empty-env '())
(define setup-env
  (lambda () (list `((false . ,#f)
		(true . ,#t)
		(+ . ,+)
		(- . ,-)
		(* . ,*)
		(/ . ,/)))))
(define global-env (setup-env))
(define scheme-apply apply)

(define evaluate
  (lambda (expr env)
    (cond [(number? expr) expr]
	  [(string? expr) expr]
	  [(symbol? expr) (lookup-variable expr env)]
	  [(expr-match? 'quote expr) (cadr expr)]
	  [(expr-match? 'define expr) (define-variable!
					(definition-var expr)
					(evaluate (definition-val expr) env)
					env)]
	  [(expr-match? 'set! expr) (set-variable!
				     (definition-var expr)
				     (evaluate (definition-val expr) env)
				     env)]
	  [(expr-match? 'lambda expr) (make-procedure (lambda-args expr)
						 (lambda-body expr)
						 env)]
	  [(expr-match? 'if expr) (eval-if expr env)]
	  [(expr-match? 'begin expr) (eval-sequence (cdr expr) env)]
	  [(expr-match? 'cond expr) (evaluate (cond->if (cond-clauses expr))
					      env)]
	  [(application? expr)
	   (my-apply (evaluate (rator expr) env)
		     (eval-list (rands expr) env)
		     env)]
	  [else (error 'evaluate "Invalid expression" expr)])))

(define my-apply
  (lambda (rator rands env)
    (if (compound-procedure? rator)
	(eval-sequence (proc-body rator)
		  (extend-env (proc-args rator)
			      rands
			      (proc-env rator)))
	(scheme-apply rator rands))))

(define expr-match?
  (lambda (sym expr)
    (eq? sym (car expr))))

(define eval-list
  (lambda (l env)
    (if (null? l)
	'()
	(cons (evaluate (car l) env)
	      (eval-list (cdr l) env)))))

(define eval-sequence
  (lambda (l env)
    (cond ((null? l) '())
	  ((null? (cdr l)) (evaluate (car l) env))
	  (else (evaluate (car l) env)
		(eval-sequence (cdr l) env)))))

(define eval-if
  (lambda (expr env)
    (let ([result (evaluate (if-predicate expr) env)])
      (if result
	  (evaluate (if-consequent expr) env)
	  (evaluate (if-alternative expr) env)))))

(define cond-clauses
  (lambda (expr) (cdr expr)))

(define clause-predicate
  (lambda (cond-clause) (car cond-clause)))

(define clause-consequent
  (lambda (cond-clause) (cdr cond-clause)))

(define cond->if
  (lambda (clauses)
    (cond ((null? clauses) 'false)
	  ((eq? (caar clauses) 'else) `(begin ,@(clause-consequent (car clauses))))
	  (else `(if ,(clause-predicate (car clauses))
		     (begin ,@(clause-consequent (car clauses)))
		     ,(cond->if (cdr clauses)))))))

(define if-predicate
  (lambda (expr) (cadr expr)))

(define if-consequent
  (lambda (expr) (caddr expr)))

(define if-alternative
  (lambda (expr)
    (if (null? (cdddr expr))
	'false
	(cadddr expr))))

(define rator
  (lambda (expr) (car expr)))

(define rands
  (lambda (expr) (cdr expr)))

(define application?
  (lambda (expr) (pair? expr)))

(define compound-procedure?
  (lambda (expr)
    (and (list? expr)
	 (eq? (car expr) 'procedure))))

(define lambda-args
  (lambda (expr) (cadr expr)))

(define lambda-body
  (lambda (expr) (cddr expr)))

(define make-procedure
  (lambda (args body env)
    (list 'procedure args body env)))

(define proc-args
  (lambda (p) (cadr p)))

(define proc-body
  (lambda (p) (caddr p)))

(define proc-env
  (lambda (p) (cadddr p)))

(define define-variable!
  (lambda (var val env)
    (let* ([first-frame (car env)]
	   [existing (assoc var first-frame)])
      (if existing
	  (set-cdr! existing val)
	  (set-car! env (cons `(,var . ,val) first-frame))))))

(define set-variable!
  (lambda (var val env)
    (let* ([first-frame (car env)]
	   [existing (assoc var first-frame)])
      (if existing
	  (set-cdr! existing val)
	  (error 'set-variable! "Unbound variable" var)))))

(define definition-var
  (lambda (expr) (cadr expr)))

(define definition-val
  (lambda (expr) (caddr expr)))

(define extend-env
  (lambda (vars vals env)
    (if (not (eq? (length vars) (length vals)))
	(error 'extend-env "Mismatched vars<->vals" vars vals)
	(cons (cons-frame vars vals) env))))

(define cons-frame
  (lambda (vars vals)
    (if (null? vars)
	'()
	(cons (cons (car vars) (car vals))
	      (cons-frame (cdr vars) (cdr vals))))))

(define lookup-variable
  (lambda (var env)
    (define scan-frame
      (lambda (frame env)
	(let ([result (assoc var frame)])
	  (if result
	      (cdr result)
	      (scan-env (cdr env))))))
    (define scan-env
      (lambda (env)
	(if (eq? env the-empty-env)
	    (error 'lookup-variable "Unbound variable" var)
	    (scan-frame (car env) env))))
    (scan-env env)))


((lambda ()
   (define test-transformer
     (lambda (proc expr expected)
       (let ([actual (apply proc (list expr))])
	 (if (equal? expected actual)
	     (display (format "- [x] ~s\n" proc))
	     (display (format "- [ ] ~s FAILED - expected <~s> got <~s>\n"
			      proc
			      expected
			      actual))))))

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
				(k #f))
			 (begin
			   (display (format "- [ ] ~s FAILED - expected <~s> got <~s>\n"
					    desc
					    who
					    c-who))
			   (k #f)))))])
	    (with-exception-handler handler
	      (lambda ()
		(evaluate expr (setup-env))
		(display (format "- [ ] ~s FAILED - exception not raised\n"
				 desc)))))))))

   (define test-eval
     (lambda (description expr expected env)
       (let ([actual (evaluate expr env)])
	 (if (not (equal? expected actual))
	     (display (format "- [ ] ~s FAILED - expected <~s> got <~s>\n"
			      description
			      expected
			      actual))
	     (display (format "- [x] ~s\n" description))))))

   (define test-env
     (lambda (description expr expected env-proc)
       (let ([env (setup-env)])
	 (evaluate expr env)
	 (let ([env-result (env-proc env)])
	   (if (equal? expected env-result)
	       (display (format "- [x] ~s\n" description))
	       (display (format "- [ ] ~s FAILED - expected <~s> got <~s>\n"
				description
				expected
				env-result)))))))

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

   (with-initial-env "'if statements evaluate their consequent if the predicate passes"
		     '(if (+ 1 2) true false)
		     #t)

   (with-initial-env "'if statements return false if the predicate fails and there is no alt"
		     '(if false true)
		     #f)

   (with-frame '((x . 5))
	       "'if statements do not evaluate their consequent if the predicate fails"
	       '(if false (set! x 6) x)
	       5)

   (test-transformer
    cond->if
    '(((null? x) '())
      ((eq? x 'y) x)
      (else y))
    '(if (null? x)
	 (begin '())
	 (if (eq? x 'y)
	     (begin x)
	     (begin y))))
   ))
