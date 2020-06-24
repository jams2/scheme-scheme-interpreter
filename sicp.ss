(define the-empty-env '())
(define setup-env
  (lambda () (list `((false . ,#f)
		(true . ,#t)
		(+ . ,+)
		(= . ,=)
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
	  [(pair? expr)
	   (case (car expr)
	     [(case) (evaluate (transform-case expr) env)]
	     [(quote) (cadr expr)]
	     [(define) (define-variable!
			 (definition-var expr)
			 (evaluate (definition-val expr) env)
			 env)]
	     [(set!) (set-variable! (definition-var expr)
				    (evaluate (definition-val expr) env)
				    env)]
	     [(lambda) (make-procedure (lambda-args expr)
				  (lambda-body expr)
				  env)]
	     [(if) (eval-if expr env)]
	     [(begin) (eval-sequence (cdr expr) env)]
	     [(cond) (evaluate (transform-cond expr) env)]
	     [(let) (evaluate (transform-let expr) env)]
	     [(let*) (evaluate (transform-let* expr) env)]
	     [(letrec) (evaluate (transform-letrec expr) env)]
	     [(and) (eval-and (cdr expr) env)]
	     [(or) (eval-or (cdr expr) env)]
	     [else (my-apply (evaluate (rator expr) env)
			     (eval-list (rands expr) env)
			     env)])]
	  [else (error 'evaluate "Invalid expression" expr)])))

(define my-apply
  (lambda (rator rands env)
    (if (compound-procedure? rator)
	(eval-sequence (proc-body rator)
		       (extend-env (proc-args rator)
				   rands
				   (proc-env rator)))
	(scheme-apply rator rands))))

(define true?
  (lambda (expr) (not (eq? expr #f))))

(define false?
  (lambda (expr) (eq? expr #f)))

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
      (if (true? result)
	  (evaluate (if-consequent expr) env)
	  (evaluate (if-alternative expr) env)))))

(define cond-clauses
  (lambda (expr) (cdr expr)))

(define clause-predicate
  (lambda (cond-clause) (car cond-clause)))

(define clause-consequent
  (lambda (cond-clause) (cdr cond-clause)))

(define transform-case
  (lambda (expr)
    (case->let (cadr expr) (cddr expr))))

(define case->let
  (lambda (key clauses)
    `(let ([k ,key])
       ,(case-clauses->if clauses))))

(define case-clauses->if
  (lambda (clauses)
    (cond ((null? clauses) clauses)
	  ((eq? (caar clauses) 'else) (seq->expr (clause-consequent (car clauses))))
	  (else `(if (eqv? k ,@(clause-predicate (car clauses)))
		     ,(seq->expr (clause-consequent (car clauses)))
		     ,(case-clauses->if (cdr clauses)))))))

(define transform-cond
  (lambda (expr)
    (cond->if (cond-clauses expr))))

(define cond->if
  (lambda (clauses)
    (if (null? clauses)
	clauses
	(let ([first-clause (car clauses)]
	      [rest (cdr clauses)])
	  (cond ((else-clause? first-clause) (seq->expr (clause-consequent first-clause)))
		((arrow-form? first-clause) (arrow-form->let
					     (arrow-form-predicate first-clause)
					     (arrow-form-proc first-clause)
					     rest))
		(else `(if ,(clause-predicate first-clause)
			   ,(seq->expr (clause-consequent first-clause))
			   ,(cond->if rest))))))))

(define else-clause?
  (lambda (clause) (eq? (car clause) 'else)))

(define arrow-form?
  (lambda (clause) (eq? (cadr clause) '=>)))

(define arrow-form->let
  (lambda (pred proc rest-clauses)
    `(let ([arrow-result ,pred])
       (if arrow-result
	   (,proc arrow-result)
	   ,(cond->if rest-clauses)))))

(define arrow-form-predicate
  (lambda (expr) (car expr)))

(define arrow-form-proc
  (lambda (expr) (caddr expr)))

(define transform-let
  (lambda (expr)
    (if (named-let? expr)
	(transform-named-let expr)
	(let ([bindings (cadr expr)]
	      [body (cddr expr)])
	  (let->lambda bindings body)))))

(define named-let?
  (lambda (expr) (symbol? (cadr expr))))

(define transform-named-let
  (lambda (expr)
    (named-let->letrec (named-let-name expr)
		       (named-let-bindings expr)
		       (named-let-body expr))))

(define named-let->letrec
  (lambda (name bindings body)
    `((letrec ([,name (lambda ,(map car bindings) ,@body)])
	,name)
      ,@(map cadr bindings))))

(define named-let-name
  (lambda (expr) (cadr expr)))

(define named-let-bindings
  (lambda (expr) (caddr expr)))

(define named-let-body
  (lambda (expr) (cdddr expr)))

(define let->lambda
  (lambda (bindings body)
    `((lambda ,(map car bindings)
	,@body)
      ,@(map cadr bindings))))

(define transform-let*
  (lambda (expr) (let*->nested-lambda (let-bindings expr) (let-body expr))))

(define let-bindings
  (lambda (expr) (cadr expr)))

(define let-body
  (lambda (expr) (cddr expr)))

(define let*->nested-lambda
  (lambda (bindings body)
    (let ((first (car bindings))
	  (rest (cdr bindings)))
      (if (null? rest)
	  (let->lambda (list first) body)
	  (let->lambda (list first)
		       (list (let*->nested-lambda rest body)))))))

(define transform-letrec
  (lambda (expr)
    (letrec->let (let-bindings expr) (let-body expr))))

(define letrec->let
  (lambda (bindings body)
    `(let ,(map (lambda (x) (list (car x) ''*unassigned*)) bindings)
       ,@(map (lambda (x) (list 'set! (car x) (cadr x))) bindings)
       ,@body)))

(define seq->expr
  (lambda (sequence)
    (cond ((null? sequence) sequence)
	  ((null? (cdr sequence)) (car sequence))
	  (else `(begin ,@sequence)))))

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
    (list 'procedure args (internal-definitions->let body) env)))

(define proc-args
  (lambda (p) (cadr p)))

(define proc-body
  (lambda (p) (caddr p)))

(define proc-env
  (lambda (p) (cadddr p)))

(define eval-and
  (lambda (exprs env)
    (if (null? exprs)
	'true
	(let ([val (evaluate (car exprs) env)])
	  (cond ((null? (cdr exprs)) val)
		((true? val) (eval-and (cdr exprs) env))
		(else 'false))))))

(define eval-or
  (lambda (exprs env)
    (if (null? exprs)
	'false
	(let ([val (evaluate (car exprs) env)])
	  (cond ((true? val) val)
		((null? (cdr exprs)) 'false)
		((eval-or (cdr exprs) env)))))))

(define internal-definitions->let
  (lambda (body)
    (let* ([defines (filter (lambda (x)
			      (and (list? x)
				   (eq? (car x) 'define)))
			    body)]
	   [rest (filter (lambda (x) (or (not (list? x))
				    (not (eq? (car x) 'define))))
			 body)])
      (if (null? defines)
	  body
	  `(let ,(map (lambda (x) (list (cadr x) ''*unassigned*)) defines)
	     ,@(map (lambda (x) (list 'set! (cadr x) (caddr x))) defines)
	     ,@rest)))))


;; environment procedures

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
	  (cond ((eq? result '*unnassigned*) (error 'lookup-variable
						    "unassigned variable"
						    var))
		(result (cdr result))
		(else (scan-env (cdr env)))))))
    (define scan-env
      (lambda (env)
	(if (eq? env the-empty-env)
	    (error 'lookup-variable "Unbound variable" var)
	    (scan-frame (car env) env))))
    (scan-env env)))

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
       (display (format "\nRan ~d tests in ~fs:\n\n\t- ~3d PASSED\n\t- ~3d FAILED\n"
			(+ passed failed)
			(- (time-second (current-time))
			   (time-second start))
			passed
			failed)))]))

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

(define first-symbol
  (lambda (expr)
    (if (symbol? expr) expr (first-symbol (car expr)))))

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
 )
