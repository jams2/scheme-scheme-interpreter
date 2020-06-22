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
	  [(expr-match? 'define expr) (define-variable! (definition-var expr)
					(evaluate (definition-val expr) env)
					env)]
	  [(expr-match? 'lambda expr) (make-procedure (lambda-args expr)
						 (lambda-body expr)
						 env)]
	  [(expr-match? 'if expr) (eval-if expr env)]
	  [(application? expr)
	   (my-apply (evaluate (rator expr) env)
		     (eval-list (rands expr) env)
		     env)])))

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
    (let ([first-frame (car env)])
      (set-car! env (cons `(,var . ,val) first-frame)))))

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
		      (begin (display (format "[] ~s PASSED\n" desc))
			     (k #f))
		      (begin
			(display (format "[x] ~s FAILED - expected <~s> got <~s>\n"
					 desc
					 who
					 c-who))
			(k #f)))))])
	 (with-exception-handler handler
	   (lambda ()
	     (evaluate expr (setup-env))
	     (display (format "[x] ~s FAILED - exception not raised\n"
			      desc)))))))))

(define test-eval
  (lambda (description expr expected env)
    (let ([actual (evaluate expr env)])
      (if (not (equal? expected actual))
	  (display (format "[x] ~s FAILED - expected <~s> got <~s>\n"
			   description
			   expected
			   actual))
	  (display (format "[] ~s PASSED\n" description))))))

(define test-env
  (lambda (description expr expected env-proc)
    (let ([env (setup-env)])
      (evaluate expr env)
      (let ([env-result (env-proc env)])
	(if (equal? expected env-result)
	    (display (format "[] ~s PASSED\n" description))
	    (display (format "[x] ~s FAILED - expected <~s> got <~s>\n"
			     description
			     expected
			     env-result)))))))

(with-initial-env "numbers 1" '1 1)
(with-initial-env "string 1" "hello world" "hello world")
(with-frame '((x . 5)) "var lookup 1" 'x 5)
(with-error 'lookup-variable "unbound variable 1" 'y)
(test-env "define 1" '(define x 5) 5 (lambda (env) (cdaar env)))
(with-empty-env "make procedure 1"
		'(lambda (x) (+ x y) (display y))
		'(procedure (x) ((+ x y) (display y)) ()))
(test-env "define procedure"
	  '(define proc (lambda (x) (+ x y) (display y)))
	  'procedure
	  (lambda (env) (cadaar env)))
(test-env "assign application result"
	  '(define x (+ 5 11))
	  '(x . 16)
	  (lambda (env) (caar env)))
(test-env "assign left left lambda result"
	  '(define x ((lambda (x y) (+ x y)) 5 11))
	  '(x . 16)
	  (lambda (env) (caar env)))
(with-initial-env "primitive application: multiple operands" '(+ 1 2) 3)
(with-initial-env "primitive application: single operand" '(- 2) -2)
(with-initial-env "left left lambda: identity" '((lambda (x) x) 11) 11)
(with-frame '((x . (procedure (x) (x) ,(setup-env))))
	    "defined process application 1"
	    '(x 11)
	    11)
(with-empty-env "quoted expression 1" ''x 'x)
(with-empty-env "quoted expression: list" ''(x y z) '(x y z))
(with-initial-env "if statement 1" '(if (+ 1 2) true false) 'true)
(with-initial-env "if statement: no alternative" '(if false true) 'false)
