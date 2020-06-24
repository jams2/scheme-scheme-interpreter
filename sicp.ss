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
      (cond (existing (set-cdr! existing val))
	    ((null? (cdr env)) (error 'set-variable! "Unbound variable" var))
	    (else (set-variable! var val (cdr env)))))))

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
