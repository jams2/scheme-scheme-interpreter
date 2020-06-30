(include "sicp.ss") ;; env operations etc.
(define primitive-env
  (lambda ()
    (list `((true . ,#t)
	    (false . ,#f)
	    (+ . (primitive . ,+))
	    (< . (primitive . ,<))
	    (- . (primitive . ,-))
	    (* . (primitive . ,*))
	    (/ . (primitive . ,/))
	    (= . (primitive . ,=))))))

(define evaluate
  (lambda (expr env)
    ((analyze expr) env)))

(define analyze
  (lambda (expr)
    (cond [(self-evaluating? expr) (gen-constant expr)]
	  [(variable-reference? expr) (gen-lookup expr)]
	  [(match? 'quote expr) (gen-quoted expr)]
	  [(match? 'define expr) (gen-definition expr)]
	  [(match? 'set! expr) (gen-assignment expr)]
	  [(match? 'begin expr) (gen-sequence (cdr expr))]
	  [(match? 'lambda expr) (gen-procedure expr)]
	  [(match? 'if expr) (gen-conditional expr)]
	  [(match? 'let expr) (gen-let expr)]
	  [(application? expr) (gen-application expr)]
	  [else (error 'analyze "invalid expression" expr)])))

(define match?
  (lambda (q expr)
    (eq? (car expr) q)))

(define self-evaluating?
  (lambda (expr)
    (number? expr)))

(define gen-constant
  (lambda (expr) (lambda (env) expr)))

(define variable-reference?
  (lambda (expr) (symbol? expr)))

(define application?
  (lambda (expr) (pair? expr)))

(define gen-quoted
  (lambda (expr)
    (let ([quoted-val (cadr expr)])
      (lambda (env) quoted-val))))

(define gen-lookup
  (lambda (expr) (lambda (env) (lookup-variable expr env))))

(define gen-definition
  (lambda (expr)
    (let ([var (cadr expr)]
	  [val (analyze (caddr expr))])
      (lambda (env) (define-variable! var (val env) env)))))

(define gen-assignment
  (lambda (expr)
    (let ([var (cadr expr)]
	  [val (analyze (caddr expr))])
      (lambda (env) (set-variable! var val env)))))

(define gen-sequence
  (lambda (actions)
    (define sequentially
      (lambda (proc1 proc2)
	(lambda (env)
	  (proc1 env)
	  (proc2 env))))
    (define loop
      (lambda (first-proc rest)
	(if (null? rest)
	    first-proc
	    (loop (sequentially first-proc (car rest))
		  (cdr rest)))))
    (let ([procs (map analyze actions)])
      (if (null? procs)
	  (error 'gen-sequence "empty sequence" actions))
      (loop (car procs) (cdr procs)))))

(define gen-procedure
  (lambda (expr)
    (let ([params (cadr expr)]
	  [body (gen-sequence (cddr expr))])
      (lambda (env) (list 'procedure params body env)))))

(define gen-application
  (lambda (expr)
    (let ([rator-closure (analyze (car expr))]
	  [rand-closures (map analyze (cdr expr))])
      (lambda (env)
	(execute-proc (rator-closure env)
		      (map (lambda (rand-closure) (rand-closure env))
			   rand-closures))))))

(define execute-proc
  (lambda (rator rands)
    (cond [(compound-procedure? rator)
	   ((proc-body rator) (extend-env (proc-params rator)
					  rands
					  (proc-env rator)))]
	  [(primitive-procedure? rator) (scheme-apply (primitive-proc rator) rands)]
	  [else (error 'execute-proc "Invalid procedure operator" rator)])))

(define primitive-procedure?
  (lambda (rator) (eq? (car rator) 'primitive)))

(define primitive-proc (lambda (rator) (cdr rator)))

(define proc-params
  (lambda (proc) (cadr proc)))

(define proc-body
  (lambda (proc) (caddr proc)))

(define proc-env
  (lambda (proc) (cadddr proc)))

(define gen-conditional
  (lambda (expr)
    (let ([p-closure (analyze (if-predicate expr))]
	  [c-closure (analyze (if-consequent expr))]
	  [a-closure (analyze (if-alternative expr))])
      (lambda (env)
	(if (true? (p-closure env))
	    (c-closure env)
	    (a-closure env))))))

(define gen-let
  (lambda (expr)
    (let ([params (map car (let-bindings expr))]
	  [args (map cadr (let-bindings expr))])
      (analyze
       `((lambda ,params
	   ,@(let-body expr))
	 ,@args)))))
