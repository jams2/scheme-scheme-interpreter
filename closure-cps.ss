(include "analysing-interpreter.ss")

(define evaluate
  (lambda (expr env)
    ((analyze expr) env (end-cont))))

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

(define end-cont
  (lambda ()
    (lambda (val) val)))

(define gen-constant
  (lambda (expr)
    (lambda (env k) (k expr))))

(define gen-lookup
  (lambda (expr)
    (lambda (env k)
      (k (lookup-variable expr env)))))

(define gen-quoted
  (lambda (expr)
    (let ([quoted-val (cadr expr)])
      (lambda (env k) (k quoted-val)))))

(define gen-definition
  (lambda (expr)
    (let ([var (cadr expr)]
	  [val-closure (analyze (caddr expr))])
      (lambda (env k)
	(val-closure env
		     (lambda (val)
		       (k (define-variable! var val env))))))))

(define gen-assignment
  (lambda (expr)
    (let ([var (cadr expr)]
	  [val (analyze (caddr expr))])
      (lambda (env k)
	(val env
	     (lambda (val)
	       (k (set-variable! var val env))))))))

(define gen-sequence
  (lambda (actions)
    (define nest-closures
      (lambda (c1 c2)
	(lambda (env k)
	  (c1 env (lambda (val)
		    (c2 env k))))))
    (define loop-closures
      (lambda (first-closure rest)
	(if (null? rest)
	    first-closure
	    (loop-closures (nest-closures first-closure (car rest))
			   (cdr rest)))))
    (let ([closures (map analyze actions)])
      (if (null? closures)
	  (error 'gen-sequence "empty sequence" actions))
      (loop-closures (car closures) (cdr closures)))))

(define gen-procedure
  (lambda (expr)
    (let ([params (cadr expr)]
	  [body (gen-sequence (cddr expr))])
      (lambda (env k) (k (list 'procedure params body env))))))

(define gen-conditional
  (lambda (expr)
    (let ([p-closure (analyze (if-predicate expr))]
	  [c-closure (analyze (if-consequent expr))]
	  [a-closure (analyze (if-alternative expr))])
      (lambda (env k)
	(p-closure env (lambda (val)
			 (if (true? val)
			     (c-closure env k)
			     (a-closure env k))))))))

(define gen-application
  (lambda (expr)
    (let ([rator-closure (analyze (car expr))]
	  [rand-closures (map analyze (cdr expr))])
      (lambda (env k)
	(rator-closure env
		       (rator-cont env rand-closures k))))))

(define rator-cont
  (lambda (env rand-closures k)
    (lambda (rator)
      (eval-closure-list rand-closures
			 env
			 (lambda (rands)
			   (execute-proc rator
					 rands
					 k))))))

(define eval-closure-list
  (lambda (l env k)
    (if (null? l)
	(k l)
	(let ([closure (car l)])
	  (closure env
		   (lambda (val)
		     (eval-closure-list (cdr l)
					env
					(lambda (next) (k (cons val next))))))))))

(define execute-proc
  (lambda (rator rands k)
    (cond [(compound-procedure? rator)
	   ((proc-body rator) (extend-env (proc-params rator)
					  rands
					  (proc-env rator))
	    k)]
	  [(primitive-procedure? rator) (k (scheme-apply (primitive-proc rator) rands))]
	  [else (error 'execute-proc "Invalid procedure operator" rator)])))
