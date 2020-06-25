(include "sicp.ss") ;; env operations etc.
(define primitive-env
  (lambda ()
    (list `((+ . ,+)
	    (- . ,-)
	    (* . ,*)
	    (/ . ,/)
	    (= . ,=)))))

(define evaluate
  (lambda (expr env)
    ((analyze expr) env)))

(define analyze
  (lambda (expr)
    (cond [(self-evaluating? expr) (gen-constant expr)]
	  [(variable-reference? expr) (gen-lookup expr)]
	  [(quoted? expr) (gen-quoted expr)]
	  [(definition? expr) (gen-definition expr)]
	  [(assignment? expr) (gen-assignment expr)]
	  [(begin? expr) (gen-sequence (cdr expr))]
	  [else (error 'analyze "invalid expression" expr)])))

(define self-evaluating?
  (lambda (expr)
    (number? expr)))

(define gen-constant
  (lambda (expr) (lambda (env) expr)))

(define quoted?
  (lambda (expr) (eq? (car expr) 'quote)))

(define gen-quoted
  (lambda (expr)
    (let ([quoted-val (cadr expr)])
      (lambda (env) quoted-val))))

(define variable-reference?
  (lambda (expr) (symbol? expr)))

(define gen-lookup
  (lambda (expr) (lambda (env) (lookup-variable expr env))))

(define definition?
  (lambda (expr) (eq? (car expr) 'define)))

(define gen-definition
  (lambda (expr)
    (let ([var (cadr expr)]
	  [val (analyze (caddr expr))])
      (lambda (env) (define-variable! var (val env) env)))))

(define assignment?
  (lambda (expr)
    (eq? (car expr) 'set!)))

(define gen-assignment
  (lambda (expr)
    (let ([var (cadr expr)]
	  [val (analyze (caddr expr))])
      (lambda (env) (set-variable! var val env)))))

(define begin?
  (lambda (expr) (eq? (car expr) 'begin)))

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
