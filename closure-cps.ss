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
	  [val (analyze (caddr expr))])
      (lambda (env k)
	(val env (lambda (val)
		   (k (define-variable! var val env))))))))

(define gen-assignment
  (lambda (expr)
    (let ([var (cadr expr)]
	  [val (analyze (caddr expr))])
      (lambda (env k)
	(val env (lambda (val)
		   (k (set-variable! var val env))))))))

(define define-variable!
  (lambda (var val env)
    (let* ([first-frame (car env)]
	   [existing (assoc var first-frame)])
      (if existing
	  (set-cdr! existing val)
	  (set-car! env (cons `(,var . ,val) first-frame))))))

(define gen-sequence
  (lambda (actions)
    (define sequentially
      (lambda (proc1 proc2)
	(lambda (env k)
	  (proc1 env (lambda (val)
		       (proc2 env k))))))
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

;; a sequence of the form (begin (+ 1 2) (+ 3 4)) can be transformed to nested application of lambdas, as we only care to return the result of the final computation.
;; (begin (+ 1 2) (+ 3 4)) \equiv ((lambda (x) (+ 3 4)) (+ 1 2))
;; (begin (+ 1 2) (+ 3 4) (+ 5 6)) ((lambda (x) ((lambda (x) (+ 5 6)) (+ 3 4))) (+ 1 2))
;; see Reynolds

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
