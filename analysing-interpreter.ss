(define the-empty-env '())
(define primitive-env
  (lambda ()
    '(((+ . ,+)
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
