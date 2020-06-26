(define-syntax run-tests
  (syntax-rules ()
    [(_ t1 ...)
     (let ([passed 0]
	   [failed 0]
	   [start (current-time)])
       (newline)
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

(define ns->s (lambda (ns) (/ ns 1e9)))

(define-syntax test-case
  (syntax-rules ()
    [(_ desc eq-proc expected expr)
     (let ([actual expr])
       (if (eq-proc expected actual)
	   (begin (display (format "- [x] ~s\n" desc))
		  #t)
	   (begin
	     (display (format "- [ ] ~s\n\texpected: ~s\n\tgot:      ~s\n"
			      desc
			      expected
			      actual))
	     #f)))]))

(define-syntax test-case-equal?
  (syntax-rules ()
    [(_ desc expected expr)
     (test-case desc equal? expected expr)]))

(define-syntax test-raises
  (syntax-rules ()
    [(_ who irritants expr)
     (call/cc
      (lambda (k)
	(with-exception-handler
	    (lambda (c)
	      (if (and (equal? (condition-who c) who)
		       (equal? (condition-irritants c) irritants))
		  (begin
		    (display (format "- [x] ~s raises exception with irritants ~s\n"
				     who
				     irritants))
		    (k #t))
		  (begin
		    (display (format "- [ ] unexpected condition!\n\twho: ~s\n\tirr: ~s\n"
				     (condition-who c)
				     (condition-irritants c)))
		    (k #f))))
	  (lambda () expr))))]))
