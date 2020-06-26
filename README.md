
# Table of Contents

1.  [Scheme Interpreters](#org2e0777e)
    1.  [Points of interest](#org8ad86a7)
        1.  [Comparative execution time of Interpreter I and the Closure Generating Compiler](#org8c0b7bf)
        2.  [Evaluation order in the case of syntax transformations (Interpreter I)](#org8d9e331)
    2.  [Tests](#org07fcdd5)
        1.  [Closure Generating Interpreter](#org644b964)
        2.  [Interpreter I](#orgbbbcf03)


<a id="org2e0777e"></a>

# Scheme Interpreters

This repo contains a number of toy Scheme interpreters, written in Chez Scheme. It is an exercise of ideas from chapter 4 of the Structure and Interpretation of Computer Programs [(the wizard book)](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html), and [William Byrd](https://www.youtube.com/channel/UCSC9kYeTee012BRsYw-y12Q)'s hangout series on youtube. There's also an example in [The Scheme Programming Language](https://www.scheme.com/tspl4/examples.html#./examples:h7), by Kent Dybvig.

See [Feeley and Lapalme '87](http://www.iro.umontreal.ca/~feeley/papers/FeeleyLapalmeCL87.pdf) for more on the closure generating interpreter ([4.1.7](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1.7) in SICP).


<a id="org8ad86a7"></a>

## Points of interest


<a id="org8c0b7bf"></a>

### Comparative execution time of Interpreter I and the Closure Generating Compiler

Evaluating the following fibonacci computation in both the first interpreter and the closure generating interpreter shows that the closure generating interpreter is indeed (by a rough measure) quicker than its counterpart; however it makes use of more memory.

    (time (evaluate '((lambda (i)
    		    ((lambda (y) ((y y) i))
    		     (lambda (f)
    		       (lambda (n)
    			 (if (< n 2) n
    			     (+ ((f f) (- n 1))
    				((f f) (- n 2))))))))
    		  30)
    		(setup-env)))

With closures:

> 110 collections
> 
> 0.961665920s elapsed cpu time, including 0.003976291s collecting
> 
> 0.962282046s elapsed real time, including 0.004115199s collecting
> 
> 926305488 bytes allocated, including 930052240 bytes reclaimed

Without:

> 87 collections
> 
> 1.130188863s elapsed cpu time, including 0.001864684s collecting
> 
> 1.130928856s elapsed real time, including 0.002018562s collecting
> 
> 732426288 bytes allocated, including 735672288 bytes reclaimed


<a id="org8d9e331"></a>

### Evaluation order in the case of syntax transformations (Interpreter I)

The interpreter, like Scheme, follows applicative order evaluation. All expressions are evaluated before being passed into procedures. Take, for example, the simple example:

    (define mult (lambda (x y) (* x y)))
    (mult (+ 1 2) (* 3 4))

Left to right order is unspecified, but the expressions `(+ 1 2)` and `(* 3 4)` should be evaluated before being passed into the `mult` procedure:

    (mult (+ 1 2) (* 3 4))
    (mult 3 12)
    (* 3 12)
    36

What about the case of syntax transformations? For example, we transform `let` forms into immediately applied anonymous procedures, as such:

    (let ([x 5]) (* x x))
    >>> ((lambda (x) (* x x)) 5)

If our let binding value is itself a combination, should it be evaluated at the time of transformation, like so;

    (let ([x (+ 2 2)]) (* x x))
    >>> ((lambda (x) (* x x)) 4)

or left "as is", to be evaluated when the left-left-lambda is evaluated?

    (let ([x (+ 2 2)]) (* x x))
    >>> ((lambda (x) (* x x)) (+ 2 2))

There is a pleasing separation of concern to performing only syntactic transformation in the transformers, but are there cases where subsequent modifications to the environment will cause unexpected behaviour? Examine an example where the value of the let binding references an object in the enclosing scope:

    (let ([y 5])
        (let ([x (* 5 y)])
    	(set! y 6)
    	x))
    
    >>> ((lambda (y)
    	     ((lambda (x)
    		(set! y 6)
    		x)
    	    (* 5 y)))
        5)

5 is associated with y in the frame constructed by evaluating the outer lambda;

    ((lambda (x)
        (set! y 6)
        x)
     (* 5 5))

Then `(* 5 5)` is evaluated to 25 and associated with x in the inner frame. Thus, not evaluating the values of let bindings at the time of transformation does not produce behaviour that would violate lexical scope, and is consistent with the programmer's expectations. Amusingly, examining this unearthed a bug in the `set-variable!` procedure, in which only the current frame was examined when looking for the variable to mutate. (It is worth considering whether `set!` should operate only in the local scope or traverse frames until a matching variable name is found. Presently I think the latter.)


<a id="org07fcdd5"></a>

## Tests


<a id="org644b964"></a>

### Closure Generating Interpreter

-   "numbers are self-evaluating"
-   "floating point numbers are self-evaluating"
-   "numbers with exponents are self-evaluating"
-   "quoted exprs return the expr"
-   "variables are looked up in the env"
-   set-variable! raises exception with irritants (x)
-   "begin forms are evaluated sequentially"
-   gen-sequence raises exception with irritants (())
-   "lambda expressions evaluate to a 'procedure tagged list"
-   "left left identity function is evaluated"
-   "primitive + is applied"
-   "primitive / is applied"

Ran 12 tests in .000092s:

-   12 PASSED
-   0 FAILED


<a id="orgbbbcf03"></a>

### Interpreter I

-   "numbers are self-evaluating"
-   "strings are self-evaluating"
-   "variables return their bound value"
-   "evaluating an unbound variable causes an exception"
-   "'define statements create a binding of var to val in the current scope"
-   "evaluating a lambda returns a 'procedure-tagged list"
-   "'define with a lambda evaluates the lambda and binds it to the variable"
-   "variable definitions evaluate their values before binding"
-   "variable definitions evaluate their values before binding, left left lambda"
-   "'define updates an existing binding if exists"
-   "primitive procedures are evaluated scheme"
-   "unary primitives are evaluated by scheme"
-   "left left lambdas are immediately evaluated and applied"
-   "defined procedures are applied to their operands"
-   "quoted expressions return the expression"
-   "quoted lists return the list"
-   "'begin expressions eval all subexpressions, returning the last result"
-   "'set! expressions mutate an existing variable in current scope"
-   "'set! on an undefined var throws an error"
-   "'if forms evaluate their consequent if the predicate passes"
-   "'if forms return false if the predicate fails and there is no alt"
-   "'let forms are evaluated"
-   "'if forms do not evaluate their consequent if the predicate fails"
-   "case forms are transformed to let"
-   "let forms are transformed to left left lambdas"
-   "cond forms are transformed to series of if forms"
-   "let\* forms are transformed to nested l l lambdas"
-   "'let\* forms are evaluated"
-   "named let forms are evaluated"
-   "letrec handles mutually recursive procedures in its bindings"
-   "letrec forms transform to let with simultaneous scope"
-   "named-let forms are transformed to letrec"
-   "cond clauses with arrow forms are evaluated"
-   "and forms short-circuit"
-   "and forms return true if no clauses"
-   "and forms return the value of the last expression if all true"
-   "or forms return the first value that evaluates to true"
-   "or forms return false if no clauses"
-   "or forms return false if all expressions evaluate to false"
-   "internal definitions are hoisted"
-   "nested lets with assignments do not violate lexical scope"
-   "combinator fib is evaluated"
-   "combinator mutually recursive even? is evaluated"

Ran 43 tests in .000234s:

-   43 PASSED
-   0 FAILED

