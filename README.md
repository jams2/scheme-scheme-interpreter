# Scheme Interpreter

This is a Scheme interpreter, written in Chez Scheme. It is an exercise of ideas from chapter 4 of the Structure and Interpretation of Computer Programs ([wizard book](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html)), and [William Byrd](https://www.youtube.com/channel/UCSC9kYeTee012BRsYw-y12Q)'s hangout series on youtube. There's also an example in [The Scheme Programming Language](https://www.scheme.com/tspl4/examples.html#./examples:h7), by Kent Dybvig.

The idea is to implement an environment passing interpreter, with lexical scope. I'm adding the ideas from the book as I go. I'm using an alist representation of environment frames, but it's somewhat arbitrary.


## Tests

The tests give a good description of the behaviour.

- [x] "numbers are self-evaluating"
- [x] "strings are self-evaluating"
- [x] "variables return their bound value"
- [x] "evaluating an unbound variable causes an exception"
- [x] "'define statements create a binding of var to val in the current scope"
- [x] "evaluating a lambda returns a 'procedure-tagged list"
- [x] "'define with a lambda evaluates the lambda and binds it to the variable"
- [x] "variable definitions evaluate their values before binding"
- [x] "variable definitions evaluate their values before binding, left left lambda"
- [x] "'define updates an existing binding if exists"
- [x] "primitive procedures are evaluated scheme"
- [x] "unary primitives are evaluated by scheme"
- [x] "left left lambdas are immediately evaluated and applied"
- [x] "defined procedures are applied to their operands"
- [x] "quoted expressions return the expression"
- [x] "quoted lists return the list"
- [x] "'begin expressions eval all subexpressions, returning the last result"
- [x] "'set! expressions mutate an existing variable in current scope"
- [x] "'set! on an undefined var throws an error"
- [x] "'if forms evaluate their consequent if the predicate passes"
- [x] "'if forms return false if the predicate fails and there is no alt"
- [x] "'let forms are evaluated"
- [x] "'if forms do not evaluate their consequent if the predicate fails"
- [x] "case forms are transformed to let"
- [x] "let forms are transformed to left left lambdas"
- [x] "cond forms are transformed to series of if forms"
- [x] "let* forms are transformed to nested l l lambdas"
- [x] "'let* forms are evaluated"
- [x] "named let forms are evaluated"
- [x] "letrec handles mutually recursive procedures in its bindings"
- [x] "letrec forms transform to let with simultaneous scope"
- [x] "named-let forms are transformed to letrec"
- [x] "cond clauses with arrow forms are evaluated"
- [x] "and forms short-circuit"
- [x] "and forms return true if no clauses"
- [x] "and forms return the value of the last expression if all true"
- [x] "or forms return the first value that evaluates to true"
- [x] "or forms return false if no clauses"
- [x] "or forms return false if all expressions evaluate to false"
- [x] "internal definitions are hoisted"

Ran 40 tests in 0.0s:

	-  40 PASSED
	-   0 FAILED