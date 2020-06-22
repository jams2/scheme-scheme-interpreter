# Scheme Interpreter

This is a Scheme interpreter, written in Chez Scheme. It is an exercise of ideas from chapter 4 of The Structure and Interpretation of Computer Programs ([wizard book](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html)), and [William Byrd](https://www.youtube.com/channel/UCSC9kYeTee012BRsYw-y12Q)'s hangout series on youtube. There's also an example in [The Scheme Programming Language](https://www.scheme.com/tspl4/examples.html#./examples:h7), by Kent Dybvig.

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
- [x] "'if statements evaluate their consequent if the predicate passes"
- [x] "'if statements return false if the predicate fails and there is no alt"
- [x] "'let statements are evaluated"
- [x] "'if statements do not evaluate their consequent if the predicate fails"
- [x] #<procedure case->let at sicp.ss:2126>
- [x] #<procedure let->lambda at sicp.ss:2845>
- [x] #<procedure cond->if at sicp.ss:2550>
