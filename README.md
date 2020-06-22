# Scheme Interpreter

This is a Scheme interpreter, written in Chez Scheme. It is an exercise of ideas from chapter 4 of The Structure and Interpretation of Computer Programs ([wizard book](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html)), and [William Byrd](https://www.youtube.com/channel/UCSC9kYeTee012BRsYw-y12Q)'s hangout series on youtube.

The idea is to implement an environment passing interpreter, with lexical scope. I'm adding the ideas from the book as I go.

## Tests

The tests give a good description of the behaviour.


- [] "numbers are self-evaluating"
- [] "strings are self-evaluating"
- [] "variables return their bound value"
- [] "evaluating an unbound variable causes an exception"
- [] "'define statements create a binding of var to val in the current scope"
- [] "evaluating a lambda returns a 'procedure-tagged list"
- [] "'define with a lambda evaluates the lambda and binds it to the variable"
- [] "variable definitions evaluate their values before binding"
- [] "variable definitions evaluate their values before binding, left left lambda"
- [] "'define updates an existing binding if exists"
- [] "primitive procedures are evaluated scheme"
- [] "unary primitives are evaluated by scheme"
- [] "left left lambdas are immediately evaluated and applied"
- [] "defined procedures are applied to their operands"
- [] "quoted expressions return the expression"
- [] "quoted lists return the list"
- [] "'begin expressions eval all subexpressions, returning the last result"
- [] "'set! expressions mutate an existing variable in current scope"
- [] "'set! on an undefined var throws an error"
- [x] "if statement 1" FAILED - expected <true> got <#t>
- [x] "if statement: no alternative" FAILED - expected <false> got <#f>