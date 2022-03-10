# CMSC 430 Midterm 1, Part 3

## Instructions

You've been provided a file ast.rkt that contains the AST data type
definition for the Fraud language as presented in class.

Recall that as a Fraud expression is evaluated, the stack is used to
store variable bindings and intermediate results in binary operations.

It is possible to compute the maximum number of elements that will
need to be pushed on the stack given an expression.

Design a function:

```
;; max-stack-size : Expr -> Natural
;; Compute the maximum number of elements pushed on the stack
;; when executing the compiled code for the given expression.
```

For the purposes of this problem, you may assume calls to the runtime
system such as `read_byte`, `write_byte`, etc. do not change the
maximum stack size.  See the provided tests for examples.

You can test your solution by running `raco test max-stack.rkt` or
pressing Run in DrRacket.  You are encouraged to add additional tests.
