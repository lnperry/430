# CMSC 430 Midterm 1, Part 2

## Instructions

In our lecture we discussed how some features can be implemented in terms of
other features. The process of implementing some features by translating it
to a (potentially) more verbose 'core' feature is often called "desugaring".

The example used in class was that with Fraud, we no longer need a `Begin`
construct in our AST. This is because the semantics we use for `let`[^1] we
could perform the following translation internally in our compiler:

```
(begin e1 e2)  ==>  (let ((x e1)) e2)
```

Where we need to ensure that `x` is not a free variable in `e2` (which
we can use `gensym` to do). In other words, we can bind `e1` to a variable,
and then ignore that variable.

With this transformation in our compiler, the programmer could write
`(begin e1 e2)` but we only have to worry about how to implement `let`.

Providing the programmer with these syntactic conveniences is known
as "Syntactic Sugar" (and hence the term "desugaring").

### A wrinkle

The issue is that while this transformation preserves the _semantic_
behavior of the program (for all programs that terminate, we'll get the
same result), it does not preserve the _operational_ behavior the program.

These two things will emit different instructions, and that difference
can have an impact on performance or resource usage.


### Your task

Answer the following questions:


- What are the differences between `begin` as we have it in Fraud, and
  `begin` being transformed into `let` as we showed above?

- What consequences can these differences have?

- What feature can you add so that you can ensure that the desugared
  version of `begin` emits the same exact code as the version of `begin`
  we have in Fraud? Hint: you'll either need to add something to `let`, or
  add a very `let`-like thing. You'll then need to tweak the transformation
  in a very minor way.

Write your answer in answer.txt.


[^1]: This is not true for all semantics for `let`, but it's true for the vast
      majority of languages that have `let`.
