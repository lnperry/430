# CMSC 430 Midterm 2, Part 2

## Instructions

You've been provided a slightly modified version of the Hoax language
as presented in class.

Many languages, including Racket and Java, will ***intern*** string
literals.  Interning string literals means that multiple occurrences
of the same string literal within a program will be allocated just
once.

You can observe this behavior in Racket with the following example:

```
(eq? "foo" "foo") ; => #t
```

The `eq?` function checks for pointer equality.

The two occurrences of `"foo"` are pointer-equal to each other and in
fact there is only one string allocated in memory.

The Hoax compiler did not do this; it allocated two distinct strings,
one for each occurrence, and therefore this example would produce `#f`
in the compiler as presented in class.

Interning string literals doesn't mean that every string that consists
of the same characters will be pointer-equal to another consisting of
exactly the same characters.  For example:

```
(eq? "fff" (make-string 3 #\f)) ; => #f
```

Even though `"fff"` and `(make-string 3 #\f)` both produce the string
`"fff"`, this example produces false because the `(make-string 3 #\f)`
expression allocates a new string: it produces `"fff"` but it is not
the ***literal*** string `"fff"`.

One simple way of achieving this kind of string literal interning is
by program transformation.

If the original example `(eq? "foo" "foo")` were instead written as
`(let ((x "foo")) (eq? x x))` then an unmodified Hoax compiler
***would*** produce true.

This leads to the following idea: take an expression that may contain
many instances of the same literal string and replace it with a single
instance of that literal, bound to a variable with many references.

Design a function:

```
;; intern : Expr -> Expr
;; Transform given expression so that all string literals are interned.
```

Your `intern` function should have the following properties:

* it preserves the meaning of the original expression with the sole
  exception that literals compared with `eq?` in the original that
  used to produce `#f` may now produce `#t`.

* any string literal should occur at most once in the transformed
  expression.

* your `intern` function should cause all of the provided compiler
  tests to pass.  Moreover, it should work for any possible test
  involving the Hoax language.

A stub of the function is provided in `intern.rkt`.  This is the only
file you may change.  No other part of the compiler can be altered,
but notice that the `compile` function in `compile.rkt` has been
changed to call `intern` on the input expression before it is compiled.
