# CMSC 430 Midterm 2, Part 4

## Instructions

Languages take different approaches when a function is called with the
wrong number of arguments.  Most statically typed languages like OCaml
and Java will use the type system to rule out such programs.  Racket
treats such mismatches as run-time errors and halts the computation
when such an error occurs.

We saw how to implement this run-time error checking mechanism in
assignment 5.  Without adding the arity checking of assignment 5, the
compilers for Iniquity, Jig, and Loot produce code that simply crashes
or produces wrong answers when arity mismatches occur.

Javascript takes a different approach.  The design of Javascript is
largely driven by the philosophy to ***keep going*** in the face of
possible errors.  Very few things in Javascript result in what we
would call a run-time error: something that stops further computation.
Arity mismatches are a good example.  If a function is called with the
wrong number of arguments in Javascript, the program simply keeps
going.  It doesn't crash and it doesn't signal an error.  It has a
well-specified semantics for such cases.

Here's what Javascript does:

* if a function is called with too many arguments, the extra arguments
  are evaluated but then ignored in the body of the function.
  
* if a function is called with too few arguments, the remaining
  parameters are bound to a special `undefined` value.

For example:

```
function f(x) { return x; }
f();
```

This calls `f` with 0 arguments, even though it expects 1, hence `x`
is bound to the `undefined` value and therefore the call returns `undefined`.

Despite the name, `undefined` is just a value in Javascript.  There's
really nothing special about it.

Here's a slight modification:

```
function f(x, y) { return x; }
f(1);
```

This calls `f` with 1 argument, even though it expects 2.  Here `x` is
bound to `1` and `y`, since there is no second argument, is bound to
`undefined`.  But the body of the function doesn't actually use `y`,
so the call returns `1` since that's what `x` was bound to.

Let's see what happens when you call a function with too many
arguments.  For example:

```
function f(x) { return x; }
f(1,2,3);
```

This calls `f` with 3 arguments even though it only expects 1.  The
parameter `x` is bound to the first argument, i.e. `1`.  The remaining
arguments are not bound to anything and therefore are inaccessible in
the body of the function.

You are given a slightly modified version of the Iniquity language as
presented in class.  Only the interpreter has been modified and it has
been changed to implement the Javascript-style approach to arity
mismatches.  Instead of `undefined` it uses `void` as the value to
bind to parameters that have no corresponding argument.

There are three example programs corresponding to the Javascript
programs above:

`example1.rkt`:
```
#lang racket
(define (f x) x)
(f) ; => void
```

`example2.rkt`:
```
#lang racket
(define (f x y) x)
(f 1) ; => 1
```

`example3.rkt`:
```
#lang racket
(define (f x) x)
(f 1 2 3) ; => 1
```

You can see what the interpreter produces by running:
```
racket -t interp-file.rkt -m example1.rkt
racket -t interp-file.rkt -m example2.rkt
racket -t interp-file.rkt -m example3.rkt
```

Note that this is ***different*** from how Racket would interpret
these programs, which is to signal an error.  So if you run these
programs directly in Racket, you'll get errors.

Your job is to explain _is as much detail as possible_ how you would modify
`compile.rkt` in order to make it consistent with the interpreter.

You may choose to implement it, if you feel that you have the time/inclination,
but only an explanation of what is needed is required.

A reasonably competent compiler engineer (e.g. a TA) should be able to take
your description and implement it without needing to think for themselves.
Pseudocode is encouraged, but not required.

Note: you do not have to change the behavior of primitive operations, only
user-defined function calls.
