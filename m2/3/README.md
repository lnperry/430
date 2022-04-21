# CMSC 430 Midterm 2, Part 3

## Instructions

This problem builds upon ***understanding*** problem 2, so make sure
you have read and understood problem 2.  However, you do not need to
solve problem 2 in order to solve this one.

Suppose you have correctly implemented the `intern : Expr -> Expr`
function from problem 2 and would now like to implement string literal
interning for Iniquity, the language after Hoax that adds the
ability to define and call functions.

For Hoax, it was possible to implement string literal interning by
calling `intern` within the `compile` function like so:

```
;; Expr -> Asm
(define (compile e)
  (prog (externs)           
        (Global 'entry)
        (Label 'entry)
        (Mov rbx rdi) ; recv heap pointer
        (compile-e (intern e) '())      ; <------- call to intern
        (Ret)
        (Label 'raise_error_align)
        pad-stack
        (Call 'raise_error)))
```

Now extending this idea to Iniquity, it's tempting to try to adapt
this approach by calling `intern` on the top-level expression
***and*** the function bodies of every function definition.

Suppose we use the following for `compile` and `compile-define` in
Iniquity:

```
;; ...

(require "intern.rkt")

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)  
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e (intern e) '())      ; <------- call to intern
           (Ret)
           (compile-defines ds)
           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error))]))

;; ...

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (seq (Label (symbol->label f))
          (compile-e (intern e) (reverse xs)) ; <---- call to intern
          (Add rsp (* 8 (length xs))) ; pop args
          (Ret))]))
```

The rest of the compiler stays the same.

Give an example showing that this compiler is broken. Write a concrete Iniquity
program that demonstrates a mismatch in behavior between the string literal
interning in Racket and this modified version of Iniquity.

Add an explanation of why this example demonstrates the problem.  Also you may
be docked points for a correct example with an incorrect explanation.
