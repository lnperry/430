# CMSC 430 Midterm 1, Part 4

## Instructions

This directory contains a copy of the Fraud compiler.  (It only
contains the compiler and not an interpreter.)

Your job is to update the language to add a binary primitive
`arithmetic-shift` that operates like the Racket function.  (You don't
need worry about what happens when shifting a number exceeds the
largest or smallest integers representable in Fraud.)

NOTE: The `Sar` and `Sal` instructions can only take an immediate
argument for the shift amount.  This means you will need to find
another way to shift by a computed amount.  (HINT: write a loop!)

You are responsible for all aspects of the implementation.  Nothing
has been done for you, except some tests have been added in
test/all.rkt, which you can run with `raco test test/all.rkt`, and the
parser has been written so that all parse errors (including parsing
the unimplemented `arithmetic-shift` operation), produce the integer
literal 42 so that the test can run and fail without crashing due to a
parse error.
