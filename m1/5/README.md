# CMSC 430 Midterm 1, Part 6

## Instructions

Let's remind ourselves of the instruction we emit for the `box` primitive:

```
(seq (Mov (Offset rbx 0) rax)
     (Mov rax rbx)
     (Or rax type-box)
     (Add rbx 8))
```

As discussed in the notes and the lectures, we increment `rbx` by 8
in order to allocate space on our heap (a region of memory we had our
RTS allocate for us).

However, we didn't implement garbage collection. So our code allocates,
but we never deal with the fact that we can possibly run out of memory.
Currently our system will keep bumping our heap pointer until we go
beyond the region that our RTS `malloc`ed, at which point we will
likely segfault.

Segfaults are very frustrating! This is also frustrating for the users
of our languages, where pointers aren't even visible in the programs
they write!

Let's fix this.

Answer the following questions:


- At a high level, how would you go about changing the code so that when we
  reach the end of our allocated heap, we end the program with
  "Error: out of memory!"? This way the programmer actually knows what went
  wrong, instead of just seeing "segfault".

  The point here is _not_ to describe Garbage Collection, quite the opposite,
  we only want a clearer error message. You should describe what parts of the
  compiler need to change and how, but you are not expected to implement these
  changes, only reason through them and explain your reasoning.

- With the context from your answer to the first part: what would the
  instructions emitted for `box` look like with all of your changes? Feel free
  to use some pseudo-code, but you should still be specifying things
  concretely. So if you need labels, don't worry about generating appropriate
  labels with gensym, we aren't running your code, we just need to get the idea.

Write your answer in answer.txt.
