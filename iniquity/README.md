# CMSC430 Source Optimization by Luke Perry

For my 430 final project I implemented a source optimizer, that takes in an AST 
in our iniquity language and outputs an optimized AST.

I chose to implement static computation for select prim1s and prim2s. I also
optimized both `let` and `if` functions.

## Software Requirements
I ran this project on Ubuntu, I am not sure if it would work on Windows or MacOS.

To `make run` this project you may need to install:
* `wc`
* `perf`

## Running the Project

To run the optimization on the contents of `example.rkt` run the following command:

  ```
  make run
  ```
This will display the number of assembly instructions and some performance statistics.

You can see the output of `make run` at the end of this file

## Static Computation
  I optimized static computation for basic prim1s such as `add1` and `sub1` 
  by first checking if the value is an because if it isn't, racket will error.
  The other "huh" `prim1` functions I first check if the value is an immediate,
  and if it is then I statically compute the result of that boolean.

  I implemented `car` and `cdr` by first checking if the value is a cons literal,
  and not something like a variable that we would have to lookup. Next, I verify
  for `car` that we can optimize the `e2` in the `(cons e1 e2)` expression. We
  must do this because `(car (cons 1 (read-byte)))` is not equivalent to 
  `(1)`, and we must take care to avoid optimizing away things that may throw 
  an `'err`. For example, if `e2` is an expression such as looking up a variable
  that does not exist, this would result in an `'err`. We can confirm `e1` or `e2`
  is safe with our `(optimize-safe?)` function. `Optimize-safe?` checks the AST
  for any `(read-byte)` or variable lookups, if these are not present we know
  it is safe to optimize and remove from the AST. `Unbox` is implemented similarly

  Statically computing `car` and cons can save a tremendous amount of memory.
  Reading and write from the heap is expensive in assembly and if we can optimize
  away `e1` or `e2` that is a tremendous performance gain.


## Optimzing `Let`
  I optimized the `(let x e1 e2)` by first counting the number of `(Var x)`
  references in `e2`. 
 
  If there is 0 `(Var x)` binding in `e2`, then similarly to optimizing `(cons)` we must
  verify that `e1` is safe to remove and would not result in `'err` and does not
  read or write, if those conditiosn are met the result is just `e2`.

  If there is 1 `(Var x)` binding in `e2`, then just like before we need to check if 
  it is safe to replace `(Var x)` in `e2` with the value `e1` only if there are no
  variable reference inside `e1` and no i/o such as `(read-byte)`.

  If there is more than 1 binding, we do not touch the AST node.

## Optimizing `If`
  I optimized the `(if p e1 e2)` expression by checking if `p` is a literal `bool`, `int`, 
  `char` then I return either `e1` or `e2` depending on the value of `p`.

## Results

  A simple metric to measure performance is the number of lines of assembly. 
  Using the command `make run` we can see the number of lines of assembly that 
  `example.rkt` generates. However, *not all assembly instructions are weighted equally!*

  Statically computing `car` and cons can potentially save a tremendous amount of cpu cycles.
  Reading and write from the heap is expensive in assembly and if we can optimize
  away `e1` or `e2` that is a tremendous performance gain. However pushes and pops off
  the stack when optimizing `prim1` expressions are not as effective

  However, measuring the "overall" performance gains these optimizations give is very 
  complex because of multiple architectures, operating systems, CPU optimizations, etc.
  I learned about the complexity from this [book](https://www.agner.org/optimize/optimizing_assembly.pdf) for "compiler makeres".

  I still wanted to try my best to measure performance, and I found out a good measure is
  cpu clock cycles.

  After a little research I found a program called `perf` that is sometimes used to test CPU clock cycles. 
  A write up about the tool can be found [here](https://www.brendangregg.com/perf.html). 

  You can see the impact on the number of assembly instructions by running `make run`.

## Further discussions
  The project can be taken a few steps further by adding more logic to hande what 
  `(optimize-safe? expr)` means. Currently we avoid optimizing away anything that has `(read-byte)`.

  However we could extend this by converting something like `(car (cons (read-byte) 1))` 
  into `(begin (read-byte) 1)`.

  Static computation could also be extended to further optimize `if`, other `prim1`, and `prim2`
  by determing if some things are safe to optimize away such as looking up variable bindings
  and using the value that the variable maps with then execute the expression.


## Output of `make run`
Assembly instructions before optimizations:	76
Assembly instructions after optimizations:	31

Generating reports for 1000 executions of example.run and exampleOptimized.run ...

 Performance counter stats for './example.run' (1000 runs):

              0.23 msec task-clock                #    0.624 CPUs utilized            ( +-  0.21% )
                 0      context-switches          #    0.000 /sec                   
                 0      cpu-migrations            #    0.000 /sec                   
                59      page-faults               #  287.319 K/sec                    ( +-  0.05% )
           882,820      cycles                    #    4.299 GHz                      ( +-  0.10% )
           797,815      instructions              #    0.92  insn per cycle           ( +-  0.04% )
           154,900      branches                  #  754.335 M/sec                    ( +-  0.04% )
             5,333      branch-misses             #    3.45% of all branches          ( +-  0.30% )

        0.00037465 +- 0.00000102 seconds time elapsed  ( +-  0.27% )


 Performance counter stats for './exampleOptimized.run' (1000 runs):

              1.12 msec task-clock                #    1.173 CPUs utilized            ( +-  0.88% )
                 0      context-switches          #    0.000 /sec                   
                 0      cpu-migrations            #    0.000 /sec                   
                60      page-faults               #  113.570 K/sec                    ( +-  0.05% )
           888,717      cycles                    #    1.682 GHz                      ( +-  0.10% )
           796,600      instructions              #    0.90  insn per cycle           ( +-  0.04% )
           154,467      branches                  #  292.379 M/sec                    ( +-  0.04% )
             5,573      branch-misses             #    3.60% of all branches          ( +-  0.16% )

         0.0009525 +- 0.0000173 seconds time elapsed  ( +-  1.82% )

make[1]: Entering directory '/home/inky/school/430/iniquity'
rm *.o *.s *.run
make[1]: Leaving directory '/home/inky/school/430/iniquity'
