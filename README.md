## A4: Compiler for *L<sub>Fun</sub>* Language

1. Implement all passes required to support functions by exending *L<sub>Tup</sub>* to *L<sub>Fun</sub>*. Refer fig. 7.10 and exercise 33 from textbook for reference.

## Deadline
24th April, 2022. 23:59

<br><br>

## A3: Dataflow analysis, Garbage collection and Compiler for *L<sub>while</sub>*, *L<sub>Tup</sub>* Languages

1. Implement all the passes required to support loops by extending *L<sub>if</sub>*
to *L<sub>while</sub>*. Use fig. 5.8 from textbook for reference.
2. Implement all the passes required to support tuples(vectors) by extending *L<sub>while</sub>* to *L<sub>Tup</sub>*. Use fig. 6.17 from textbook for reference.
3. Teams with **4 students** must additionally implement `constant_propagation` pass after `select_instructions`

## Deadline
13th April, 2022. 23:59

<br><br>

## A2: Register allocation and Compiler for *L<sub>if</sub>* Language

## Homework questions
1. Solve **Exercises 13 to 17** and **Exercises 19 to 27** of the text book.
2. Teams with **4 students** must additionally solve **Exercise 18, 28 and 29** too.

## Deadline
13th March, 23:59

<br><br>

## A1: Compiler for *L<sub>var</sub>* Language

## Homework questions
1. Solve **Exercises 2 to 9** of the text book.
2. Teams with **4 students** must additionally solve **Exercise 10 and 11** too.

## Deadline
28rd February, 23:59

## Homework instructions

For your homework exercises, you will be expected to implement various
compiler passes. It will ultimately be up to you how exactly to do
this, but for the first assignment you are given code templates in
`compiler.rkt` to fill out.

As you fill out the functions in `compiler.rkt`, tests are run with the
`run-tests.rkt` module. You can run these tests either from the command
line with:

```
   racket run-tests.rkt
```

Or by opening and running `run-tests.rkt` in DrRacket.

Before running the compiler tests, you need to compile
`runtime.c` (see below).

## Public student code

Utility code, test suites, etc. for the compiler course.

This code will be described in the Appendix of the book.

The `runtime.c` file needs to be compiled and linked with the assembly
code that your compiler produces. To compile `runtime.c`, do the
following
```
   gcc -c -g -std=c99 runtime.c
```
This will produce a file named `runtime.o`. The -g flag is to tell the
compiler to produce debug information that you may need to use
the gdb (or lldb) debugger.

Next, suppose your compiler has translated the Racket program in file
`foo.rkt` into the x86 assembly program in file `foo.s` (The .s filename
extension is the standard one for assembly programs.) To produce
an executable program, you can then do
```
  gcc -g runtime.o foo.s
```
which will produce the executable program named a.out.

There is an example "compiler" in the file `compiler.rkt`.  That
file defines two passes that translate R_0 programs to R_0 programs
and tests them using the `interp-tests` function from `utilities.rkt`. It
tests the passes on the three example programs in the tests
subdirectory. You may find it amusing (I did!) to insert bugs in the
compiler and see the errors reported. Note that `interp-tests` does not
test the final output assembly code; you need to use `compiler-tests`
for that purpose. The usage of `compiler-tests` is quite similar to
`interp-tests`. Example uses of these testing procedures appear in
`run-tests.rkt`.

As new languages are added, `run-tests.rkt` will be extended to
test new passes. You will be provided with new iterations of
the script for each assignment.