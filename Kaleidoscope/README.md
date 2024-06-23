# My First Language Frontend with LLVM Tutorial

## The Kaleidoscope Language

[HOMEPAGE](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)

This is the “Kaleidoscope” Language tutorial, showing how to implement a simple language using LLVM components in C++.

Kaleidoscope is a procedural language that allows you to define functions, use conditionals, math, etc. Over the course of the tutorial, we'll extend Kaleidoscope to support the if/then/else construct, a for loop, user defined operators, JIT compilation with a simple command line interface, debug info, etc.

We want to keep things simple, so the only datatype in Kaleidoscope is a 64-bit floating point type (aka ‘double’ in C parlance). As such, all values are implicitly double precision and the language doesn’t require type declarations. This gives the language a very nice and simple syntax. For example, the following simple example computes Fibonacci numbers:

```python
# Compute the x'th fibonacci number.
def fib(x)
    if x < 3 then
        1
    else
        fib(x-1) + fib(x-2)

# This expression will compute the 40th number.
fib(40)
```

## The Lexer

## Implementing a Parser and AST

This chapter shows you how to use the lexer, built in Chapter 1, to build a full parser for Kaleidoscope language.

```llvm
$ ./toy
ready> def foo(x y) x+foo(y, 4.0);
ready> Parsed a function definition.
ready> def foo(x y) x+y y;
ready> Parsed a function definition.
ready> Parsed a top-level expr.
ready> def foo(x y) x+y );
ready> Parsed a function definition.
ready> Error: unknown token when expecting an expression
ready> extern sin(a);
ready> Parsed an extern.
ready> ^D
```

## Code generation to LLVM IR

This chapter shows you how to transform the Abstract Syntax Tree, built in Chapter 2, into LLVM IR.

```llvm
ready> 4+5;
ready> Read top level expression:
define double @__anon_expr() {
entry:
  ret double 9.000000e+00
}

ready> def foo(a b) a*a + 2*a*b + b*b;
ready> Read function definition:
define double @foo(double %a, double %b) {
entry:
  %MulTemp = fmul double %a, %a
  %MulTemp1 = fmul double 2.000000e+00, %a
  %MulTemp2 = fmul double %MulTemp1, %b
  %AddTemp = fadd double %MulTemp, %MulTemp2
  %MulTemp3 = fmul double %b, %b
  %AddTemp4 = fadd double %AddTemp, %MulTemp3
  ret double %AddTemp4
}

ready> def bar(a) foo(a, 4.0) + bar(2024);
ready> Read function definition:
define double @bar(double %a) {
entry:
  %CallTemp = call double @foo(double %a, double 4.000000e+00)
  %CallTemp1 = call double @bar(double 2.024000e+03)
  %AddTemp = fadd double %CallTemp, %CallTemp1
  ret double %AddTemp
}

ready> extern cos(x);
ready> Read extern:
declare double @cos(double)

ready> cos(2);
ready> Read top level expression:
define double @__anon_expr() {
entry:
  ret double 9.000000e+00

entry1:                                           ; No predecessors!
  %CallTemp = call double @cos(double 2.000000e+00)
  ret double %CallTemp
}

ready> ^D
ready> ; ModuleID = 'JIT'
source_filename = "JIT"

define double @__anon_expr() {
entry:
  ret double 9.000000e+00

entry1:                                           ; No predecessors!
  %CallTemp = call double @cos(double 2.000000e+00)
  ret double %CallTemp
}

define double @foo(double %a, double %b) {
entry:
  %MulTemp = fmul double %a, %a
  %MulTemp1 = fmul double 2.000000e+00, %a
  %MulTemp2 = fmul double %MulTemp1, %b
  %AddTemp = fadd double %MulTemp, %MulTemp2
  %MulTemp3 = fmul double %b, %b
  %AddTemp4 = fadd double %AddTemp, %MulTemp3
  ret double %AddTemp4
}

define double @bar(double %a) {
entry:
  %CallTemp = call double @foo(double %a, double 4.000000e+00)
  %CallTemp1 = call double @bar(double 2.024000e+03)
  %AddTemp = fadd double %CallTemp, %CallTemp1
  ret double %AddTemp
}

declare double @cos(double)
```
