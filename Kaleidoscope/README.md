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
