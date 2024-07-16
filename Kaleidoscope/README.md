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

## Adding JIT and Optimizer Support

LLVM provides many optimization passes, which do many different sorts of things and have different tradeoffs.

For example, with two transformations: reassociation of expressions (to make the add’s lexically identical) and Common Subexpression Elimination (CSE), we can Optimize code like above.

Before Optimize:

```llvm
ready> def test(x) (1+2+x)*(x+(1+2));
ready> Read function definition:
define double @test(double %x) {
entry:
  %AddTemp = fadd double 3.000000e+00, %x
  %AddTemp1 = fadd double %x, 3.000000e+00
  %MulTemp = fmul double %AddTemp, %AddTemp1
  ret double %MulTemp
}
```

After Optimize:

```llvm
ready> def test(x) (1+2+x)*(x+(1+2));
ready> Read function definition:
define double @test(double %x) {
entry:
  %AddTemp = fadd double %x, 3.000000e+00
  %MulTemp = fmul double %AddTemp, %AddTemp
  ret double %MulTemp
}
```

Now that we have reasonable code coming out of our front-end, let’s talk about executing it!

Here, we’ll add JIT compiler support to our interpreter. The basic idea that we want for Kaleidoscope is to have the user enter function bodies as they do now, but immediately evaluate the top-level expressions they type in. For example, if they type in “1 + 2;”, we should evaluate and print out 3. If they define a function, they should be able to call it from the command line.

```llvm
ready> 4+5;
ready> Read top level expression:
define double @__anon_expr() {
entry:
  ret double 9.000000e+00
}

Evaluated to 9.000000
ready> def foo(x) x+1;
ready> Read function definition:
define double @foo(double %x) {
entry:
  %AddTemp = fadd double %x, 1.000000e+00
  ret double %AddTemp
}

ready> foo(2023);
ready> Read top level expression:
define double @__anon_expr() {
entry:
  %CallTemp = call double @foo(double 2.023000e+03)
  ret double %CallTemp
}

Evaluated to 2024.000000
ready> extern cos(x);
ready> Read extern:
declare double @cos(double)

ready> cos(37);
ready> Read top level expression:
define double @__anon_expr() {
entry:
  %CallTemp = call double @cos(double 3.700000e+01)
  ret double 0x3FE87E459C20218C
}

Evaluated to 0.765414
ready> ready> ^D
; ModuleID = 'JIT'
source_filename = "JIT"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
```

## Extending the Language: Control Flow

Unfortunately, as presented, Kaleidoscope is mostly useless: it has no control flow other than call and return. This means that you can't have conditional branches in the code, significantly limiting its power. In this episode of "build that compiler", we'll extend Kaleidoscope to have an if/then/else expression plus a simple 'for' loop.

```LLVM
ready> extern foo();
ready> Read extern:
declare double @foo()

ready> extern bar();
ready> Read extern:
declare double @bar()

ready> def baz(x) if x then foo() else bar();
ready> Read function definition:
define double @baz(double %x) {
entry:
  %ifcond = fcmp ueq double %x, 0.000000e+00
  br i1 %ifcond, label %else, label %then

then:                                             ; preds = %entry
  %CallTemp = call double @foo()
  br label %ifcont

else:                                             ; preds = %entry
  %CallTemp1 = call double @bar()
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %IfTemp = phi double [ %CallTemp, %then ], [ %CallTemp1, %else ]
  ret double %IfTemp
}
```

Now that we know how to add basic control flow constructs to the language, we have the tools to add more powerful things. Let’s add something more aggressive, a ‘for’ expression:

```llvm
ready> extern bar(x);
ready> Read extern:
declare double @bar(double)

ready> def baz(n)
ready>   for i = 1, i < n, 1.0 in bar(i);
Read function definition:
define double @baz(double %n) {
entry:
  br label %Loop

Loop:                                             ; preds = %Loop, %entry
  %i = phi double [ 1.000000e+00, %entry ], [ %nextval, %Loop ]
  %CallTemp = call double @bar(double %i)
  %nextval = fadd double %i, 1.000000e+00
  %CMPTemp = fcmp ult double %i, %n
  br i1 %CMPTemp, label %Loop, label %afterloop

afterloop:                                        ; preds = %Loop
  ret double 0.000000e+00
}

ready> ^D
ready> ; ModuleID = 'JIT'
source_filename = "JIT"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
```

## Extending the Language: User-defined Operators

At this point in our tutorial, we now have a fully functional language that is fairly minimal, but also useful. There is still one big problem with it, however. Our language doesn’t have many useful operators (like division, logical negation, or even any comparisons besides less-than).

This chapter of the tutorial takes a wild digression into adding user-defined operators to the simple and beautiful Kaleidoscope language.

First, add unary operators, like logical operator not.

```llvm
ready> def unary!(v) if v then 0 else 1;
ready> Read function definition:
define double @"unary!"(double %v) {
entry:
  %ifcond = fcmp ueq double %v, 0.000000e+00
  %. = select i1 %ifcond, double 1.000000e+00, double 0.000000e+00
  ret double %.
}

ready> !(5);
ready> Read top level expression:
define double @__anon_expr() {
entry:
  %unop = call double @"unary!"(double 5.000000e+00)
  ret double %unop
}

Evaluated to 0.000000
ready> !(0);
ready> Read top level expression:
define double @__anon_expr() {
entry:
  %unop = call double @"unary!"(double 0.000000e+00)
  ret double %unop
}

Evaluated to 1.000000
```

And, also the binary operator, but noticed that, binary operator involve precedence issues. Here we define binary operator and precedence together.

```llvm
ready> def binary> 10 (LHS RHS) RHS < LHS;
ready> Read function definition:
define double @"binary>"(double %LHS, double %RHS) {
entry:
  %CMPTemp = fcmp ult double %RHS, %LHS
  %BoolTemp = uitofp i1 %CMPTemp to double
  ret double %BoolTemp
}

ready> 1 < 0;
ready> Read top level expression:
define double @__anon_expr() {
entry:
  ret double 0.000000e+00
}

Evaluated to 0.000000
ready> 0 < 1;
ready> Read top level expression:
define double @__anon_expr() {
entry:
  ret double 1.000000e+00
}

Evaluated to 1.000000
```

```llvm
ready> def binary| 5 (LHS RHS) if LHS then 1 else if RHS then 1 else 0;
ready> Read function definition:
define double @"binary|"(double %LHS, double %RHS) {
entry:
  %ifcond = fcmp ueq double %LHS, 0.000000e+00
  %ifcond1 = fcmp ueq double %RHS, 0.000000e+00
  %. = select i1 %ifcond1, double 0.000000e+00, double 1.000000e+00
  %IfTemp5 = select i1 %ifcond, double %., double 1.000000e+00
  ret double %IfTemp5
}

ready> 1 | 0;
ready> Read top level expression:
define double @__anon_expr() {
entry:
  %binop = call double @"binary|"(double 1.000000e+00, double 0.000000e+00)
  ret double %binop
}

Evaluated to 1.000000
ready> 0 | 1;
ready> Read top level expression:
define double @__anon_expr() {
entry:
  %binop = call double @"binary|"(double 0.000000e+00, double 1.000000e+00)
  ret double %binop
}

Evaluated to 1.000000
ready> 0 | 0;
ready> Read top level expression:
define double @__anon_expr() {
entry:
  %binop = call double @"binary|"(double 0.000000e+00, double 0.000000e+00)
  ret double %binop
}

Evaluated to 0.000000
```

Other operator can be define like above.

```text
# Logical unary not.
def unary!(v)
  if v then
    0
  else
    1;

# Unary negate.
def unary-(v)
  0-v;

# Define > with the same precedence as <.
def binary> 10 (LHS RHS)
  RHS < LHS;

# Binary logical or, which does not short circuit.
def binary| 5 (LHS RHS)
  if LHS then
    1
  else if RHS then
    1
  else
    0;

# Binary logical and, which does not short circuit.
def binary& 6 (LHS RHS)
  if !LHS then
    0
  else
    !!RHS;

# Define = with slightly lower precedence than relationals.
def binary = 9 (LHS RHS)
  !(LHS < RHS | LHS > RHS);

# Define ':' for sequencing: as a low-precedence operator that ignores operands
# and just returns the RHS.
def binary : 1 (x y) y;
```

## Extending the Language: Mutable Variables

In this chapter, we introduce mutable variables into Kaleidoscope. Noticed that it not simple, LLVM require all register values to be in SSA form, but look example of mutable variable:

```c
int G, H;
int test(_Bool Condition) {
  int X;
  if (Condition)
    X = G;
  else
    X = H;
  return X;
}
```

The LLVM IR that we want for this example looks like this:

```llvm
@G = weak global i32 0   ; type of @G is i32*
@H = weak global i32 0   ; type of @H is i32*

define i32 @test(i1 %Condition) {
entry:
  br i1 %Condition, label %cond_true, label %cond_false

cond_true:
  %X.0 = load i32, i32* @G
  br label %cond_next

cond_false:
  %X.1 = load i32, i32* @H
  br label %cond_next

cond_next:
  %X.2 = phi i32 [ %X.1, %cond_false ], [ %X.0, %cond_true ]
  ret i32 %X.2
}
```

We use PHI node to merge the incoming value form different branch. The question is "who places the PHI node when lowering assignments to mutable variables?". The issue here is that LLVM requires that its IR be in SSA form: there is no "non-ssa" mode for it.

The trick here is that while LLVM does require all register values to be in SSA form, it does not require (or permit) memory objects to be in SSA form.

With this in mind, the high-level idea is that we want to make a stack variable (which lives in memory, because it is on the stack) for each mutable object in a function.

Now, we could rewrite the example to use the alloca technique to avoid using a PHI node.

```llvm
@G = weak global i32 0   ; type of @G is i32*
@H = weak global i32 0   ; type of @H is i32*

define i32 @test(i1 %Condition) {
entry:
  %X = alloca i32           ; type of %X is i32*.
  br i1 %Condition, label %cond_true, label %cond_false

cond_true:
  %X.0 = load i32, i32* @G
  store i32 %X.0, i32* %X   ; Update X
  br label %cond_next

cond_false:
  %X.1 = load i32, i32* @H
  store i32 %X.1, i32* %X   ; Update X
  br label %cond_next

cond_next:
  %X.2 = load i32, i32* %X  ; Read X
  ret i32 %X.2
}
```

With this, we have discovered a way to handle arbitrary mutable variables without the need to create Phi nodes at all:

1. Each mutable variable becomes a stack allocation.
2. Each read of the variable becomes a load from the stack.
3. Each update of the variable becomes a store to the stack.
4. Tacking the address of a variale just use the stack address directly.

While this solution has solved our immediate problem, it introduced another one: we have now apparently introduced a lot of stack traffic for very simple and common operations, a major performance problem. Fortunately for us, the LLVM optimizer has a highly-tuned optimization pass named "mem2reg" that handles this case, promoting allocas like this into SSA registers, inserting Phi nodes as appropriate. If you run this example through the pass, for example, you'll get:

```llvm
$ llvm-as < example.ll | opt -passes=mem2reg | llvm-dis
@G = weak global i32 0
@H = weak global i32 0

define i32 @test(i1 %Condition) {
entry:
  br i1 %Condition, label %cond_true, label %cond_false

cond_true:
  %X.0 = load i32, i32* @G
  br label %cond_next

cond_false:
  %X.1 = load i32, i32* @H
  br label %cond_next

cond_next:
  %X.01 = phi i32 [ %X.1, %cond_false ], [ %X.0, %cond_true ]
  ret i32 %X.01
}
```

## Extending the Language: Mutable Variables

While Kaleidoscope is interesting as a functional language, this chapter will extend the language with mutable variables.

```llvm
ready> extern printd(x);
ready> Read extern:
declare double @printd(double)

ready> def binary : 1 (x y) y;
ready> Read function definition:
define double @"binary:"(double %x, double %y) {
entry:
  ret double %y
}

ready> def test(x) printd(x) : x = 4: printd(x);
ready> Read function definition:
define double @test(double %x) {
entry:
  %CallTemp = call double @printd(double %x)
  %binop = call double @"binary:"(double %CallTemp, double 4.000000e+00)
  %CallTemp4 = call double @printd(double 4.000000e+00)
  %binop5 = call double @"binary:"(double %binop, double %CallTemp4)
  ret double %binop5
}

ready> test(123);
ready> Read top level expression:
define double @__anon_expr() {
entry:
  %CallTemp = call double @test(double 1.230000e+02)
  ret double %CallTemp
}

123.000000
4.000000
Evaluated to 0.000000
```

When run, this example prints "123" and then "4", showing that we did catually mutate the value!

Alse we support user-defined local variable by keyword var.

```llvm
ready> var a = 2024 in 1;
ready> Read top level expression:
define double @__anon_expr() {
entry:
  ret double 1.000000e+00
}

Evaluated to 1.000000
```

## Compiling to Object Code

Notice that, it seems JIT need to be disable.

We can emit object file, when enter CTRL-D, will find it emit.

```llvm
ready> def average(x y) (x+y) * 0.5;
ready> Read function definition:
define double @average(double %x, double %y) {
entry:
  %y2 = alloca double, align 8
  %x1 = alloca double, align 8
  store double %x, ptr %x1, align 8
  store double %y, ptr %y2, align 8
  %x3 = load double, ptr %x1, align 8
  %y4 = load double, ptr %y2, align 8
  %AddTemp = fadd double %x3, %y4
  %MulTemp = fmul double %AddTemp, 5.000000e-01
  ret double %MulTemp
}

ready> ready> Wrote: output.o
```

Then write code in C++:

```cpp
// main.cpp
#include <iostream>

extern "C" {
double average(double, double);
}

int main() {
  std::cout << average(3, 4) << std::endl;
  return 0;
}
```

And compile lie this:

```bash
clang++ main.cpp output.o -o main
```

If run `./main`, we will see the result of 3.5. How amazing!!!
