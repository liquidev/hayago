---
layout: page
title: Manual
permalink: /man
---

# Language documentation

## Syntax

As mentioned, rod's syntax is similar to Nim, so learning it is very easy for
existing Nim users. The only large difference is that rod uses braces instead of
indentation, to make parsing simpler.

### Comments

Comments in rod look exactly as in C++. Only single-line comments are supported.

```
// This is a comment
```

### Literals

rod supports ordinary number and string literals.

```
3.141592        // This is a number literal
"Hello, world!" // This is a string literal
```

As of now, only basic literals (as shown above) are supported. So no scientific,
binary, hexadecimal, or octal numbers, and no escape sequences in strings.
This is subject to change, though.

### Expressions

rod divides its syntax into expressions and statements. Expressions always have
a type, while statements do not (we denote the lack of a type as `void`).
Expressions in rod include literals, arithmetic and logic operations, both unary
and binary, calls, sequence and table indexing, object construction, and `if`.

#### Operator precedence

Unary operators always take precedence over binary operators. Unlike Nim, there
are *no exceptions to this rule*. In Nim the `@` operator binds stronger than
a primary suffix (that includes calls, dot syntax, array access, and such).
This rule does not apply in rod, to keep parsing simple. Binary operator
precedence follows [Nim's rules][nim-prec].

  [nim-prec]: https://nim-lang.org/docs/manual.html#syntax-precedence

### Statements

Statements in rod are pretty simple - they include all expressions, loops, and
declarations. Statements in rod are delimited with line breaks.

### Blocks

rod makes heavy use of blocks throughout the language's syntax, here's what they
look like:
```
{
  echo("Hello, world!")
}
```
Another, more compact style is also supported:
```
{ echo("Hello, world!")
  echo("Going compact!") } // notice how a line break isn't required here
```
The above syntax is an exception to the usual statement rules. In blocks, `}`
can also be used as a statement terminator.

## Variables

Variables are a very important part of any language. rod offers two ways of
declaring them:
```
var
  a = 2,
  b = 3
let
  x = 4,
  y = 5
```
The rules behind these two definition types are the same as in Nim: `var`
declares a regular variable, and `let` declares a single-assignment variable.
Attempting to assign to such a variable twice will result in a compile error:
```
let x = 2
x = 3 // error: attempt to reassign a 'let' variable
```
Even though `let` variables cannot be reassigned, that does not mean their value
cannot be changed: if their value is an object, its fields can be modified just
fine.
Variables in rod are statically typed. That means that a number variable will
always stay a number variable, until it goes out of scope. The type of a
variable cannot be changed.
```
var x = 3
x = "Hello, world!" // error: type mismatch, attempt to assign a string to a
                    // number variable
```
Currently there's no way of defining a variable without specifying a value,
but that's subject to change.

## Flow control

Flow control is achieved in rod through 3 basic constructs: `if`, `while`,
and `for`.

### `if` expression

In rod, `if` is an expression. That means it can be used in places like variable
values, proc parameters, etc.

The basic syntax of an `if` expression is like so:
```
if condition {
  // do things
} elif condition {
  // do things
} elif condition {
  // do things
} else {
  // do things
}
```
Each condition must be a boolean. If it's of a different type, an error is
raised.

An `if` expression executes all of its blocks sequentially, and stops whenever
one of the condition evaluates to `true`. If none of the expressions evaluate to
`true`, then the `else` branch is executed.

The type of an `if` expression is inferred by the following conditions:

- If the `if` expression is used in statement context, its type is not inferred
  from anything and is `void`.
- If the `if` expression is used in expression context, its type is the type of
  the last expression of the first block. If that type is `void`, an error is
  raised. All other blocks must have that same type (also inferred from the last
  statement of their corresponding blocks).

### `while` loop

A `while` loop is the simplest kind of loop. All it does is it executes its body
as long as its condition stays `true`.

The syntax of a `while` loop is like so:
```
while condition {
  // do things
}
```
An infinite loop may be created by setting the condition to a `true` constant.
```
while true {
  // do things indefinitely
}
```

A `while` loop can be stopped by using the `break` keyword.
```
var x = 0
while true {
  x = x + 1
  if x > 10 {
    break
  }
}
```
The `continue` keyword will cause the loop to jump back to the beginning of
the block.
```
// List even numbers from 0 to 100
var x = -1
while x <= 100 {
  x = x + 1
  if x mod 2 == 0 {
    continue
  }
  echo($x)
}
```

### `for` loop

The `for` loop is an advanced version of the `while` loop. Instead of iterating
as long as a condition is met, it uses *iterators*.
```
for variable in iterator {
  // do things
}
```

`for` loops support `break` and `continue`, just like `while` loops do.

rod currently doesn't have a way of defining custom iterators, but a few are
provided in its standard library. First of all, the range iterators:

```
// displays numbers from 0 to 100
for i in 0..100 {
  echo(i)
}
```

This is the inclusive range operator. It iterates over all numbers in the given
range, here from 0 to 100.

```
for i in 0..<100 {
  echo(i)
}
```

This exclusive range iterator is similar. The only difference is that it skips
the last number in the range, so the above example will print numbers
from 0 to 99 instead of 0 to 100.

Two more range iterators are available:

- `countup(min, max, step)` – counts numbers from `min` to `max`, skipping over
  `step` of them at a time. `step` may be any number, it doesn't have to be an
  integer. It *must* however be a positive number; otherwise an exception is
  raised.
- `countdown(max, min, step)` – the same as `countup`, but counts in the
  opposite direction. `step` must also be a positive number.

More iterators will be described later.

## Procedures

TODO: Procedures
