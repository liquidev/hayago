---
layout: page
title: Manual
permalink: /manual
---

# Language documentation

#### Note

This documentation is still unfinished, and will grow with new language features
being added.

## Syntax

As mentioned, rod's syntax is similar to Nim, so learning it is very easy for
existing Nim users. The only major difference is that rod uses braces instead of
indentation, to make parsing simpler.

### Comments

Comments in rod look exactly as in C++. Only single-line comments are supported.

```rod
// This is a comment
```

### Literals

rod supports ordinary number and string literals.

```rod
3.141592         // This is a number literal
"Hello, world!"  // This is a string literal
```

As of now, only basic literals (as shown above) are supported. So no scientific,
binary, hexadecimal, or octal numbers, and no escape sequences in strings.
This is subject to change, though.

### Identifiers

An identifier starts with a character from the set
`{'a'..'z', 'A'..'Z', '_', '\x7f'..'\xff'}` and continues with 0 or more
characters from the set `{'a'..'z', 'A'..'Z', '0'..'9', '_', '\x7f'..'\xff'}`.

Arbitrary sequences of characters (including reserved keywords) may be used as
identifiers provided that they're *stropped*. Stropping a sequence of characters
is done using backticks '\`'. Example:
```rod
var `if` = 0
```
Keep in mind that whitespace is ignored inside a stropped identifier, so this:
```rod
var `hello world` = 1
```
is the same as this:
```rod
var helloworld = 1
```
Stropping is rarely required though and should only be used in very specific
cases described later in the manual. Using stropped identifiers where it's not
required is bad style.

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
```rod
{
  echo("Hello, world!")
}
```
Another, more compact style is also supported:
```rod
{ echo("Hello, world!")
  echo("Going compact!") }  // notice how a line break isn't required here
```
The above syntax is an exception to the usual statement rules. In blocks, `}`
can also be used as a statement terminator.

## Variables

Variables are a very important part of any language. rod offers two ways of
declaring them:
```rod
// using var:
var a, b = 2      // both a and b have the value 2
var c, d: string  // both c and d have the type string with its default value ""
// using let:
let x, y = 5
// let z, w: bool - error: 'let' variables must have a value.
```
The rules behind these two definition types are the same as in Nim: `var`
declares a regular variable, and `let` declares a single-assignment variable.
Attempting to assign to such a variable twice will result in a compile error:
```rod
let x = 2
x = 3      // error: attempt to reassign a 'let' variable
```
Even though `let` variables cannot be reassigned, that does not mean their value
cannot be changed: if their value is an object, its fields can be modified just
fine.

Variables in rod are statically typed. That means that a number variable will
always stay a number variable, until it goes out of scope. The type of a
variable cannot be changed.
```rod
var x = 3
x = "Hello, world!"  // error: type mismatch, attempt to assign a string to a
                     // number variable
```
Currently there's no way of defining a variable without specifying a value,
but that's subject to change.

## Flow control

Flow control is achieved in rod through 3 basic constructs: `if`, `while`,
and `for`.

### `if` expression

In rod, `if` is an expression. That means it can be used in places like variable
values, proc arguments, etc.

The basic syntax of an `if` expression is like so:
```rod
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

Here's an example of the `if` expression in action:
```rod
let respHi = 0
let respBye = 1
let respNone = 2

let in = readInputFromUser()
var response = respNone

if in == "hi" { response = respHi }
elif in == "bye" { response = respBye }

echo(if response == respHi { "Hello!" }
     elif response == respBye { "Goodbye!" }
     else { "..." })
```

The type of an `if` expression is inferred by the following conditions:

- If the `if` expression is used in statement context, its type is not inferred
  from anything and is `void`.
- If the `if` expression is used in expression context, its type is the type of
  the last expression of the first block. If that type is `void`, an error is
  raised. All other blocks must have that same type (also inferred from the last
  statement of their corresponding blocks).

#### `and` and `or` operators

These two operators are special, because they're *short-circuiting*. That means
if one of their operands makes the result 'obvious', the other operand will not
be evaluated. Example:

```rod
proc a() -> bool {
  echo("a")
  result = true
}

proc b() -> bool {
  echo("b")
  result = false
}

a() or b()
```
Output:
```
a
```
As you can see, the second operand is not evaluated, because if the first
operand is `true`, we know that the result will always be `true`, according to
the truth table of the `OR` logic operation:

| A | B | Output |
| --- | --- | --- |
| 0 | 0 | 0 |
| 0 | 1 | 1 |
| 1 | 0 | 1 |
| 1 | 1 | 1 |

A similar thing happens with `and`: if the first operand evaluates to `false`,
the second operand will not be evaluated, because we know that if one operand of
the `AND` logic operation is `false`, the output will always be `false`.

| A | B | Output |
| --- | --- | --- |
| 0 | 0 | 0 |
| 0 | 1 | 0 |
| 1 | 0 | 0 |
| 1 | 1 | 1 |


### `while` loop

A `while` loop is the simplest kind of loop. All it does is it executes its body
as long as its condition stays `true`.

The syntax of a `while` loop is like so:
```rod
while condition {
  // do things
}
```
An infinite loop may be created by setting the condition to a `true` constant.
This literal `while true` loop is optimized by the code generator, and does not
do the initial condition check—it simply jumps back to the beginning when the
block is completed.
```rod
while true {
  // do things indefinitely
}
```
Similarly, a `while false` loop generates no code at all.

A `while` loop can be stopped by using the `break` keyword.
```rod
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
```rod
// List even numbers from 0 to 100
var x = -1
while x <= 100 {
  x = x + 1
  if x mod 2 == 1 {
    continue
  }
  echo($x)
}
```

### `for` loop

The `for` loop is an advanced version of the `while` loop. Instead of iterating
as long as a condition is met, it uses *iterators*.
```rod
for variable in iterator {
  // do things
}
```

`for` loops support `break` and `continue`, just like `while` loops do.

Iterators are described later in the manual.

### Implicit `items`

If a `for` loop has exactly one variable, and the for loop's expression `e` is
not an iterator, the expression is rewritten to `items(e)`. For example, the
following two loops are equivalent:
```rod
for x in [1, 2, 3] { echo(x) }        // prints 1.0, 2.0, 3.0
for x in items([1, 2, 3]) { echo(x) } // also prints 1.0, 2.0, 3.0
```

## Objects

Objects are homogenous containers of other values. They can contain any set of
values, and they can even form recursive data structures.

An object is declared like so:
```rod
object MyObject {
  a: string
  b: number
  x, y: bool
}
```
Objects are constructed using the following syntax:
```rod
var myObj = MyObject(a: "test", b: 3.1415926, x: true, y: false)
```
All fields of an object must be initialized to a value, although this is subject
to change.

Fields in objects can be read back from by using the dot syntax:
```rod
echo(myObj.a) // output: test
```
Fields can also be assigned to:
```rod
myObj.b = 42
echo($myObj.b) // output: 42
```

Note that even if you create a `let` variable with an object value, you can
still write to that object's fields, because the object itself is mutable. Only
the variable cannot be reassigned.
```rod
let myObj = MyObject(a: "test", b: 3.1415926, x: true, y: false)
myObj.x = false // this is legit
```

## Procedures

Procedures in rod are what other languages call 'functions'. Each procedure has
a name, arguments, and an optional return type.
In rod, procedures are declared like so:
```rod
proc myProcedure(arg1, arg2: string, arg3: number) -> string {
  // do things
}
```

There are a few things to note here:
- The procedure's arguments have what's called *type propagation*, which you can
  notice with `arg1` and `arg2`. It allows you to declare multiple arguments
  with a common type without having to repeat that type.
- Unlike Nim, rod uses `->` for specifying the return type. It works better with
  its brace-based syntax.

The return type of the procedure can be omitted. This will make the return type
`void`.
```rod
proc myProcedure() {
  // do things
}
```

Procedure arguments are not assignable. That means it's an error to do this:
```rod
proc myProcedure(x: number) {
  x = 2 // error!
}
```

To return a value from a procedure, the `return` statement is used. It halts the
execution of the procedure immediately, and returns the associated value (which
*can* be empty, in that case, the value returned is the value of the `result`
variable, which is described below).
```rod
proc theAnswer() -> number {
  return 42
}
```
It's important to note that the return statement is *always* the last statement
in a block. This means that this:
```rod
proc doSomeCalculations() -> number {
  return 2
  -42.sin
}
```
Is equivalent to this:
```rod
proc doSomeCalculations() -> number {
  return 2 - 42.sin
}
```

rod procedures can be called in 3 different ways:
- With regular call syntax – `someProc(arg1, arg2, ...)`
- With method call syntax – `arg1.someProc(arg2, ...)`
- With getter syntax – `arg1.someProc`

This allows for extra clarity in object-oriented code.
The two first examples are functionally equivalent. The third example is not
the same, because this syntax can only be used for calling procs which accept 1
parameter.

### The `result` variable

`result` is a special, implicitly declared variable present in all `proc`s with
a non-`void` return type. It is a convenience feature which helps avoid
unnecessary temporary variable declarations:
```rod
// without result
proc fac(n: number) -> number {
  var r = 1
  for i in 1..n {
    r = r * i
  }
  return r
}

// with result
proc fac(n: number) -> number {
  result = 1
  for i in 1..n {
    result = result * i
  }
}
```
Note how when we use `result` we don't need an extra `return` to actually
return the result of our operation. That is the main purpose of `result`: if
an accumulative operation is being done (eg. calculating the factorial of some
number), one can avoid an extra `return` statement at the end of the procedure.
In fact, `result` should be preferred over `return` whenever its flow control
capabilities are not required.

The initial value of `result` is dependent on the return type of the procedure.
It is always the default value for that type (eg. `0` for numbers).

### Setters

There's also another way of calling procs: that way is through assignment.
Only 'setters' can be called this way. A setter is declared by adding `=` to the
proc's name. Because `=` is not an identifier character, the name must be
stropped:
```rod
proc `someProperty=`(a: number, b: number) {
  // do things
}
```
Setters must always have two arguments. They can be called using the following
syntax:
```rod
a.someProperty = b
```
In a nutshell, they look exactly like an object field assignment. However, a
proc is called instead, and property setters can be declared for non-object
types like `number`s (albeit it's a bit useless in that case).

Object field assigmnents take precedence over setters:
```rod
object Vec2 {
  x, y: number
}

var myVec = Vec2(x: 1, y: 2)
myVec.x = 3 // sets the field directly

proc `x=`(vec: Vec2, val: number) {
  vec.x = val
}

myVec.x = 4 // also sets the field directly
```
To avoid this, the field must be declared with a different name. The idiomatic
way is to prefix the field with `f`:
```rod
object Vec2 {
  fX, fY: number
}

var myVec = Vec2(x: 1, y: 2)
myVec.fX = 3 // sets the field directly

proc `x=`(vec: Vec2, val: number) {
  vec.fX = val
}

myVec.x = 4 // calls the setter
```

### Operator overloading

All valid rod operators can be overloaded. Currently, this only includes
built-in operators, but support for custom operators is planned.

To overload an operator, simply name your proc with it:
```rod
object Vec2 {
  x, y: number
}

proc `+`(a, b: Vec2) -> Vec2 {
  result = Vec2(x: a.x + b.x, y: a.y + b.y)
}

var a = Vec2(x: 3, y: 2)
var b = Vec2(x: 2, y: 3)
var c = a + b             // Vec2(x: 5, y: 5)
```
As shown above, this feature is most useful with mathematical types, like
vectors.

Overloaded unary operators accept one parameter, and binary operators accept
two parameters. `not` and `$` are unary-only operators.

## Iterators

Iterators are what rod uses for executing `for` loops. An iterator is defined
like so:
```rod
iterator emptyIter() -> number {
  // body
}
```
An iterator *must* have a yield type. An iterator with no yield type is a
compile error.

Iterators have a special statement that can be used in them: the `yield`
statement. This statement makes the iterator "return" a value. Unlike a regular
`return`, however, `yield` does not stop the iterator. It simply passes the
execution back to the calling `for` loop.
```rod
iterator count3 -> number {
  yield 1
  yield 2
  yield 3
}

for x in count3() {
  echo(x) // outputs 1.0, 2.0, 3.0
}
```

Iterators are actually nothing more than inlining facilities. You can imagine
each `yield` statement in the iterator simply getting substituted by the for
loop's body. Continuing the previous example, the loop used there is actually
*roughly* equivalent to the following code:
```rod
{ // the iterator's variables
  let x = 1
  // the loop's body
  { echo(x) } }
{ let x = 2
  { echo(x) } }
{ let x = 3
  { echo(x) } }
```

Iterators have full scope hygiene, so the following example will fail:
```rod
// this is the implementation of countup in the standard library
iterator countup(min, max: number) -> number {
  var i = min
  while i <= max {
    yield i
  }
}

for x in countup(1, 10) {
  echo(i)  // error: 'i' is not defined
}
```
The scopes defined in the iterator are actually completely separate from the
scopes in the iterator's callsite.

## Generics

Generics are rod's way of generalizing code. They work similarly to C++
templates or Nim generics.

Valid symbols for being generic are types and callables (procedures and
iterators). Making a symbol generic involves adding square brackets with
*generic parameters* after the symbol's name:

```rod
object MyGenericObject[T] {}

proc myGenericProc[T] {}

iterator myGenericIterator[T] -> number {}
```

After definition, generic parameters behave like normal types: they can be
used in fields (for objects), or parameters, the return type, and the body (for
callables). Example:

```rod
object Pair[T] {
  a, b: T  // creates a new field of an unknown type `T`
}

proc print[T](a: T) {
  echo($a)
}
```

Generic symbols cannot be used by themselves. Instead, they must be
*instantiated*. Continuing the previous example, instantiations occur through
the use of square brackets:

```rod
let nums = Pair[number](a: 1, b: 2)

print[number](2)
```

However, this quickly gets verbose, so rod offers *basic* generic type inference
from call parameters and fields:

```rod
let nums = Pair(a: 1, b: 2)
//  ^ instantiated as Pair[number], because the provided value for `a: T`
//    is a `number`

print(2)
// ^ instantiated as print[number], because the provided value for `a: T`
//   is a `number`
```

Note that type inference does not work for return types. In that case, the type
must be provided explicitly:

```rod
object Box[T] {
  boxed: T
}

proc newEmptyBox[T] -> Box[T] {}

let numbox1 = newEmptyBox[number]()          // ok
// let numbox2 = newEmptyBox()               // error
// let numbox3: Box[number] = newEmptyBox()  // error
```

This is not allowed to keep the language implementation simple. Advanced type
inference like this would require much more context (ie. the call's
surroundings), and thus, increase code generation complexity by a fair bit.
