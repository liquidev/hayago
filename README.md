<p align="center">
  <img src="rod-logo-svg.svg" width="256px"/>
</p>

# rod

A fast, object-oriented, embeddable programming language for Nim,
with a modern, Rust-like syntax.

```rust
// main.rod
class Rod {
  fn say_hello(target) {
    println(f"Hello, {target}!")
  }

  export {
    say_hello(1)
  }
}

Rod::say_hello("world")
```

| **Note** | rod is not finished yet. This readme is a draft of the language's goals. |

### Features
 - **A fast VM.** rod uses a small, yet efficient instruction set for its VM,
   making code execution very fast.
 - **Easy-to-understand syntax.** The language's syntax is similar to Rust,
   but without a lot of the hard stuff – there's no lifetimes,
   it's dynamically typed, and it has classes.
 - **Easy API.** rod is easily embeddable into other applications. Its FFI
   can automatically generate the necessary wrappers for just about any object,
   enum, or procedure.
 - **Concurrency.** rod's concurrency is very lightweight. It uses fibers to
   achieve fast, yet powerful parallelization, without giving up thread-safety.

### Running rod
To run rod, compile it from source code:
```sh
$ git clone https://github.com/liquid600pgm/rod
$ nimble compile-exec
```
An executable file should appear in the `src/rod` directory. Add it to your
`PATH` and try to execute some code:
```rust
// hello.rod
println("Hello, World!")
```
```sh
$ rod hello.rod
Hello, World!
```
rod can also be run in REPL mode:
```rust
$ rod
r~ > println("Hello, World!")
Hello, World!
   < null
```
For more details, check the language guide.

### Embedding
To start with embedding rod, install the nimble package:
```sh
$ nimble install rod
```
Then, import rod, create a new VM and execute some code:
```nim
import rod

var vm = newRodVM()

vm.interpret("""
  println("Hello, World!")
""")
```
Output:
```
Hello, World!
```
Check the embedding guide for more details.

### Examples

```rust
// A simple hello world
println("Hello, World!"); // semicolons are optional
```

```rust
// Variables
let x = 10;
let y = x + 5;
assert(y == 15);
x += 2;
assert(x == 12);
assert(y == 15); // variables are not lazily evaluated

// constants:
let const z = x + y;
z = 10; // error: constants cannot be reassigned
```

```rust
// Strings
println("Hello, World!");

// interpolation:
let world = "World";
println(f"Hello, {world}!");

// escape sequences:
println("Hello,\nWorld!");

// multiline:
println("Hello,
World!");

// raw literals:
println(r"C:\Windows\System32"); // escape sequences are not interpreted
```

```rust
// Number literals
// all numbers in rod are Nim `float`s
let int = 32;
let dec = 3.14159;
let hex = 0x30d10d;
let bin = 0b10100001;
let oct = 0o777;
```

```rust
// Functions

fn f(x) {
  // implicit return, only allowed at the end of a function
  // an implicit return **must not** have a semicolon
  x + 2
}

fn g(y) {
  // explicit return, can be used anywhere in a function
  return x + 5;
}

assert(f(1) == 3);
println(f"f(10) = {f(10)}");
```

```rust
// Flow control
fn cond(x) {
  x > 3
}

if cond(3) {
  println("This will never execute");
} else {
  println("This will execute instead");
}

let i = 10;
while cond(i) {
  println(f"{i}");
  --i; // there are no postfix operators in rod
}

loop {
  ++i;

  if i >= 20 { break; }
}

// loop...while is equivalent to a do...while in other languages
loop {
  ++i;
} while i < 100;

for i in 1..10 {
  println(f"{i}"); // prints 1 through 10
}
// rod doesn't have a primitive for loop, only an iterator-based one

let list = [2, 5, 4, 10, 30];
let sum = accum x in list into y { // universal comprehensions
  y += x;
}
```

```rust
// Types

// a struct can be declared like so:
struct SimpleVector(x, y);

let position = SimpleVector(10, 20);
// structs have an implicit Display trait implementation:
println(f"position = {position}");

// structs cannot have their own methods, but classes can:
class Vector {
  let x, y;

  // constructors
  fn .ctor(self, x, y) {
    self.x = x;
    self.y = y;
  }

  // instance methods
  fn add(self, other) {
    return Self(self.x + other.x, self.y + other.y);
  }

  fn sub(self, other) {
    return Self(self.x - other.x, self.y - other.y);
  }

  // operator overloading
  // overloaded operators cannot have a `self` argument because they are not
  // instance methods
  fn a + b {
    return a.add(b);
  }

  fn a - b {
    return a.sub(b);
  }

  // an export block makes certain fields and methods public
  export {
    x, y,
    // methods are exported with an argument count
    // (or without, if all overloads are to be exported)
    +(2), -()
  }
}

let point_a = Vector(20, 10);
let point_b = Vector(-30, 10);
let point_c = point_a + point_b; // use the overloaded operator
println(f"c = ({point_c.x}, {point_c.y})");

trait Hello {
  fn say_hello(target);

  export {
    say_hello(1)
  }
}

class Greeter {}

impl Hello for Greeter {
  fn say_hello(target) {
    println(f"Hello, {target}!");
  }
}
```
