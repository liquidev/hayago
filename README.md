<p align="center">
  <img src="rod-logo-svg.svg" width="256px"/>
</p>

# rod

A fast, object-oriented, statically typed embeddable scripting language for Nim,
with a modern syntax.

```rust
// main.rod
class Rod {
  pub fn say_hello(target) {
    echo(f"Hello, {target}!")
  }
}

Rod.say_hello("world")
```

| **Note** | rod is not finished yet. This readme is a draft of the language's goals. |
| --- | --- |

### Features
 - **A fast VM.** rod uses a register-based VM, which makes it much faster than
   classic stack-based VMs.
 - **Easy-to-understand syntax.** The language's syntax is a mix between Rust,
   Go, and Nim, with a bit of its own uniqueness, making for a very pleasant
   typing experience.
 - **Easy API.** rod is easily embeddable into other applications. Its FFI
   can automatically generate the necessary wrappers for just about any object,
   enum, or procedure.
 - **Concurrency.** rod's concurrency is very lightweight. All rod functions can
   stop and start anywhere, making for an efficient, concurrent language.
   Even `for` loops use them, making it very easy to write iterators.

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
echo("Hello, World!")
```
```
$ rod hello.rod
Hello, World!
```
rod can also run in a REPL:
```rust
$ rod
r~> echo("Hello, World!")
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
  echo("Hello, World!")
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
echo("Hello, World!")
```

```rust
// Variables
x := 10
y := x + 5
assert(y == 15)
x = x + 2
assert(x == 12)
assert(y == 15) // variables are not lazily evaluated

// constants:
z ::= x + y
z = 10 // error: constants cannot be reassigned
```

```rust
// Strings
echo("Hello, World!")

// interpolation:
world := "World"
echo(f"Hello, {world}!")

// escape sequences:
echo("Hello,\nWorld!")

// multiline:
echo("Hello,
World!")

// raw literals:
echo(r"C:\Windows\System32") // escape sequences are not interpreted
```

```rust
// Number literals
// all numbers in rod are Nim `float`s
int := 32
dec := 3.14159
hex := 0x30d10d
bin := 0b10100001
oct := 0o777
```

```rust
// Flow control
fn cond(x: num) bool {
  x > 3
}

if cond(3) {
  echo("This will never execute")
} else {
  echo("This will execute instead")
}

i := 10
while cond(i) {
  echo(f"{i}")
  --i // there are no postfix operators in rod
}

loop {
  ++i

  if i >= 20 { break }
}

// loop...while is equivalent to a do...while in other languages
loop {
  ++i
} while i < 100

// rod doesn't have a primitive for loop, only an iterator-based one
// if you need a primitive for, use while or loop...while
for i in 1..10 {
  echo(f"{i}") // prints 1 through 10
}

// comprehensions
sequence := ["hello", true, 3.141592]
str_seq := [
  sequence |x| { x.to_str() }
]
echo(strs) // ["hello", "true", "3.141592"]
table := { hello: "world", fun: true, pi: 3.141592 }
str_tab := {
  table |k, v| { (k, v.to_str()) }
}
echo(str_tab) // { "hello": "world", "fun": "true", "pi": "3.141592" }
// comprehensions are syntactic sugar over do blocks:
str_seq_raw := do {
  new_seq := sequence.__class() // calling a class constructs an instance of it
  for x in sequence {
    new_seq.add(x.to_str())
  }
  new_seq
}
```

```rust
// Functions

fn f(x: num) num {
  // implicit return, only allowed at the end of a function
  // an implicit return **must not** have a semicolon
  x + 2
}

fn g(y: num) num {
  // explicit return, can be used anywhere in a function
  return x + 5
}

assert(f(1) == 3)
echo(f"f(10) = {f(10)}")
```

```rust
// Types

// a struct can be declared like so:
struct SimpleVector(x, y)

position := SimpleVector(10, 20)
// structs have an implicit Display trait implementation:
echo(f"position = {position}")

// structs cannot have their own methods, but classes can:
class Vector {
  pub x, y: num

  // constructors
  pub fn new(self, x, y: num) [ctor] {
    self.x = x
    self.y = y
  }

  // instance methods
  // methods without 'pub' are private, that means they're only accessible from
  // within the declaring class and its subclasses
  pub fn add(self, other: Vector) Vector {
    Self(self.x + other.x, self.y + other.y)
  }

  pub fn sub(self, other: Vector) Vector {
    Self(self.x - other.x, self.y - other.y)
  }

  // operator overloading
  // operators are expression aliases
  pub op +(a, b: Vector) = a.add(b)
  pub op -(a, b: Vector) = a.sub(b)
}

point_a := Vector(20, 10)
point_b := Vector(-30, 10)
point_c := point_a + point_b // use the overloaded operator
echo(f"c = ({point_c.x}, {point_c.y})")

// Traits

trait Hello {
  pub fn say_hello(target)
}

class Greeter {}

impl Hello for Greeter {
  fn say_hello(target) {
    echo(f"Hello, {target}!")
  }
}
```

```rust
// Modules
// As your scripts grow bigger, you might need to split them into multiple
// files. rod handles this gracefully:

// in file: strutils.rod
pub fn replace_all(string, src, replacement: str) str {
  result := string.copy()
  for i in 0..a.len() {
    ch := a[i]
    if ch == src[0] { result[i] = replacement[0] }
  }
  result
}

// in file: main.rod
use strutils::{replace_all}

echo(replace_all("he11o", "1", "l")) // → hello
```
