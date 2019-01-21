# rod

A fast, object-oriented, embeddable programming language for Nim,
with a modern, Rust-like syntax.

```rust
// main.rod
class Rod {
  pub fn say_hello(person) {
    println("Hello, {}!", person);
  }
}

Rod::say_hello("world");
```

### Features
 - **A fast VM.** rod uses bytecode instead of direct execution from AST, which
   makes it very fast.
 - **Easy-to-understand syntax.** The language's syntax is similar to Rust,
   but without a lot of the hard stuff – there's no lifetimes,
   it's dynamically typed, and it has classes.
 - **Source code protection.** It's possible to save the bytecode directly to a
   binary format for source code protection. Debugging symbols are stripped,
   and a separate symbol table must be used.
 - **Easy API.** rod is easily embeddable into other applications. Its FFI
   can automatically generate the necessary wrapper procedures for just about
   any object.
 - **Concurrency.** rod's concurrency is very lightweight. There aren't any
   threads, but that is a design choice with intentions of making the language
   much more simple.

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
println("Hello, World!");
```
```sh
$ rod hello.rod
Hello, World!
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
  println("Hello, World!");
""")
```
Output:
```
Hello, World!
```
Check the embedding guide for more details.
