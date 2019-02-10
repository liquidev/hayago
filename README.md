<p align="center">
  <img src="rod-logo-svg.svg" width="256px"/>
</p>

# rod

A fast, object-oriented, embeddable programming language for Nim,
with a modern, Rust-like syntax.

```rust
// main.rod
class Rod {
  pub fn say_hello(target: str) {
    println("Hello, {}!", [target]);
  }
}

Rod::say_hello("world");
```

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
println("Hello, World!");
```
```sh
$ rod hello.rod
Hello, World!
```
rod can also be run in REPL mode:
```rust
$ rod
r~ > println("Hello, World!");
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
  println("Hello, World!");
""")
```
Output:
```
Hello, World!
```
Check the embedding guide for more details.
