<p align="center">
  <img src="logo.svg" width="256px"/>
</p>

# hayago

**hayago** (早語, _fast language_, pronounced ha-ya-go) is a small, fast,
embeddable, statically typed scripting language, written in Nim. Its syntax is
inspired by Nim itself.

| **Note** | hayago is not finished yet. This readme is a draft of the language's goals. |
| --- | --- |

Its main goals are:

- **Simplicity.** The core feature set remains small, but powerful enough for
  usage in applications of varying complexities.
- **Soundness.** Unlike most scripting languages out there, hayago has a static
  typing system instead of a dynamic one, which makes code more robust and
  maintainable. It has generics with type inference, minimizing code repetition
  and making refactoring easy.
- **Speed.** It's in the name: _fast language_. While its speed doesn't match
  that of more complex languages with JIT compilers, static typing gives hayago
  a big advantage over other scripting languages.
- **Easy embedding.** Embedding hayago in your application is as simple as
  listing all the things you need to be available in the VM.

```nim
proc hello(target: string) {
  echo("Hello, " & target)
}

hello("Nim users")

iterator items[T](list: seq[T]) -> T {
  var len = list.len
  for i in 0..<len {
    yield list[i]
  }
}

let features = ["simple", "sound", "fast", "concurrent", "embeddable"]

var message = "hayago is a "
var i = 0
for x in features.items {
  message.add(x)
  if i != features.len - 1 {
    message.add(", ")
  }
  i = i + 1
}
message.add(" scripting language")
echo(message)
```

## Roadmap

hayago is not finished yet. The following checklist represents the current state
of affairs when it comes to features:

- hayago 0.1 (currently worked on)
  - [x] variables
  - [x] flow control (`if`, `while`, `for`)
  - [x] objects
    - [ ] inheritance
    - [ ] non-`ref` and `ref` objects
  - [x] procedures
    - [ ] closures
    - [x] UFCS
  - [x] iterators
  - [x] generics
    - generic type inference
      - [x] in procedure calls
      - [ ] in object constructors
  - [ ] modules and `import`
  - [ ] embedding
    - [ ] low-level, unsafe functionality
    - [ ] high-level macro-based API
  - [ ] standard library
    - [ ] math
    - [ ] string manipulation
    - [ ] seq manipulation
    - …
- hayago 0.2
  - [ ] coroutines
  - [ ] error handling with a `try…except…finally`-like system
- hayago 0.3
  - [ ] tuples
  - [ ] multiple `for` loop variables

There is no hard deadline for any of the listed features. This checklist is
supposed to show how much of the language is complete, but it probably misses
some points.

If you want to propose a new feature, feel free to open an issue. I'm open to
suggestions.

## Didn't this use to be called `rod`?

Yes. I decided to change the name to avoid conflicts with
[yglukhov/rod](https://github.com/yglukhov/rod), which is a much older project
than this. Also, _hayago_ just sounds so much nicer and friendlier, doesn't it?
