<p align="center">
  <img src="rod-logo-svg.svg" width="256px"/>
</p>

# rod

**rod** is a small, fast, embeddable, statically typed scripting language,
written in Nim. Its syntax is highly inspired by Nim itself, with a bit of C
mixed in.

| **Note** | rod is not finished yet. This readme is a draft of the language's goals. |
| --- | --- |

Its main goals are:

- **Simplicity.** The core feature set remains small, but powerful enough for
  usage in applications of varying complexities.
- **Soundness.** Unlike most scripting languages out there, rod has a static
  typing system instead of a dynamic one, which makes code more robust and
  maintainable. It has generics with type inference, minimizing code repetition
  and making refactoring easy.
- **Speed.** While rod isn't the fastest scripting language out there, it uses
  quite a few optimizations to make execution suitable for real-time
  applications, like games.
- **Easy embedding.** Embedding rod in your application is as simple as listing
  all the things you need to be available in the VM.

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

var message = "rod is a "
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

rod is not finished yet. The following checklist represents the current state of
affairs when it comes to features:

- rod 0.1 (currently worked on)
  - [x] variables
  - [x] flow control (`if`, `while`, `for`)
  - [x] objects
    - [ ] inheritance
    - [ ] non-`ref` and `ref` objects
  - [x] procedures
    - [ ] closures
    - [ ] UFCS
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
- rod 0.2
  - [ ] coroutines
  - [ ] error handling with a `try…except…finally`-like system
- rod 0.3
  - [ ] tuples
  - [ ] multiple `for` loop variables

There is no hard deadline for any of the listed features. This checklist is
supposed to show how much of rod is complete, but it probably misses some
points.

If you want to propose a new feature, feel free to open an issue. I'm open to
suggestions.
