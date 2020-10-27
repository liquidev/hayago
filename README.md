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
- **Speed.** It's in the name: _fast language_. While its speed doesn't match
  that of more complex languages with JIT compilers, design choices like a
  static object layout make hayago faster than a lot of embeddable scripting
  languages out there.
- **Easy embedding.** Embedding hayago in your application is as simple as
  listing all the things you need to be available in the VM.

Non-goals:

- **Compactness.** There already exist languages like Lua and Wren that fit that
  purpose much better. hayago does not try to be as compact as possible, but
  also doesn't require the embedding application to use _all_ the modules.
  It also doesn't try to implement a blazing-fast compiler, and doesn't strive
  to be usable on embedded systems.
- **Implementing Nim.** hayago can be thought of as a _companion_ to Nim, rather
  than Nim as a scripting language. While the syntax tries to remain familiar,
  certain changes were made to make hayago work better as a scripting language.

```nim
proc hello(target) =
  echo("Hello, " & target)

hello("Nim users")

let features = ["simple", "fast", "concurrent", "embeddable"]
for x in features:
  echo(x)
```

## Roadmap

hayago is not finished yet. The following checklist represents the current state
of affairs when it comes to features:

- hayago 0.1 (currently worked on)
  - [ ] variables
  - [ ] control flow (`if`, `while`, `for`)
  - [ ] objects
    - [ ] `impl`
    - [ ] `bycopy`
    - [ ] inheritance
  - [ ] procedures
    - [ ] closures
  - [ ] modules and `import`
  - [ ] embedding
    - [ ] low-level, unsafe functionality
    - [ ] high-level macro-based API
  - [ ] standard library
    - [ ] math
    - [ ] string manipulation
    - [ ] seq manipulation
    - …

There is no hard deadline for any of the listed features. This checklist is
supposed to show how much of the language is complete, but it probably misses
some points.

If you want to propose a new feature, feel free to open an issue. I'm open to
suggestions.

## Didn't this use to be called `rod`?

Yes. I decided to change the name to avoid conflicts with
[yglukhov/rod](https://github.com/yglukhov/rod), which is a much older project
than this. Also, _hayago_ just sounds so much nicer and friendlier, doesn't it?
