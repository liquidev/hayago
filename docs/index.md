---
layout: page
title: Home
---

**rod** is a small, fast, embeddable, statically typed scripting language,
written in Nim. Its syntax is highly inspired by Nim itself, with a bit of C
mixed in.

Its main goals are:

- **Simplicity.** The core feature set remains small, but powerful enough for
  usage in applications of all complexities.
- **Soundness.** Unlike most scripting languages out there, rod has a static
  typing system instead of a dynamic one, which makes code more robust and
  maintainable.
- **Speed.** While rod isn't the fastest scripting language out there, it uses
  quite a few optimizations to make execution suitable for real-time
  applications, like games.
- **Concurrency.** rod embraces Lua-like coroutines for lightweight and simple
  concurrency.
- **Easy embedding.** Embedding rod in your application is as simple as listing
  all the things you need to be available in the VM.

```nim
proc hello(target: string) {
  echo("Hello, " & target)
}

hello("Nim users")

coroutine allItems[T](list: seq[T]) -> T {
  for x in list {
    yield x
  }
}

let features = ["simple", "sound", "fast", "concurrent", "embeddable"]

var coro = allItems()
var message = "rod is a "
while not coro.done {
  message = message + coro(features)
  if not coro.done {
    message = message + ", "
  }
}
message = message + " scripting language"
echo(message)
```

## Installing

Installing rod should be as simple as:

```
$ nimble install rod
```

No extra dependencies required. At all. You only need Nim 0.20.0.
