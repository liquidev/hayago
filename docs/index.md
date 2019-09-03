---
layout: page
title: Home
---

**rod** is a small, fast, embeddable, statically typed scripting language,
written in Nim. Its syntax is highly inspired by Nim itself, with a bit of C
mixed in.

Its main goals are:

- **Simplicity.** The language is purposefully simple, for easy learning.
  It also prevents bugs in the implementation.
- **Speed.** While rod isn't the fastest scripting language out there, it uses
  quite a few optimizations to make execution suitable for real-time
  applications, like games.
- **Easy embedding.** Embedding rod in your application is as simple as listing
  all the things you need to be available in the VM. It's as easy as it can get.
- **Portability.** rod avoids tricks like NaN tagging to make it suitable for
  use on a large variety of systems. It also avoids the use of any OS-specific
  APIs, making it run on any OS Nim supports.

```rod
var hello = "Hello, rod!"

object Greeter {
  target: string
}

proc newGreeter(target: string) -> Greeter {
  result = Greeter {
    target: target
  }
}

proc greet(greeter: Greeter) {
  echo("Hello, " & greeter.target & "!")
}

var worldGreeter = newGreeter("World")
worldGreeter.greet()
```

## Installing

Installing rod should be as simple as:

```
$ nimble install rod
```

No extra dependencies required. At all. You only need Nim 0.20.0.
