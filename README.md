<p align="center">
  <img src="rod-logo-svg.svg" width="256px"/>
</p>

# rod

A fast, small, statically typed embeddable scripting language for Nim,
with a modern syntax inspired by Nim itself.

| **Note** | rod is not finished yet. This readme is a draft of the language's goals. |
| --- | --- |

```nim,c
proc hello(target: string) -> string {
  result = "Hello, " & target & "!"
}

proc sayHello(target: string) {
  echo(target.hello)
}

sayHello("World")
```

```nim,c
object Vec2 {
  x, y: number
}

proc vec2(x, y: number) -> Vec2 {
  result = Vec2(x: x, y: y)
}

proc +(a, b: Vec2) -> Vec2 {
  result = vec2(a.x + b.x, y: a.y + b.y)
}

var
  a = vec2(10, 10),
  b = vec2(20, 30),
  c = a + b
```
