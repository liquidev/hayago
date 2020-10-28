import unittest

import hayago/private/ast
import hayago/private/scanner
import hayago/private/parser

template parse(input: string) =
  var scanner = initScanner(input, "testcase.hyo")
  echo parseScript(scanner).render

suite "parser":
  test "expressions":
    parse(r"2 * 2 + 2")
  test "statements":
    parse(r"var x, y, z = 2")
    parse(r"let x, y: number")
  test "blocks":
    parse("""
      { var x = 4
        let y = 2 }
    """)
  test "scripts":
    parse("""
      var x = 2
      var y: number
      { var z = 4 }
    """)
  test "if expressions":
    parse("""
      if true {
      } elif 2 {
      } elif 3 {
      } else {
      }
    """)
  test "while loops":
    parse("""
      while true {}
    """)
  test "for loops":
    parse("""
      for x in 1..10 {}
    """)
  test "objects":
    parse("""
      object Test {
        a: number
        b, c: string
        testing: obj
      }

      object Pair[T, U] {
        a: T
        b: U
      }
    """)
    parse("""
      var myVec = Vec2(x: 10, y: 20)
    """)
  test "procs":
    parse("""
      someProc("testing")
    """)
    parse("""
      var x: proc (a: string, b, c: number, d: bool) -> bool
    """)
    parse("""
      proc hello(a: string) -> string {
        echo("hello, " & a)
      }
      proc reverse[T](x: seq[T]) -> seq[T] {}
    """)
    parse("""
      onPrint(proc (text: string) {
        echo(text)
      })
    """)
  test "indexing":
    parse("""
      var x = a[2]
      var y = m[0][1]
    """)
  test "dot":
    parse("""
      var x = a.x
      var y = a.x[3]
      a.x = 10
    """)
  test "iterators":
    parse("""
      iterator countup(min, max: number) -> number {
        var i = min
        while i <= max {
          yield i
          i = i + 1
        }
      }
    """)

