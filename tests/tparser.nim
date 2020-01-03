import unittest

import rod/private/scanner
import rod/private/parser

template parse(input: string) =
  var scanner = initScanner(input, "testcase.rod")
  echo parseScript(scanner).treeRepr

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
