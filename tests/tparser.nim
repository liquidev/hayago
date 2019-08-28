import unittest

import rod/private/scanner
import rod/private/parser

template parse(input: string) =
  var scanner = initScanner(input, "testcase.rod")
  echo parseScript(scanner)

suite "parser":
  test "expressions":
    parse(r"2 * 2 + 2")
  test "statements":
    parse(r"var x = 2, y = 3, z = 4")
    parse(r"let x = 3, y = 4, z = 5")
  test "blocks":
    parse("""
      { var x = 4
        let y = 2 }
    """)
  test "scripts":
    parse("""
      var x = 2, y = 3
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
    """)
