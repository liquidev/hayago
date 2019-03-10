import unittest

import ../src/rod/[
  chunk,
  compiler,
  parser,
  scanner
]
import utils

template testCompile*(parseFn: untyped, input: string): untyped =
  measureTime("compilation") do:
    var
      cp = newCompiler()
      chunk = newChunk()
      scan = newScanner(input)
    cp.compile(chunk, `parse parseFn`(scan))
    echo chunk

suite "compiler":
  test "literals":
    testCompile(Literal, "2")
  test "prefix operations":
    testCompile(Prefix, "-5")
  test "infix operations":
    testCompile(Expr, "2 + 3 * 4")
  test "variables":
    testCompile(Expr, "(a + b) * h / 2")
    testCompile(Script, """
      let x = 10;
      let y = x + 2;
      {
        let x = x + y * 2;
      }
    """)
  test "flow control":
    testCompile(Script, """
      let x = true;
      let y = 2;
      if x {
        println("Hello, World!");
      } else if y == 2 {
        println("Hello, Nim!");
      } else {
        println("No hello for you");
      }
    """)
  test "scripts":
    testCompile(Script, """
      print(2 + 2);
    """)
