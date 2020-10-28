import unittest

import hayago/private/scanner

template scan(input: string) =
  var
    scanner = initScanner(input, "testcase.rod")
    token: Token
  while true:
    token = scanner.next()
    echo token
    if token.kind == tokEnd:
      break

suite "literals":
  test "numbers":
    scan("42 3.14159")
  test "operators":
    scan("+ - * / = and or")
  test "idents":
    scan("hello helloWorld HelloWorld HELLO_WORLD hello_world")
  test "variables":
    scan("var x = 10")
    scan("let π = 3.14")
  test "blocks":
    scan("{ hello(world) }")
  test "flow control":
    scan("if x {} elif x {} else {}")
    scan("while true {}")
    scan("for x in y {}")
  test "object and proc":
    scan("object X { }")
    scan("proc x() -> number { }")
