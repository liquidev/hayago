import unittest

import ../src/rod/[
  chunk,
  compiler,
  parser,
  scanner
]

template testCompile*(parseFn: untyped, input: string): untyped =
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
    testCompile(Infix, "2 + 3 * 4")
