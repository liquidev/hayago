import unittest

import ../src/rod/[
  chunk,
  scanner, parser, compiler,
  value, vm, stl
]
import utils

template testRun*(input: string): untyped =
  var chunk: RodChunk
  measureTime("compilation") do:
    var
      cp = newCompiler()
      scan = newScanner(input)
    chunk = newChunk()
    cp.compile(chunk, parseScript(scan))
    echo chunk
  measureTime("runtime") do:
    var
      env = newEnv()
      vm = newVM()
    vm.loadStdlib(env, rsBase)
    echo "interpret result:\n  " & $+vm.interpret(env, chunk)

suite "VM":
  test "calls":
    testRun("""
      println("Hello, World!");
    """)
