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
      let x = 5;
      println(x * 2 + 2);
      {
        let y = x * 2;
        println(y + 5);
      }
    """)
  test "if stmt":
    testRun("""
      let x = 2;
      let y = 3;
      if x < y {
        println("x (2) is less than y (3)");
      } else {
        println("the world is ending");
      }
    """)
  test "loop stmt":
    testRun("""
      let x = 0;
      loop {
        println(x);
        x = x + 1;
      } while x < 10;
    """)
