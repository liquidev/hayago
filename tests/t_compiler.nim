import unittest

import rod/[value, lexer, compiler, vm, std]

template debugCompile(rule: untyped, source: string): untyped =
  echo "--- vm ---"
  var vm = newRodVM()
  vm.registerStdlib()
  echo vm

  var cp = vm.newCompiler(tokenize(source))
  var chunk = newChunk()
  discard cp.rule(chunk)
  echo "--- chunk ---"
  chunk

suite "bytecode compilation":
  test "constants":
    echo $debugCompile(constant, "1")
  test "prefix ops":
    echo $debugCompile(prefixOp, "-~1")
