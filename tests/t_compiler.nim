import unittest

import rod/[value, lexer, compiler, vm, std]

template debugCompile(source: string,
                      compile: untyped): untyped {.dirty.} =
  echo "--- vm ---"
  var vm = newRodVM()
  vm.registerStdlib()
  echo vm

  var cp = vm.newCompiler(tokenize(source))
  var module = newModule()
  var chunk = module.newChunk()
  discard compile
  echo "--- chunk ---"
  echo chunk

suite "bytecode compilation":
  test "constants":
    debugCompile("1"): cp.constant(chunk)
  test "prefix ops":
    debugCompile("-~1"): cp.prefixOp(chunk)
  test "infix ops":
    debugCompile("2 * 3 + 5 * 4"): cp.infixOp9(chunk)
