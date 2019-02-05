import unittest

import rod/[value, lexer, parser, compiler, vm, std]

template debugCompile(source: string, rule: string): untyped {.dirty.} =
  echo "--- vm ---"
  var vm = newRodVM()
  vm.registerStdlib()
  echo vm

  var node = parse(tokenize(source), rule)

  var cp = vm.newCompiler()
  var module = newModule()
  var chunk = cp.compile(module, node)

  echo "--- chunk ---"
  echo chunk

suite "bytecode compilation":
  test "constants":
    debugCompile("1", "literal")
  test "prefix ops":
    debugCompile("-~1", "prefixOp")
  test "infix ops":
    debugCompile("2 * 3 + 5 * 4", "infixOp")
  test "variables":
    debugCompile("""
      let x = 2;
      x * 2;
    """, "script")
