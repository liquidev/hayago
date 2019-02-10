import unittest

import rod/[value, parser, compiler, vm, std]

template debugCompile(source: string, rule: string): untyped {.dirty.} =
  echo "--- vm ---"
  var vm = newRodVM()
  vm.registerStdlib()
  echo vm

  echo "--- AST ---"
  var node = parse(source, rule)
  echo node.toLispStr(false)

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
      let mut x = 2;
      x * 2;
      x = 3;
      println(x);
    """, "script")
