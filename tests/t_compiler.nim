import unittest

import rod/[lexer, compiler, vm]

template debugCompile(rule: string, source: string): untyped =
  var vm = newRodVM()
  var cp = vm.newCompiler()
  cp.compile(tokenize(source), rule)

suite "bytecode compilation":
  test "constants":
    echo $debugCompile("constant", "1")
  test "prefix ops":
    echo $debugCompile("prefix-op", "!true")
