import times
import unittest

import rod/private/scanner
import rod/private/parser
import rod/private/chunk
import rod/private/codegen
import rod/private/disassembler
import rod/private/vm
import rod/private/rodlib

template benchmark(name, body) =
  let t0 = epochTime()
  body
  echo name, " took ", (epochTime() - t0) * 1000, "ms"

template dumpTokens(input: string) =
  var
    scanner = initScanner(input, "dump.rod")
    token: Token
  while true:
    token = scanner.next()
    echo token
    if token.kind == tokEnd:
      break

template run(input: string) =
  dumpTokens(input)
  benchmark("compilation"):
    var scanner = initScanner(input, "testcase.rod")
    let ast = parseScript(scanner)
    var
      # script
      main = newChunk()
      script = newScript(main)
      # modules
      system = script.modSystem()
      module = newModule("testcase")
      # codegen
      gen = initCodeGen(script, module, main)
    module.load(system)
    gen.genScript(ast)
  echo `$`(script, input)
  var vm = newVm()
  benchmark("runtime"):
    discard vm.interpret(script, main)

suite "VM":
  test "hello world":
    run("""
      echo("Hello, world!")
    """)
  test "variables":
    run("""
      var x = 2
      var y = 4
    """)
  test "if statements":
    run("""
      var x = true
      if x {
        echo("got true")
      }
      var y = false
      if y {
        echo("error")
      } elif not y {
        echo("success")
      }
      var z = 2
      if z < 0 {
        echo("negative")
      } elif z == 0 {
        echo("zero")
      } elif z > 0 {
        echo("positive")
      }
      echo("if statements done, no stack corruption")
    """)
  test "while loops":
    run("""
      var x = 0
      while x < 20 {
        echo(x)
        x = x + 1
      }
    """)

