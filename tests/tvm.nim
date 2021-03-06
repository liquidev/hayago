import segfaults
import tables
import times
import unittest

import hayago/private/ast
import hayago/private/scanner
import hayago/private/parser
import hayago/private/chunk
import hayago/private/codegen
import hayago/private/disassembler
import hayago/private/vm
import hayago/private/hayalib
import hayago/private/sym

template benchmark(name, body) =
  let t0 = epochTime()
  body
  echo name, " took ", (epochTime() - t0) * 1000, "ms"

template run(input: string) =
  benchmark("compilation"):
    var scanner = initScanner(input, "testcase.hyo")
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
  when defined(dumpModule):
    echo module
  when defined(dumpDisassembly):
    echo `$`(script, {
      "system.hyo": HayalibSystemSrc,
      "testcase.hyo": input,
    }.toTable)
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
      let y = 2
      if y < 0 { echo("negative") }
      elif y == 0 { echo("zero") }
      elif y > 0 { echo("positive") }
    """)
    run("""
      if false {
        echo("leak test")
      }
    """)
    run("""
      if false {}
      else {}
    """)
  test "while loops":
    run("""
      var x = 1
      while x <= 20 {
        echo($x)
        x = x + 1
      }
    """)
    run("""
      var x = 0
      while true {
        x = x + 1
        if x == 2 {
          continue
        }
        echo($x)
        if x == 10 {
          break
        }
      }
    """)
  test "objects":
    run("""
      object Vector {
        x, y: float
      }

      var a = Vector(x: 1.0, y: 2.0)
      echo($a.x)
      echo($a.y)

      a.x = 3.0
      a.y = 4.0
      echo($a.x)
      echo($a.y)
    """)
  test "procedures":
    run("""
      proc fac(n: int) -> int {
        result = 1
        var i = 1
        while i <= n {
          result = result * i
          i = i + 1
        }
      }

      echo($fac(10))
    """)
    run("""
      proc printNums(x, max: int) {
        echo($x)
        if x < max { printNums(x + 1, max) }
      }
      printNums(0, 10)
    """)
  test "iterators":
    run("""
      for x in 1..10 {
        echo($x)
      }
    """)
    try:
      run("""
        for x in 10..15 {
          echo($i)
        }
      """)
      assert false, "did not catch error; 'i' is visible"
    except HayaCompileError as err:
      echo "pass - ", err.msg
    run("""
      for x in 1..10 {
        echo($x)
        if x == 5 {
          break
        }
      }
    """)
    run("""
      for x in 1..5 {
        if x == 2 { continue }
        echo($x)
      }
    """)
  test "UFCS":
    run("""
      object Stuff {
        fThing: int
      }

      proc thing(s: Stuff) -> int {
        result = s.fThing
      }

      proc `thing=`(s: Stuff, n: int) {
        s.fThing = n
      }

      proc sayThing(s: Stuff) {
        echo($s.thing)
      }

      var stuff = Stuff(fThing: 1)
      stuff.sayThing()
      stuff.thing = 5
      stuff.sayThing()
    """)
