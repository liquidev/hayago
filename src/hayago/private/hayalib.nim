#--
# the hayago scripting language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

## This module implements the hayago standard library modules.

import chunk
import codegen
import parser
import scanner
import sym
import value

proc compileHaya*(script: Script, module: Module, filename, code: string) =
  ## Compile some hayago code to the given script and module.
  ## Any generated toplevel code is discarded. This should only be used for
  ## declarations of hayago-side things, eg. iterators.
  var scanner = initScanner(code, filename)
  let ast = parseScript(scanner)
  var
    chunk = newChunk()
    gen = initCodeGen(script, module, chunk)
  gen.genScript(ast)

const
  HayalibSystemSrc* = """
    iterator `..`(min, max: int) -> int {
      var i = min
      while i <= max {
        yield i
        i = i + 1
      }
    }
  """

proc modSystem*(script: Script): Module =
  ## Create and initialize the ``system`` module.

  # foreign stuff
  result = newModule("system")
  result.initSystemTypes()
  script.initSystemOps(result)

  script.addProc(result, "echo", {"text": "string"}, "void",
    proc (args: StackView): Value =
      echo args[0].stringVal[])

  script.addProc(result, "$", {"x": "bool"}, "string",
    proc (args: StackView): Value =
      result = initValue($args[0].boolVal))

  script.addProc(result, "$", {"x": "int"}, "string",
    proc (args: StackView): Value =
      result = initValue($args[0].intVal))

  script.addProc(result, "$", {"x": "float"}, "string",
    proc (args: StackView): Value =
      result = initValue($args[0].floatVal))

  script.addProc(result, "$", {"x": "string"}, "string",
    proc (args: StackView): Value =
      result = initValue(args[0].stringVal[]))

  # native stuff
  script.compileHaya(result, "system.hyo", HayalibSystemSrc)

