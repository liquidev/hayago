#--
# the rod scripting language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

## This module implements the rod standard library modules.

import chunk
import codegen
import value

proc modSystem*(script: Script): Module =
  ## Create and initialize the ``system`` module.
  result = newModule("system")
  result.initSystemTypes()
  script.addProc(result, "echo", {"text": "string"}, "void",
    proc (args: StackView): Value =
      echo args[0].stringVal)
  # script.addProc(result, "$", {"x": "bool"}, "string",
  #   proc (args: StackView): Value =
  #     result = initValue($args[0].boolVal))
  script.addProc(result, "$", {"x": "number"}, "string",
    proc (args: StackView): Value =
      result = initValue($args[0].numberVal))

