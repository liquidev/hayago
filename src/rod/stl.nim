#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

import value
import vm

type
  RodLib* = enum
    rsBase

var stl: array[low(RodLib)..high(RodLib),
  proc (vm: var RodVM, env: var RodEnv) {.nimcall.}]

proc loadStdlib*(vm: var RodVM, env: var RodEnv, lib: RodLib) =
  stl[lib](vm, env)

proc println(vm: var RodVM, env: var RodEnv) =
  var print = ""
  for i in 1..vm.slotAmt - 1:
    let v = vm[i]
    print.add($v)
    if i <= vm.slotAmt - 1:
      print.add(' ')
  echo print

stl[rsBase] = proc (vm: var RodVM, env: var RodEnv) =
  env["println"] = println
