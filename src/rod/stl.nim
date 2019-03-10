#~~
# the rod programming language
# copyright (C) iLiquid, 2019
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

proc numType(vm: var RodVM, env: var RodEnv) =
  var numT = newClass("Num")
  numT.addMethod("+") do (vm: var RodVM, env: var RodEnv):
    vm[0] = vm[0].numVal + vm[1].numVal
  numT.addMethod("-") do (vm: var RodVM, env: var RodEnv):
    vm[0] = vm[0].numVal - vm[1].numVal
  numT.addMethod("*") do (vm: var RodVM, env: var RodEnv):
    vm[0] = vm[0].numVal * vm[1].numVal
  numT.addMethod("/") do (vm: var RodVM, env: var RodEnv):
    vm[0] = vm[0].numVal / vm[1].numVal
  numT.addMethod("==") do (vm: var RodVM, env: var RodEnv):
    vm[0] = vm[0].numVal == vm[1].numVal
  numT.addMethod("!=") do (vm: var RodVM, env: var RodEnv):
    vm[0] = vm[0].numVal != vm[1].numVal
  numT.addMethod("<") do (vm: var RodVM, env: var RodEnv):
    vm[0] = vm[0].numVal < vm[1].numVal
  numT.addMethod(">") do (vm: var RodVM, env: var RodEnv):
    vm[0] = vm[0].numVal > vm[1].numVal
  numT.addMethod("<=") do (vm: var RodVM, env: var RodEnv):
    vm[0] = vm[0].numVal <= vm[1].numVal
  numT.addMethod(">=") do (vm: var RodVM, env: var RodEnv):
    vm[0] = vm[0].numVal >= vm[1].numVal
  env["Num"] = newObject(numT)

stl[rsBase] = proc (vm: var RodVM, env: var RodEnv) =
  env["println"] = println
  numType(vm, env)
