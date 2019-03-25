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
  RodRange* = ref object
    min, max, index: float

template on(class, fn, body: untyped): untyped {.dirty.} =
  class.addMethod(fn) do (vm: var RodVM, env: var RodEnv):
    body

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

proc numTypes(vm: var RodVM, env: var RodEnv) =
  var numT = newClass("Num")
  numT.on("+"): vm[0] = vm[0].num + vm[1].num
  numT.on("-"): vm[0] = vm[0].num - vm[1].num
  numT.on("*"): vm[0] = vm[0].num * vm[1].num
  numT.on("/"): vm[0] = vm[0].num / vm[1].num
  numT.on("=="): vm[0] = vm[0].num == vm[1].num
  numT.on("!="): vm[0] = vm[0].num != vm[1].num
  numT.on("<"): vm[0] = vm[0].num < vm[1].num
  numT.on(">"): vm[0] = vm[0].num > vm[1].num
  numT.on("<="): vm[0] = vm[0].num <= vm[1].num
  numT.on(">="): vm[0] = vm[0].num >= vm[1].num
  numT.on(".."):
    vm[3] = true
    vm[0] = env["Range"].class.construct(vm)
  numT.on("..."):
    vm[3] = false
    vm[0] = env["Range"].class.construct(vm)
  env["Num"] = numT

  var rangeT = newClass("Range")
  rangeT.on(".ctor"):
    var rObj = vm[0].obj
    rObj.make(
      RodRange(
        min: vm[1].num, max: vm[2].num,
        index: 0
      )
    )
    vm[0] = rObj
  rangeT.on("has_next"):
    let r = vm[0].obj.get(RodRange)
    vm[0] = r.index >= r.max
  rangeT.on("get"):
    vm[0] = vm[0].obj.get(RodRange).index
  rangeT.on("next"):
    var r = vm[0].obj.get(RodRange)
    r.index += 1
    vm[0] = RodNull
  env["Range"] = rangeT

stl[rsBase] = proc (vm: var RodVM, env: var RodEnv) =
  env["println"] = println
  numTypes(vm, env)
