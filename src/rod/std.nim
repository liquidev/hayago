#~~
# rod stdlib
# copyright (C) iLiquid, 2018
#~~

## The standard library of rod is implemented in Nim, for better performance. \
## It's very lightweight, and doesn't have any unnecessary functions.
## The stdlib is loaded into every VM instance.

import vm

proc boolNot(vm: var RodVM) =
  discard

proc numBnot(vm: var RodVM) =
  discard

proc numNegate(vm: var RodVM) =
  discard

proc numAdd(vm: var RodVM) =
  discard

proc numSub(vm: var RodVM) =
  discard

proc numMul(vm: var RodVM) =
  discard

proc numDiv(vm: var RodVM) =
  discard

proc println(vm: var RodVM) =
  discard

proc registerStdlib*(vm: var RodVM) =
  # top-level
  vm.addFn("println(_)", println)

  # class-level
  var boolClass = newForeignClass()
  boolClass.addFn("!(_)", boolNot)
  vm.addClass(boolClass, "bool")

  var numClass = newForeignClass()
  numClass.addFn("~(_)", numBnot)
  numClass.addFn("-(_)", numNegate)
  numClass.addFn("+(_,_)", numAdd)
  numClass.addFn("-(_,_)", numSub)
  numClass.addFn("*(_,_)", numMul)
  numClass.addFn("/(_,_)", numDiv)
  vm.addClass(numClass, "num")
