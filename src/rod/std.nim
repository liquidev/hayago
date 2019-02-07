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

proc registerStdlib*(vm: var RodVM) =
  vm.registerForeignFn("bool::!(_)", boolNot)
  vm.registerForeignFn("num::~(_)", numBnot)
  vm.registerForeignFn("num::-(_)", numNegate)
  vm.registerForeignFn("num::+(_,_)", numAdd)
  vm.registerForeignFn("num::-(_,_)", numSub)
  vm.registerForeignFn("num::*(_,_)", numMul)
  vm.registerForeignFn("num::/(_,_)", numDiv)
