#~~
# rod stdlib
# copyright (C) iLiquid, 2018
#~~

## The standard library of rod is implemented in Nim, for better performance. \
## It's very lightweight, and doesn't have any unnecessary functions.
## The stdlib is loaded into every VM instance.

import vm

proc rod_boolNot(vm: var RodVM) =
  discard

proc rod_numBnot(vm: var RodVM) =
  discard

proc rod_numNegate(vm: var RodVM) =
  discard

proc rod_numAdd(vm: var RodVM) =
  discard

proc rod_numSub(vm: var RodVM) =
  discard

proc rod_numMul(vm: var RodVM) =
  discard

proc rod_numDiv(vm: var RodVM) =
  discard

proc registerStdlib*(vm: var RodVM) =
  vm.registerForeignFn("bool::!(_)", rod_boolNot)
  vm.registerForeignFn("num::~(_)", rod_numBnot)
  vm.registerForeignFn("num::-(_)", rod_numNegate)
  vm.registerForeignFn("num::+(_,_)", rod_numAdd)
  vm.registerForeignFn("num::-(_,_)", rod_numSub)
  vm.registerForeignFn("num::*(_,_)", rod_numMul)
  vm.registerForeignFn("num::/(_,_)", rod_numDiv)
