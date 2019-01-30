#~~
# rod vm
# copyright (C) iLiquid, 2019
#~~

import tables

import compiler
import std
import value

type
  RodVM* = object of RodBaseVM
  RodModule* = object

proc newRodVM*(): RodVM =
  RodVM(
    foreignFns: @[]
  )

proc newCompiler*(vm: RodVM): RodCompiler =
  result = baseCompiler()
  result.foreignFnSignatures = vm.foreignFnSignatures

proc registerForeignFn*(vm: var RodVM, signature: string, impl: RodForeignFn) =
  let id = len(vm.foreignFns)
  vm.foreignFns.add(impl)
  vm.foreignFnSignatures.add(signature, id)
