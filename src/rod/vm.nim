#~~
# rod vm
# copyright (C) iLiquid, 2019
#~~

from strutils import `toHex`
import tables

type
  RodVM* = ref object of RootObj
    foreignFns*: seq[RodForeignFn]
    foreignFnSignatures*: TableRef[string, int16]
  RodForeignFn* = proc (vm: var RodVM)

proc `$`*(vm: RodVM): string =
  result.add("foreign fns:")
  for sig, id in vm.foreignFnSignatures:
    result.add("\n  [" & toHex(id) & "] " & sig)

proc newRodVM*(): RodVM =
  result = RodVM(
    foreignFns: @[],
    foreignFnSignatures: newTable[string, int16]()
  )

proc registerForeignFn*(vm: var RodVM, signature: string, impl: RodForeignFn) =
  let id = int16 len(vm.foreignFns)
  vm.foreignFns.add(impl)
  vm.foreignFnSignatures.add(signature, id)
