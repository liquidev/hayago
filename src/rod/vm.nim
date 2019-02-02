#~~
# rod vm
# copyright (C) iLiquid, 2019
#~~

from strutils import `toHex`

import deques
import tables

import value

type
  RodVM* = ref object of RootObj
    # FFI
    foreignFns*: seq[RodForeignFn]
    foreignFnSignatures*: TableRef[string, int16]
    # runtime
    stack: Deque[RodValue] ## \
      ## This is the main point of action of rod's VM – that's becasue \
      ## it's stack-based.
      ## Many implementations have proven that stack-based VMs are slower \
      ## than register-based VMs, but register-based VMs are much harder to \
      ## compile bytecode for.
    slots: seq[RodValue] ## \
      ## The slots are used for interfacing with foreign functions.
      ## Slot 0 is always ``self`` or the returned value of a function. Other \
      ## slots are the function's parameters.
      ## Note that slots are used internally too, for passing parameters \
      ## between native functions.
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

proc push*(vm: var RodVM, value: RodValue) =
  vm.stack.addLast(value)

proc pop*(vm: var RodVM): RodValue =
  vm.stack.popLast()

proc `[]`*(vm: var RodVM, slot: int): RodValue =
  vm.slots[slot]

proc `[]=`*(vm: var RodVM, slot: int, value: RodValue) =
  if len(vm.slots) - 1 < slot:
    vm.slots.setLen(slot + 1)
  vm.slots[slot] = value
