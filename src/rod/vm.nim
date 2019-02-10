#~~
# rod vm
# copyright (C) iLiquid, 2019
#~~

from strutils import `toHex`

import deques
import tables

import value

type
  RodVMCfg* = ref object
    writeFn*: proc (str: string)
      ## A callback which writes the string ``str`` to stdout.
  RodVM* = ref object of RootObj
    # config
    cfg*: RodVMCfg
    # FFI
    foreignFns*: seq[RodForeignFn]
    foreignFnSignatures*: TableRef[string, int16]
    foreignClasses*: seq[RodForeignClass]
    foreignClassSignatures*: TableRef[string, int16]
    # runtime
    stack: Deque[RodValue] ## \
      ## This is the main point of action of rod's VM – that's becasue \
      ## it's stack-based.
      ## Many implementations have proven that stack-based VMs are slower \
      ## than register-based VMs, but register-based VMs are much harder to \
      ## compile bytecode for. With rod's architecture (which is similar \
      ## to Wren's), where there aren't any dedicated arithmetic \
      ## instructions, a register-based VM won't improve performance \
      ## that much.
  RodForeignClass* = ref object
    fns*: seq[RodForeignFn]
    fnSignatures*: TableRef[string, int16]
  RodForeignFn* = proc (vm: var RodVM)

proc `$`*(vm: RodVM): string =
  result.add("foreign fns:")
  for sig, id in vm.foreignFnSignatures:
    result.add("\n  [" & toHex(int16 id) & "] " & sig)
  result.add("\nforeign classes:")
  for class, classId in vm.foreignClassSignatures:
    result.add("\n  [" & toHex(int16 classId) & "] " & class & ":")
    for fn, fnId in vm.foreignClasses[classId].fnSignatures:
      result.add("\n    [" & toHex(int16 fnId) & "] " & fn)

proc newRodVM*(): RodVM =
  result = RodVM(
    foreignFns: @[],
    foreignFnSignatures: newTable[string, int16](),
    foreignClasses: @[],
    foreignClassSignatures: newTable[string, int16]()
  )

proc newForeignClass*(): RodForeignClass =
  RodForeignClass(
    fns: @[],
    fnSignatures: newTable[string, int16]()
  )

proc addClass*(vm: var RodVM, class: RodForeignClass, name: string) =
  let id = int16 len(vm.foreignClasses)
  vm.foreignClasses.add(class)
  vm.foreignClassSignatures.add(name, id)

proc addFn*(vm: var RodVM, sig: string, impl: RodForeignFn) =
  let id = int16 len(vm.foreignFns)
  vm.foreignFns.add(impl)
  vm.foreignFnSignatures.add(sig, id)

proc getForeignClass*(vm: RodVM, name: string): RodForeignClass =
  vm.foreignClasses[vm.foreignClassSignatures[name]]

proc getFnId*(class: RodForeignClass, sig: string): int16 =
  class.fnSignatures[sig]

proc addFn*(class: var RodForeignClass,
            signature: string, impl: RodForeignFn) =
  let id = int16 len(class.fns)
  class.fns.add(impl)
  class.fnSignatures.add(signature, id)

proc push*(vm: var RodVM, value: RodValue) =
  vm.stack.addLast(value)

proc pop*(vm: var RodVM): RodValue =
  vm.stack.popLast()
