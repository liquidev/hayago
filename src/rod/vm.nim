#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

import chunk
import opcode
import scanner
import tables
import value

type
  RodForeignProc* = proc (vm: var RodVM, env: var RodEnv) {.nimcall.}
  RodForeignFn* = ref object of RodBaseFn
    impl*: RodForeignProc

  RodEnv* = ref object
    globals: TableRef[string, RodValue]
  RodVM* = ref object
    stack, locals, slots: seq[RodValue]
    callStack: seq[Call]

  RuntimeError* = object of Exception
    at*: TextPos
  TypeError* = object of RuntimeError

  Call = ref object
    env*: RodEnv
    case isModMain: bool
    of false:
      fn: RodBaseFn
    of true:
      chunk: RodChunk
    case isForeign: bool
    of false:
      pc: int
    of true: discard

#~~
# Environments
#~~

proc `[]`*(env: RodEnv, global: string): RodValue =
  result = env.globals[global]

proc `[]=`*(env: var RodEnv, global: string, val: RodValue) =
  env.globals[global] = val

proc `[]=`*(env: var RodEnv, global: string, fn: RodForeignProc) =
  env.globals[global] = RodValue(kind: rvkFn, fnVal:
    RodForeignFn(impl: fn))

proc newEnv*(): RodEnv =
  result = RodEnv(
    globals: newTable[string, RodValue]()
  )

#~~
# Calls
#~~

proc readOp(call: var Call): RodOpcode =
  result = call.chunk.readOp(call.pc)
  call.pc += sizeof(result)

proc readU8(call: var Call): uint8 =
  result = call.chunk.readU8(call.pc)
  call.pc += sizeof(result)

proc readU16(call: var Call): uint16 =
  result = call.chunk.readU16(call.pc)
  call.pc += sizeof(result)

proc readU32(call: var Call): uint32 =
  result = call.chunk.readU32(call.pc)
  call.pc += sizeof(result)

#~~
# Functions
#~~

# I really want this section to stay right there while still having access to
# some VM functions, that's why that proc has a forwarded declaration here
proc err*(vm: RodVM, exc: typedesc, msg: string)

proc enterCall(vm: var RodVM, call: Call)

proc exitCall(vm: var RodVM)

proc currentCall(vm: RodVM): Call

method doCall*(fn: RodBaseFn, vm: var RodVM) {.base.} =
  vm.err(TypeError, "doCall() is not implemented for " & $type(fn))

method doCall*(fn: RodForeignFn, vm: var RodVM) =
  fn.impl(vm, vm.currentCall.env)

#~~
# VM
#~~

proc enterCall(vm: var RodVM, call: Call) =
  vm.callStack.add(call)

proc exitCall(vm: var RodVM) =
  discard vm.callStack.pop()

proc currentCall(vm: RodVM): Call =
  vm.callStack[vm.callStack.len - 1]

proc err(vm: RodVM, exc: typedesc, msg: string) =
  if vm.callStack.len > 0:
    let call = vm.currentCall
    var exMsg =
      if call.isModMain:
        let pos = call.chunk.posOf(call.pc)
        $pos.ln & ":" & $pos.col & ": " & msg
      else:
        msg
    raise newException(exc, exMsg)
  else:
    raise newException(exc, msg)

proc push(vm: var RodVM, val: RodValue) =
  vm.stack.add(val)

proc peek(vm: var RodVM): RodValue =
  vm.stack[vm.stack.len - 1]

proc pop(vm: var RodVM): RodValue =
  vm.stack.pop()

proc popSeq(vm: var RodVM, arity: int): seq[RodValue] =
  if arity > 0:
    for n in 0..<arity:
      result.insert(vm.pop(), 0)

proc slotAmt*(vm: RodVM): int =
  vm.slots.len

proc ensureSlots*(vm: var RodVM, slots: int) =
  if vm.slots.len < slots:
    vm.slots.setLen(slots)

iterator slots*(vm: var RodVM): (int, var RodValue) =
  var i = 0
  for s in mitems(vm.slots):
    yield (i, s)
    i += 1

proc `[]`*(vm: RodVM, index: int): RodValue =
  vm.slots[index]

proc `[]=`*(vm: var RodVM, index: int, val: RodValue) =
  vm.slots[index] = val

proc runCall(vm: var RodVM): RodValue =
  var call = vm.currentCall
  let
    chunk = call.chunk
    code = chunk.code
  while call.pc < code.len:
    # {.computedGoto.}
    let op = call.readOp()

    case op
    # values and variables
    of roPushConst:
      vm.push(chunk.consts[call.readU16()])
    of roPushGlobal:
      let name = chunk.symbols[call.readU16()]
      if call.env.globals.hasKey(name):
        vm.push(call.env[name])
      else:
        vm.push(RodNull)
    of roPushLocal:
      vm.push(vm.locals[call.readU16()])
    of roDiscard:
      discard vm.pop()
    of roPopGlobal:
      call.env[chunk.symbols[call.readU16()]] = vm.pop()
    of roPopLocal:
      let id = int call.readU16()
      if id <= vm.locals.len:
        vm.locals.setLen(id)
      vm.locals[id] = vm.pop()

    # calls
    of roCallFn:
      let fn = vm.pop()
      vm.slots.add(RodNull)
      vm.slots.add(vm.popSeq(int call.readU8()))
      if fn.kind == rvkFn:
        fn.fnVal.doCall(vm)
      else:
        vm.err(TypeError, $fn & " is not a function")
      vm.push(vm[0])

    # flow control
    of roReturn:
      if vm.stack.len > 0:
        return vm.pop()
      else:
        return RodNull
    else:
      echo "warning: unimplemented opcode \"", op, "\""

proc interpret*(vm: var RodVM, env: var RodEnv, chunk: RodChunk): RodValue =
  let call = Call(
    env: env,
    isModMain: true, chunk: chunk,
    isForeign: false, pc: 0
  )
  vm.enterCall(call)
  result = vm.runCall()
  vm.exitCall()

proc newVM*(): RodVM =
  result = RodVM(
    stack: @[],
    locals: @[],
    slots: @[]
  )
