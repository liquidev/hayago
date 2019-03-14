#~~
# the rod programming language
# copyright (C) iLiquid, 2019
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

proc jump(call: var Call, pc: int) =
  call.pc = pc

proc initCall(env: RodEnv): Call =
  Call(
    env: env,
    isModMain: false,
    isForeign: false
  )

proc modMain(call: Call): Call =
  result = call
  result.isModMain = true

proc foreign(call: Call): Call =
  result = call
  result.isForeign = true

proc setFn(call: Call, fn: RodBaseFn): Call =
  result = call
  result.fn = fn

proc setChunk(call: Call, chunk: RodChunk): Call =
  result = call
  result.chunk = chunk

#~~
# Functions
#~~

method doCall*(fn: RodBaseFn, vm: var RodVM) {.base.}

method doCall*(fn: RodForeignFn, vm: var RodVM)

#~~
# Objects
#~~

proc construct*(class: RodClass, vm: var RodVM): RodObj =
  class[".ctor"].doCall(vm)

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

proc makeSlots*(vm: var RodVM, slots: int) =
  vm.slots.setLen(slots)

proc clearSlots*(vm: var RodVM) =
  vm.slots.setLen(0)

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
    {.computedGoto.}
    let op = call.readOp()

    case op
    # values and variables
    of roPushConst:
      vm.push(chunk.consts[int call.readU16()])
    of roPushGlobal:
      let name = chunk.symbols[int call.readU16()]
      if call.env.globals.hasKey(name):
        vm.push(call.env[name])
      else:
        vm.err(TypeError, "Cannot reference undeclared global '" & name & "'")
    of roPushLocal:
      vm.push(vm.locals[int call.readU16()])

    of roDiscard:
      discard vm.pop()
    of roNewGlobal:
      let name = chunk.symbols[int call.readU16()]
      if not call.env.globals.hasKey(name):
        call.env[name] = RodNull
    of roPopGlobal, roStoreGlobal:
      let name = chunk.symbols[int call.readU16()]
      if call.env.globals.hasKey(name):
        call.env[name] = case op
          of roPopGlobal: vm.pop()
          else: vm.peek()
      else:
        vm.err(TypeError, "Cannot store undeclared global '" & name & "'")
    of roPopLocal, roStoreLocal:
      let id = int call.readU16()
      if id <= vm.locals.len:
        vm.locals.setLen(id + 1)
      vm.locals[id] = case op
        of roPopLocal: vm.pop()
        else: vm.peek()

    # functions, methods and calls
    of roPushMethod:
      let
        receiver = vm.peek()
        name = chunk.symbols[int call.readU16()]
      var class: RodClass
      if receiver.kind == rvkObj: class = receiver.objVal.class
      else: class = call.env[receiver.className()].objVal.class
      if class.methods.hasKey(name):
        vm.push(class.methods[name])
      else:
        vm.err(TypeError, class.name & " doesn't implement " & name & "()")
    of roCallFn:
      vm.clearSlots()
      let fn = vm.pop()
      vm.makeSlots(1)
      vm.slots[0] = RodNull
      vm.slots.add(vm.popSeq(int call.readU8()))
      if fn.kind == rvkFn:
        fn.fnVal.doCall(vm)
      elif fn.kind == rvkClass:
        vm.push(fn.classVal.construct(vm))
      else:
        vm.err(TypeError, $fn & " is not a function")
    of roCallMethod:
      vm.clearSlots()
      vm.makeSlots(1)
      vm.slots[0] = RodNull
      vm.slots.add(vm.popSeq(int call.readU8()))
      let
        fn = vm.pop()
        receiver = vm.pop()
      vm.slots[0] = receiver
      if fn.kind == rvkFn:
        fn.fnVal.doCall(vm)
      else:
        vm.err(TypeError, $fn & " is not a function")

    # flow control
    of roJump:
      call.jump(chunk.offsets[int call.readU16()])
    of roJumpCond:
      let condition = vm.pop()
      if condition:
        call.jump(chunk.offsets[int call.readU16()])
      else:
        call.pc += 2
    of roReturn:
      if vm.stack.len > 0:
        return vm.pop()
      else:
        return RodNull

proc interpret*(vm: var RodVM, env: var RodEnv, chunk: RodChunk): RodValue =
  vm.enterCall(initCall(env)
    .modMain()
    .setChunk(chunk))
  result = vm.runCall()
  vm.exitCall()

proc newVM*(): RodVM =
  result = RodVM(
    stack: @[],
    locals: @[],
    slots: @[]
  )

#~~
# Function implementations
#~~

proc addMethod*(class: var RodClass, name: string, impl: RodForeignProc) =
  class.methods[name] = RodForeignFn(impl: impl)

method doCall*(fn: RodBaseFn, vm: var RodVM) {.base.} =
  vm.err(TypeError, "Tried to call an unimplemented function type")

method doCall*(fn: RodForeignFn, vm: var RodVM) =
  vm.enterCall(initCall(vm.currentCall.env)
    .foreign()
    .setFn(fn))
  fn.impl(vm, vm.currentCall.env)
  vm.push(vm[0])
  vm.exitCall()
