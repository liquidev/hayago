#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

import chunk
import opcode
import tables
import value

type
  RodEnv* = ref object
    globals: TableRef[string, RodValue]
  RodFiber* = object
    chunk: RodChunk
    pc: int
    env*: RodEnv
  RodVM* = ref object
    stack: seq[RodValue]
    locals: seq[RodValue]

proc `[]`*(env: RodEnv, global: string): RodValue =
  result = env.globals[global]

proc `[]=`*(env: var RodEnv, global: string, val: RodValue) =
  env.globals[global] = val

proc readOp(fiber: var RodFiber): RodOpcode =
  result = fiber.chunk.readOp(fiber.pc)
  fiber.pc += sizeof(result)

proc readU8(fiber: var RodFiber): uint8 =
  result = fiber.chunk.readU8(fiber.pc)
  fiber.pc += sizeof(result)

proc readU16(fiber: var RodFiber): uint16 =
  result = fiber.chunk.readU16(fiber.pc)
  fiber.pc += sizeof(result)

proc readU32(fiber: var RodFiber): uint32 =
  result = fiber.chunk.readU32(fiber.pc)
  fiber.pc += sizeof(result)

proc push(vm: var RodVM, val: RodValue) =
  vm.stack.add(val)

proc peek(vm: var RodVM): RodValue =
  vm.stack[vm.stack.len - 1]

proc pop(vm: var RodVM): RodValue =
  result = vm.stack.pop()

proc run*(vm: var RodVM, fiber: var RodFiber) =
  let
    chunk = fiber.chunk
    code = chunk.code
  while fiber.pc < code.len:
    let op = fiber.readOp()
    case op
    # values and variables
    of roPushConst:
      {.computedGoto.}
      vm.push(chunk.consts[fiber.readU16()])
    of roPushGlobal:
      let name = chunk.symbols[fiber.readU16()]
      if fiber.env.globals.hasKey(name):
        vm.push(fiber.env[name])
      else:
        vm.push(RodNull)
    of roPushLocal:
      vm.push(vm.locals[fiber.readU16()])
    of roDiscard:
      discard vm.pop()
    of roPopGlobal:
      fiber.env[chunk.symbols[fiber.readU16()]] = vm.pop()
    of roPopLocal:
      let id = int fiber.readU16()
      if id <= vm.locals.len:
        vm.locals.setLen(id)
      vm.locals[id] = vm.pop()
    else:
      echo "warning: unimplemented opcode \"", op, "\""

proc newFiber*(chunk: RodChunk, env: RodEnv): RodFiber =
  result = RodFiber(
    env: env,
    chunk: chunk,
    pc: 0
  )

proc newVM*(): RodVM =
  result = RodVM()
