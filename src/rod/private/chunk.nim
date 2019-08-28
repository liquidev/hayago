#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import tables

import common
import types
import value

type
  Opcode* = enum
    # Stack
    opcPushTrue = "pushTrue"
    opcPushFalse = "pushFalse"
    opcPushN = "pushN" ## Push number
    opcPushG = "pushG" ## Push global
    opcPopG = "popG" ## Pop global
    opcPushL = "pushL" ## Push local
    opcPopL = "popL" ## Pop local
    opcDiscard = "discard" ## Discard 1 value
    opcNDiscard = "nDiscard" ## Discard n values
    # Arithmetic operations
    opcNegN = "negN" ## Negate number
    opcAddN = "addN" ## Add numbers
    opcSubN = "subN" ## Subtract numbers
    opcMultN = "multN" ## Multiply numbers
    opcDivN = "divN" ## Divide numbers
    # Logic operations
    opcInvB = "invB" ## Invert bool
    # Relational operations
    opcEqB = "eqB" ## Equal bools
    opcEqN = "eqN" ## Equal numbers
    opcLessN = "lessN" ## Number less than
    opcLessEqN = "lessEqN" ## Number less than or equal
    opcGreaterN = "greaterN" ## Number greater than
    opcGreaterEqN = "greaterEqN" ## Number greater than or equal
    # Execution
    opcJumpFwd = "jumpFwd" ## Jump forward
    opcJumpFwdT = "jumpFwdT" ## Jump forward if true
    opcJumpFwdF = "jumpFwdF" ## Jump forward if false
    opcJumpBack = "jumpBack" ## Jump backward
    opcHalt = "halt"
  LineInfo* = tuple
    ln, col: int
    runLength: int
  Chunk* = object
    module*: Module
    code*: seq[uint8]
    ln*, col*: int
    lineInfo: seq[LineInfo]
    consts*: OrdTable(RodValueKind, seq[RodValue])
  Module* = object
    name*: string
    types*: Table[string, RodType]
    globals*: Table[string, Variable]
  VariableKind* = enum
    vkGlobal
    vkLocal
  Variable* = object
    ty*: RodType
    name*: string
    isMutable*, isSet*: bool
    case kind*: VariableKind
    of vkGlobal:
      discard
    of vkLocal:
      stackPos*, scope*: int

proc addLineInfo*(chunk: var Chunk, n: int) =
  if chunk.lineInfo.len > 0:
    if chunk.lineInfo[^1].ln == chunk.ln and
       chunk.lineInfo[^1].col == chunk.col:
      inc(chunk.lineInfo[^1].runLength, n)
      return
  chunk.lineInfo.add((chunk.ln, chunk.col, n))

proc emit*(chunk: var Chunk, opc: Opcode) =
  chunk.addLineInfo(1)
  chunk.code.add(opc.uint8)

proc emit*(chunk: var Chunk, u8: uint8) =
  chunk.addLineInfo(1)
  chunk.code.add(u8)

proc emit*(chunk: var Chunk, u16: uint16) =
  chunk.addLineInfo(2)
  chunk.code.add([uint8 u16 and 0x00ff'u16,
                  uint8 (u16 and 0xff00'u16) shr 8])

proc emitHole*(chunk: var Chunk, size: int): int =
  result = chunk.code.len
  chunk.addLineInfo(size)
  for i in 1..size:
    chunk.code.add(0x00)

proc fillHole*(chunk: var Chunk, hole: int, val: uint8) =
  chunk.code[hole] = val

proc fillHole*(chunk: var Chunk, hole: int, val: uint16) =
  chunk.code[hole] = uint8 val and 0x00ff
  chunk.code[hole + 1] = uint8 (val and 0xff00) shr 8

proc getOpcode*(chunk: Chunk, i: int): Opcode =
  result = chunk.code[i].Opcode

proc getU8*(chunk: Chunk, i: int): uint8 =
  result = chunk.code[i]

proc getU16*(chunk: Chunk, i: int): uint16 =
  result = chunk.code[i].uint16 or chunk.code[i + 1].uint16 shl 8

proc getLineInfo*(chunk: Chunk, i: int): LineInfo =
  var n = 0
  for li in chunk.lineInfo:
    for r in 1..li.runLength:
      if n == i:
        return li
      inc(n)

proc initChunk*(): Chunk =
  result = Chunk()

proc addSimpleType*(module: var Module, name: string) =
  module.types.add(name, RodType(name: name, kind: tkSimple))

proc initModule*(): Module =
  result.addSimpleType("void")
  result.addSimpleType("bool")
  result.addSimpleType("number")
