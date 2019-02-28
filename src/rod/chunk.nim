#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

import strutils

import opcode
import value

type
  RodChunk* = ref object
    code*: seq[uint8]
    consts*: seq[RodValue]
    symbols*: seq[string]

proc id*(chunk: var RodChunk, val: RodValue): uint16 =
  for i, c in chunk.consts:
    if c == val: return uint16 i
  result = uint16 chunk.consts.len
  chunk.consts.add(val)

proc sym*(chunk: var RodChunk, val: string): uint16 =
  for i, s in chunk.symbols:
    if s == val: return uint16 i
  result = uint16 chunk.symbols.len
  chunk.symbols.add(val)

proc emitU8*(chunk: var RodChunk, val: uint8) =
  chunk.code.add(val)

proc emitU16*(chunk: var RodChunk, val: uint16) =
  chunk.code.add([
    uint8((val and 0xff00'u16) shr 8),
    uint8((val and 0x00ff'u16))
  ])

proc emitU32*(chunk: var RodChunk, val: uint32) =
  chunk.code.add([
    uint8((val and 0xff000000'u32) shr 24),
    uint8((val and 0x00ff0000'u32) shr 16),
    uint8((val and 0x0000ff00'u32) shr 8),
    uint8((val and 0x000000ff'u32))
  ])

proc emitOp*(chunk: var RodChunk, op: RodOpcode) =
  chunk.emitU8(uint8 op)

proc readU8*(chunk: RodChunk, at: int): uint8 =
  result = chunk.code[at]

proc readU16*(chunk: RodChunk, at: int): uint16 =
  result =
    (chunk.code[at + 0] shl 8) and
    (chunk.code[at + 1])

proc readU32*(chunk: RodChunk, at: int): uint32 =
  result =
    (chunk.code[at + 0] shl 24) and
    (chunk.code[at + 1] shl 16) and
    (chunk.code[at + 2] shl 8) and
    (chunk.code[at + 3])

proc readOp*(chunk: RodChunk, at: int): RodOpcode =
  RodOpcode chunk.readU8(at)

proc disassemble*(chunk: RodChunk): string =
  result.add("size: " & $chunk.code.len & " bytes")
  var pc = 0
  while pc < chunk.code.len:
    result.add('\n')
    result.add(toHex(uint32 pc))
    let opcode = RodOpcode chunk.code[pc]
    result.add(" " & align($opcode, 16) & " ")
    pc += 1
    case opcode
    of roPushConst:
      result.add($chunk.consts[chunk.readU16(pc)])
      pc += 2
    of roCall:
      result.add($chunk.symbols[chunk.readU16(pc)])
      pc += 2

proc `$`*(chunk: RodChunk): string =
  result.add("bytes:")
  for b in chunk.code:
    result.add(" " & toHex(b))
  result.add("\nconsts:")
  for i, c in chunk.consts:
    result.add("\n  [" & toHex(int16 i) & "] " & $c)
  result.add("\nsymbols:")
  for i, s in chunk.symbols:
    result.add("\n  [" & toHex(int16 i) & "] " & s)
  result.add("\ndisassembly:")
  result.add(indent(disassemble(chunk), 2))

proc newChunk*(): RodChunk =
  RodChunk(
    code: newSeq[uint8](),
    consts: newSeq[RodValue]()
  )
