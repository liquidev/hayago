#~~
# the rod programming language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#~~

import sets
import strutils

import scanner
import opcode
import value

type
  RodChunk* {.package.} = ref object
    code*: seq[uint8]
    consts*: seq[RodValue]
    symbols*: seq[string]
    offsets*: seq[int]
    lines*: seq[tuple[pos: TextPos, len: int]]
    # compile-time state
    currentPos*: TextPos
  RodChunkLoc* = tuple[pos, size: int]

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

proc off*(chunk: var RodChunk, offset: int): uint16 =
  for i, s in chunk.offsets:
    if s == offset: return uint16 i
  result = uint16 chunk.offsets.len
  chunk.offsets.add(offset)

proc hasSym*(chunk: RodChunk, val: string): bool =
  val in chunk.symbols

proc pos*(chunk: RodChunk): int =
  chunk.code.len

proc emit(chunk: var RodChunk, n: int) =
  chunk.lines.add((chunk.currentPos, n))

proc emitU8*(chunk: var RodChunk, val: uint8) =
  chunk.emit(1)
  chunk.code.add(val)

proc emitU16*(chunk: var RodChunk, val: uint16) =
  chunk.emit(2)
  chunk.code.add([
    uint8((val and 0xff00'u16) shr 8),
    uint8((val and 0x00ff'u16))
  ])

proc emitU32*(chunk: var RodChunk, val: uint32) =
  chunk.emit(4)
  chunk.code.add([
    uint8((val and 0xff000000'u32) shr 24),
    uint8((val and 0x00ff0000'u32) shr 16),
    uint8((val and 0x0000ff00'u32) shr 8),
    uint8((val and 0x000000ff'u32))
  ])

proc emitOp*(chunk: var RodChunk, op: RodOpcode) =
  chunk.emitU8(uint8 op)

proc emitPtr*(chunk: var RodChunk, size: int): RodChunkLoc =
  result = (chunk.code.len, size)
  chunk.emit(size)
  for n in 0..<size:
    chunk.emitU8(0'u8)

proc fillPtr*(chunk: var RodChunk, point: RodChunkLoc, val: int) =
  let
    pos = point.pos
    size = point.size
  case size
  of 1:
    let u8 = uint8 val
    chunk.code[pos] = u8
  of 2:
    let u16 = uint16 val
    chunk.code[pos + 0] = uint8((u16 and 0xff00'u16) shr 8)
    chunk.code[pos + 1] = uint8((u16 and 0x00ff'u16))
  of 4:
    let u32 = uint32 val
    chunk.code[pos + 0] = uint8((u32 and 0xff000000'u32) shr 24)
    chunk.code[pos + 1] = uint8((u32 and 0x00ff0000'u32) shr 16)
    chunk.code[pos + 2] = uint8((u32 and 0x0000ff00'u32) shr 8)
    chunk.code[pos + 3] = uint8((u32 and 0x000000ff'u32))
  else:
    raise newException(ValueError,
      "Invalid chunk pointer size (must be 8, 16, or 32)")

proc readU8*(chunk: RodChunk, at: int): uint8 =
  result = chunk.code[at]

proc readU16*(chunk: RodChunk, at: int): uint16 =
  result =
    (uint16(chunk.code[at + 0]) shl 8) or
    (uint16(chunk.code[at + 1]))

proc readU32*(chunk: RodChunk, at: int): uint32 =
  result =
    (uint16(chunk.code[at + 0]) shl 24) or
    (uint16(chunk.code[at + 1]) shl 16) or
    (uint16(chunk.code[at + 2]) shl 8) or
    (uint16(chunk.code[at + 3]))

proc readOp*(chunk: RodChunk, at: int): RodOpcode =
  RodOpcode chunk.readU8(at)

proc posOf*(chunk: RodChunk, pc: int): TextPos =
  var i = 0
  for l in chunk.lines:
    if i >= pc:
      return l.pos
    i += l.len

proc disassemble*(chunk: RodChunk): string =
  result.add("chunk of size: " & $chunk.code.len & " bytes")
  var
    pc = 0
    line = -1
  while pc < chunk.code.len:
    result.add('\n')
    result.add(toHex(uint32 pc))

    let currentLine = chunk.posOf(pc).ln
    if currentLine == line:
      result.add("   |")
    else:
      result.add(" " & align($currentLine, 3))
    line = currentLine

    let opcode = RodOpcode chunk.code[pc]
    result.add(" " & align($opcode, 12) & " ")
    pc += 1
    case opcode
    of roPushConst:
      result.add($+chunk.consts[int chunk.readU16(pc)])
      pc += 2
    of roNewGlobal, roPushGlobal, roPopGlobal, roStoreGlobal,
       roPushMethod:
      result.add($chunk.symbols[int chunk.readU16(pc)])
      pc += 2
    of roPushLocal, roPopLocal, roStoreLocal:
      result.add("%" & $chunk.readU16(pc))
      pc += 2
    of roCallFn, roCallMethod:
      result.add($chunk.readU8(pc))
      pc += 1
    of roJump, roJumpCond:
      result.add("@" & toHex(int32 chunk.offsets[int chunk.readU16(pc)]))
      pc += 2
    of roDiscard, roReturn: discard

proc `$`*(chunk: RodChunk): string =
  result.add("bytes:")
  for b in chunk.code:
    result.add(" " & toHex(b))
  result.add("\nconsts:")
  for i, c in chunk.consts:
    result.add("\n  [" & toHex(int16 i) & "] " & $+c)
  result.add("\nsymbols:")
  for i, s in chunk.symbols:
    result.add("\n  [" & toHex(int16 i) & "] " & s)
  result.add("\noffsets:")
  for i, o in chunk.offsets:
    result.add("\n  [" & toHex(int16 i) & "] " & toHex(int32 o))
  result.add("\ndisassembly:\n")
  result.add(indent(disassemble(chunk), 2))

proc newChunk*(): RodChunk =
  result = RodChunk()
