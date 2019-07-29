#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import common
import value

type
  Opcode* = enum
      ## Available instruction layouts:
      ##  - O 8 8 8 - Opcode with 3 8-bit params (Op3u8)
      ##  - O 8 16  - Opcode with 1 8-bit param and 1 16-bit param (Op1u8u16)
      ##  - O 24    - Opcode with a single 24-bit param (Op1u24)
      ##  - O       - Opcode without params (Op)
    opcMoveN = "moveN"
      ## Layout: O 8 16
      ## moveN <dest> <id>
    opcNegN = "negN"
      ## Layout: O 8 8 8
      ## negN <dest> <src> 00h
    opcAddN = "addN"
      ## Layout: O 8 8 8
      ## addN <result> <a> <b>
    opcSubN = "subN"
      ## Layout: O 8 8 8
      ## subN <result> <a> <b>
    opcMultN = "multN"
      ## Layout: O 8 8 8
      ## multN <result> <a> <b>
    opcDivN = "divN"
      ## Layout: O 8 8 8
      ## divN <result> <a> <b>
    opcHalt = "halt"
      ## Layout: O
      ## halt
  Chunk* = object
    code*: seq[uint32]
    consts*: OrdTable(RodValueKind, seq[RodValue])

proc emit*(chunk: var Chunk, opc: Opcode) =
  chunk.code.add(uint32(opc) shl 24)

proc emit*(chunk: var Chunk, opc: Opcode, a: uint32) =
  chunk.code.add(uint32(opc) shl 24 or a)

proc emit*(chunk: var Chunk, opc: Opcode, a, b, c: uint8) =
  chunk.code.add(uint32(opc) shl 24 or
                 uint32(a) shl 16 or
                 uint32(b) shl 8 or
                 uint32(c))

proc emit*(chunk: var Chunk, opc: Opcode, a: uint8, b: uint16) =
  chunk.code.add(uint32(opc) shl 24 or
                 uint32(a) shl 16 or b)

proc getOp*(chunk: Chunk, i: int): Opcode =
  result = Opcode(chunk.code[i] shr 24)

proc getOp3u8*(chunk: Chunk, i: int): tuple[opc: Opcode, a, b, c: uint8] =
  let u32 = chunk.code[i]
  result = (Opcode(u32 shr 24),
            uint8 (u32 and 0x00ff0000) shr 16,
            uint8 (u32 and 0x0000ff00) shr 8,
            uint8 (u32 and 0x000000ff))

proc getOp1u24*(chunk: Chunk, i: int): tuple[opc: Opcode, a: uint32] =
  let u32 = chunk.code[i]
  result = (Opcode(u32 shr 24),
            u32 shr 16)

proc getOp1u8u16*(chunk: Chunk, i: int): tuple[opc: Opcode,
                                               a: uint8, b: uint16] =
  let u32 = chunk.code[i]
  result = (Opcode(u32 shr 24),
            uint8 (u32 and 0x00ff0000) shr 16,
            uint16 (u32 and 0x0000ffff))

proc initChunk*(): Chunk =
  result = Chunk()
