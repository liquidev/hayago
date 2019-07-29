#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import strutils

import chunk
import value

proc register*(num: uint8): string =
  result = '[' & $num & ']'

proc disassemble*(chunk: Chunk): string =
  for i, c in chunk.code:
    let opc = chunk.getOp(i)
    result.add(i.toHex(8) & "  ")
    result.add(alignLeft($opc, 12))
    case opc
    of opcMoveN:
      let (_, dest, id) = chunk.getOp1u8u16(i)
      result.add(dest.register & ' ' & $chunk.consts[rvNumber][id])
    of opcNegN:
      let (_, dest, src, _) = chunk.getOp3u8(i)
      result.add(dest.register & ' ' & src.register)
    of opcAddN, opcSubN, opcMultN, opcDivN:
      let (_, dest, a, b) = chunk.getOp3u8(i)
      result.add(dest.register & ' ' & a.register & ' ' & b.register)
    else: discard
    result.add('\n')
