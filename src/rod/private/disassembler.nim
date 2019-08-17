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
  block dumpHex:
    var col = 0
    for i in chunk.code:
      result.add(i.toHex & " ")
      inc(col)
      if col >= 16:
        result.add("\n")
        col = 0
    result.add("\n")
  var
    pc = 0
    lineInfo: LineInfo
  while pc < chunk.code.len:
    result.add(pc.toHex(8) & " ")
    let
      opc = chunk.getOpcode(pc)
      li = chunk.getLineInfo(pc)
    if lineInfo.ln == li.ln:
      result.add("   ·")
    else:
      result.add(align($li.ln, 4))
    lineInfo = li
    result.add("  " & alignLeft($opc, 12))
    inc(pc)
    case opc
    of opcPushN:
      result.add($chunk.consts[rvNumber][chunk.getU16(pc)])
      inc(pc, 2)
    of opcPushG, opcPopG:
      result.add(chunk.consts[rvString][chunk.getU16(pc)].str)
      inc(pc, 2)
    of opcPushL, opcPopL, opcNDiscard:
      result.add($chunk.getU8(pc))
      inc(pc, 1)
    of opcJumpFwd, opcJumpFwdF, opcJumpBack:
      result.add(alignLeft($chunk.getU16(pc), 6))
      result.add("→ ")
      if opc in {opcJumpFwd, opcJumpFwdF}:
        result.add(toHex(pc + chunk.getU16(pc).int, 8))
      else:
        result.add(toHex(pc - chunk.getU16(pc).int, 8))
      inc(pc, 2)
    of opcPushTrue, opcPushFalse, opcDiscard,
       opcNegN, opcAddN, opcSubN, opcMultN, opcDivN,
       opcHalt: discard
    result.add('\n')
