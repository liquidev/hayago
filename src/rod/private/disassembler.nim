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
    case opc
    of opcPushN:
      result.add($chunk.consts[rvNumber][chunk.getU16(pc + 1)])
      inc(pc, 3)
    of opcPushG, opcPopG:
      result.add(chunk.consts[rvString][chunk.getU16(pc + 1)].str)
      inc(pc, 3)
    of opcPushL, opcPopL, opcNDiscard:
      result.add($chunk.getU8(pc + 1))
      inc(pc, 2)
    of opcJumpFwd, opcJumpFwdF, opcJumpFwdT, opcJumpBack:
      result.add(alignLeft($chunk.getU16(pc + 1), 6))
      result.add("→ ")
      if opc in {opcJumpFwd, opcJumpFwdF, opcJumpFwdT}:
        result.add(toHex(pc + chunk.getU16(pc + 1).int, 8))
      else:
        result.add(toHex(pc - chunk.getU16(pc + 1).int, 8))
      inc(pc, 3)
    of opcPushTrue, opcPushFalse, opcDiscard,
       opcInvB, opcEqB,
       opcNegN, opcAddN, opcSubN, opcMultN, opcDivN,
       opcEqN, opcLessN, opcLessEqN, opcGreaterN, opcGreaterEqN,
       opcHalt: inc(pc, 1)
    result.add('\n')
