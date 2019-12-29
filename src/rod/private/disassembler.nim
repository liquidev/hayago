#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import strutils

import chunk
import value

proc disassemble*(chunk: Chunk, input = ""): string =
  block dumpHex:
    var col = 0
    for i in chunk.code:
      result.add(i.toHex & " ")
      inc(col)
      if col >= 16:
        result.add("\n")
        col = 0
    result.add("\n")
  let inputLines = input.splitLines()
  var
    pc = 0
    lineInfo: LineInfo
    inputLine = inputLines[0]
  while pc < chunk.code.len:
    var line = ""
    line.add(pc.toHex(8) & " ")
    let
      opc = chunk.getOpcode(pc)
      li = chunk.getLineInfo(pc)
    if lineInfo.ln == li.ln:
      line.add("   ·")
      inputLine = ""
    else:
      line.add(align($li.ln, 4))
      inputLine = inputLines[li.ln - 1]
    lineInfo = li
    line.add("  " & alignLeft($opc, 12))
    case opc
    of opcPushN:
      line.add($chunk.getValue(pc + 1).into.numberVal)
      inc(pc, 1 + ValueSize)
    of opcPushS, opcPushG, opcPopG:
      line.add(escape(chunk.strings[chunk.getU16(pc + 1)]))
      inc(pc, 3)
    of opcCallD:
      line.add($chunk.getU16(pc + 1))
      inc(pc, 3)
    of opcPushL, opcPopL, opcConstrObj, opcPushF, opcPopF, opcDiscard:
      line.add($chunk.getU8(pc + 1))
      inc(pc, 2)
    of opcJumpFwd, opcJumpFwdF, opcJumpFwdT, opcJumpBack:
      line.add(alignLeft($chunk.getU16(pc + 1), 6))
      line.add("→ ")
      if opc in {opcJumpFwd, opcJumpFwdF, opcJumpFwdT}:
        line.add(toHex(pc + chunk.getU16(pc + 1).int, 8))
      else:
        line.add(toHex(pc - chunk.getU16(pc + 1).int, 8))
      inc(pc, 3)
    of opcPushTrue, opcPushFalse,
       opcInvB, opcEqB,
       opcNegN, opcAddN, opcSubN, opcMultN, opcDivN,
       opcEqN, opcLessN, opcLessEqN, opcGreaterN, opcGreaterEqN,
       opcCallR,
       opcHalt: inc(pc, 1)
    if inputLine != "":
      line = alignLeft(line, 48) & inputLine
    line.add('\n')
    result.add(line)

