#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

import strutils
import tables

type
  RodScanner* = object
    input*: string
    pos*: int
  RodTokenKind* = enum
    rtkNone
    # types
    rtkNull, rtkBool, rtkNum, rtkStr
    # operators
    rtkOp
  RodToken* = object
    case kind*: RodTokenKind
    of rtkBool: boolVal*: bool
    of rtkNum:  numVal*: float
    of rtkStr:  strVal*: string
    of rtkOp:
      op*: string
      prec*: int
    else: discard
    pos*: int
  TextPos* = tuple
    ln, col: int
  ScannerError* = object of Exception

#~~
# Scanner
#~~

proc atEnd*(scan: RodScanner): bool =
  scan.pos >= scan.input.len

proc peek*(scan: RodScanner, amt: int): string =
  if scan.pos + amt <= scan.input.len:
    return scan.input[scan.pos..scan.pos + (amt - 1)]
  else: return "\0"

proc next*(scan: var RodScanner, amt: int) =
  scan.pos += amt

proc consume*(scan: var RodScanner, amt: int): string =
  result = scan.peek(amt)
  scan.next(amt)

proc match*(scan: var RodScanner, str: string): bool =
  if scan.peek(str.len) == str:
    scan.next(str.len)
    result = true

proc textPos*(scan: var RodScanner): TextPos =
  var ln, col = 0
  for l in splitLines(scan.input[0..scan.pos]):
    col = 0
    for c in l:
      col += 1
    ln += 1
  return (ln, col)

proc err*(scan: var RodScanner, msg: string) =
  var
    pos = scan.textPos()
    msg = $pos.ln & ":" & $pos.col & ": " & msg
  raise newException(ScannerError, msg)

#~~
# Rules
#~~

var rules: array[low(RodTokenKind)..high(RodTokenKind),
  proc (scan: var RodScanner): RodToken {.nimcall.}]

proc expect*(scan: var RodScanner,
             token: var RodToken, expected: varargs[RodTokenKind]): bool =
  for exp in expected:
    let ruleResult = rules[exp](scan)
    if ruleResult.kind != rtkNone:
      token = ruleResult
      return true

template rule(tokenKind: RodTokenKind, body: untyped): untyped {.dirty.} =
  rules[tokenKind] = proc (scan: var RodScanner): RodToken {.nimcall.} =
    let pos = scan.pos
    body
    if result.kind != rtkNone:
      result.pos = pos
    else:
      scan.pos = pos

rule rtkNull:
  if scan.match("null"):
    result = RodToken(kind: rtkNull)

rule rtkBool:
  if scan.match("false"):
    result = RodToken(kind: rtkBool, boolVal: false)
  elif scan.match("true"):
    result = RodToken(kind: rtkBool, boolVal: true)

rule rtkNum:
  if scan.match("0x"):
    var hexNum = ""
    while scan.peek(1)[0] in HexDigits:
      hexNum.add(scan.consume(1))
    if hexNum == "":
      scan.err("Hex number literal must have at least one digit")
    result = RodToken(kind: rtkNum, numVal: float parseHexInt(hexNum))
  else:
    var num = ""
    while scan.peek(1)[0] in Digits:
      num.add(scan.consume(1))
    if scan.peek(1) == "." and scan.peek(2)[1] in Digits:
      num.add(scan.consume(1))
      while scan.peek(1)[0] in Digits:
        num.add(scan.consume(1))
    if num != "":
      result = RodToken(kind: rtkNum, numVal: parseFloat(num))

rule rtkStr:
  if scan.match("\""):
    var str = ""
    while scan.peek(1) != "\"":
      str.add(scan.consume(1))

const
  OperatorChars = {
    '=', '+', '-', '*', '/', '<', '>',
    '@', '$', '~', '&', '%', '|',
    '!', '?', '^', '.', ':',
    'a'..'z'
  }
  ReservedOps = [
    ".", "=", ":", "::"
  ]

rule rtkOp:
  var op = ""
  while scan.peek(1)[0] in OperatorChars:
    op.add(scan.consume(1))
  if op != "" and op notin ReservedOps:
    var prec = -1
    # primary operators
    if op in [ "^" ]: prec = 10
    elif op in [ "*", "/", "%" ]: prec = 9
    elif op in [ "+", "-" ]: prec = 8
    elif op in [ "&" ]: prec = 7
    elif op in [ "..", "..." ]: prec = 6
    elif op in [ "==", "<=", "<", ">=", ">", "!=",
                 "is", "in" ]: prec = 5
    elif op in [ "&&" ]: prec = 4
    elif op in [ "||" ]: prec = 3
    # fallback operators
    if prec == -1:
      if op.endsWith("="): prec = 1
      elif op.endsWith("->") or op.endsWith("=>") or op.endsWith("~>"):
        prec = 0
      case op[0]
      of '$', '^': prec = 10
      of '*', '%', '/': prec = 9
      of '+', '-', '|', '~': prec = 8
      of '&': prec = 7
      of '.': prec = 6
      of '=', '<', '>', '!': prec = 5
      of '@', ':', '?': prec = 2
      else: discard

    if prec != -1:
      result = RodToken(kind: rtkOp, op: op, prec: prec)
    else:
      scan.err("Failed to determine operator's precedence")

proc newScanner*(text: string): RodScanner =
  result = RodScanner(
    input: text,
    pos: 0
  )
