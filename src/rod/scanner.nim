#~~
# the rod programming language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#~~

import strutils
import tables
import unicode

type
  RodScanner* = object
    input*: string
    pos*: int
  RodTokenKind* = enum
    rtkNone
    # parenthesis and punctuation
    rtkLParen = "(", rtkRParen = ")"
    rtkLBracket = "[", rtkRBracket = "]"
    rtkLBrace = "{", rtkRBrace = "}"
    rtkComma = ","
    rtkEndStmt = ";" # TODO: change this to support either ; or \n
    # flow control
    rtkIf = "if", rtkElse = "else"
    rtkLoop = "loop", rtkWhile = "while"
    rtkFor = "for", rtkIn = "in"
    rtkBreak = "break", rtkContinue = "continue"
    rtkDo = "do"
    # declarations
    rtkLet = "let"
    # types
    rtkNull = "null"
    rtkTrue = "true", rtkFalse = "false"
    rtkNum, rtkStr
    # operators
    rtkDot = ".", rtkEq = "="
    rtkColon = ":", rtkDColon = "::"
    rtkOp, rtkIdent
  RodToken* = object
    case kind*: RodTokenKind
    of rtkNum:   numVal*: float
    of rtkStr:   strVal*: string
    of rtkOp:
      op*: string
      prec*: int
      leftAssoc*: bool
    of rtkIdent: ident*: string
    else: discard
    pos*: int
  TextPos* = tuple
    ln, col: int
  SyntaxError* = object of Exception

#~~
# Scanner
#~~

proc atEnd*(scan: RodScanner): bool =
  scan.pos >= scan.input.len

proc textPos*(scan: var RodScanner): TextPos =
  var ln, col = 0
  if not scan.atEnd():
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
  raise newException(SyntaxError, msg)

proc peek*(scan: RodScanner, amt: int): string =
  if scan.pos + amt <= scan.input.len:
    return scan.input[scan.pos..scan.pos + (amt - 1)]
  else: return "\0"

proc next*(scan: var RodScanner, amt: int) =
  scan.pos += amt

proc goto*(scan: var RodScanner, pos: int) =
  scan.pos = pos

proc consume*(scan: var RodScanner, amt: int): string =
  result = scan.peek(amt)
  scan.next(amt)

proc match*(scan: var RodScanner, str: string): bool =
  if scan.peek(str.len) == str:
    scan.next(str.len)
    result = true

#~~
# Rules
#~~

var rules: array[low(RodTokenKind)..high(RodTokenKind),
  proc (scan: var RodScanner): RodToken {.nimcall.}]

proc ignore*(scan: var RodScanner): bool {.discardable.} =
  while not scan.atEnd():
    # whitespace
    if scan.peek(1)[0] in Whitespace:
      scan.next(1)
      result = true
      continue
    # single-line comments
    if scan.peek(2) == "//":
      result = true
      scan.next(2)
      while not scan.atEnd():
        scan.next(1)
        if scan.peek(1) == "\n": break
      continue
    # multi-line comments
    # rod's multiline comments support nesting, like so:
    # /* nested /* comment */ */
    var
      nested = false
      nesting = 0
    while not scan.atEnd():
      if scan.peek(2) == "/*":
        result = true
        nested = true
        scan.next(2)
        nesting += 1
      if scan.peek(2) == "*/":
        scan.next(2)
        nesting -= 1
      if nesting == 0:
        break
      scan.next(1)
    if nested: continue
    break

proc expect*(scan: var RodScanner,
             token: var RodToken, expected: openarray[RodTokenKind],
             peek: static[bool] = false): bool =
  ## The generic ``expect``. It matches all tokens in ``expected``, \
  ## and returns true if any of them matched successfully.
  ## It also stores the resulting token in ``token``.
  let lastPos = scan.pos
  scan.ignore()
  for exp in expected:
    let
      posBeforeRule = scan.pos
      ruleResult = rules[exp](scan)
    if ruleResult.kind != rtkNone:
      token = ruleResult
      result = true
      break
    else:
      scan.goto(posBeforeRule)
  when peek:
    scan.goto(lastPos)

proc expectLit*(scan: var RodScanner, lit: RodTokenKind,
                peek: static[bool] = false): bool =
  ## ``expect``, but for literals. \
  ## Returns true if the next characters in the input match ``$lit``.
  let lastPos = scan.pos
  scan.ignore()
  let litStr = $lit
  if scan.peek(litStr.len) == litStr:
    scan.next(litStr.len)
    result = true
  else:
    scan.goto(lastPos)
  when peek:
    scan.goto(lastPos)

proc expectKw*(scan: var RodScanner, kw: RodTokenKind,
               peek: static[bool] = false): bool =
  ## ``expectLit``, but for keywords. \
  ## Instead of checking for the literal match, it matches an identifier and \
  ## compares it against ``$kw``.
  ## This prevents bugs where eg. an assignment to a variable called \
  ## ``loopCheck`` would be invalidly interpreted as a ``loop`` statement.
  let lastPos = scan.pos
  scan.ignore()
  var ident: RodToken
  if scan.expect(ident, [rtkIdent]):
    let eq = ident.ident == $kw
    if eq:
      result = true
    else:
      scan.goto(lastPos)
  when peek:
    scan.goto(lastPos)

proc nextToken*(scan: var RodScanner,
                expected: varargs[RodTokenKind]): RodToken =
  if scan.expect(result, expected):
    discard

template rule(tokenKind: RodTokenKind, body: untyped): untyped {.dirty.} =
  rules[tokenKind] = proc (scan: var RodScanner): RodToken {.nimcall.} =
    let pos = scan.pos
    body
    if result.kind != rtkNone:
      result.pos = pos
    else:
      scan.goto(pos)

rule rtkNum:
  template intLit(digits: set[char], parser: untyped, name: string) =
    var num = ""
    while scan.peek(1)[0] in HexDigits:
      num.add(scan.consume(1))
    if num == "":
      scan.err(name & " number literal must have at least one digit")
    result = RodToken(kind: rtkNum, numVal: float parser(num))
  if scan.match("0x"):
    intLit(HexDigits, parseHexInt, "Hexadecimal")
  elif scan.match("0b"):
    intLit({'0', '1'}, parseBinInt, "Binary")
  elif scan.match("0o"):
    intLit({'0', '7'}, parseOctInt, "Octal")
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
    scan.next(1)
    result = RodToken(kind: rtkStr, strVal: str)

const
  OperatorChars = {
    '=', '+', '-', '*', '/', '<', '>',
    '@', '$', '~', '&', '%', '|',
    '!', '?', '^', '.', ':', '\\',
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
    var
      prec = -1
      leftAssoc = true
    # enforced operators
    if op in [ "==", "<=", "<", ">=", ">", "!=",
               "is", "in" ]: prec = 5
    elif op in [ "&&" ]: prec = 4
    elif op in [ "||" ]: prec = 3
    # custom operators
    if prec == -1:
      if op.endsWith("="): prec = 1
      elif op.endsWith("->") or op.endsWith("=>") or op.endsWith("~>"):
        prec = 0
      case op[0]
      of '^':
        prec = 10
        leftAssoc = false
      of '$': prec = 10
      of '*', '%', '/', '\\': prec = 9
      of '+', '-', '|', '~': prec = 8
      of '&': prec = 7
      of '.': prec = 6
      of '=', '<', '>', '!': prec = 5
      of '@', ':', '?': prec = 2
      else: discard
    if prec != -1:
      result = RodToken(
        kind: rtkOp, op: op,
        prec: prec, leftAssoc: leftAssoc
      )

rule rtkIdent:
  var ident = ""
  if scan.peek(1)[0] in IdentStartChars:
    ident.add(scan.consume(1))
    while scan.peek(1)[0] in IdentChars:
      ident.add(scan.consume(1))
  if ident != "":
    result = RodToken(kind: rtkIdent, ident: ident)

proc newScanner*(text: string): RodScanner =
  result = RodScanner(
    input: text,
    pos: 0
  )
