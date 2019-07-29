#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

## This module is the rod parser.

import strutils
import tables

import common

type
  Scanner* = object
    file, input: string
    pos, ln*, col*: int
  TokenKind* = enum
    tokNumber = "number"
    tokOperator = "operator", tokIdent = "ident"
    tokDot = "."
    tokEnd = "(end of input)"
  Token* = object
    ln*, col*: int
    case kind*: TokenKind
    of tokNumber:
      numberVal*: float
    of tokOperator:
      operator*: string
      prec*: int
    of tokIdent:
      ident*: string
    else: discard

proc `$`*(token: Token): string =
  result = align($token.ln & ":" & $token.col, 8) & "  " &
           alignLeft($token.kind, 12)
  case token.kind
  of tokNumber: result.add($token.numberVal)
  of tokOperator: result.add(token.operator & " (prec: " & $token.prec & ")")
  of tokIdent: result.add(token.ident)
  else: discard

const OperatorChars = {
  '=', '+', '-', '*', '/', '<', '>',
  '$', '&', '%', '!', '^', '.',
}

const Operators = {
  "not": 0, "->": 0, "$": 0,

  "=": 0,
  "or": 1, "xor": 1,
  "and": 2,
  "==": 3, "<=": 3, "<": 3, ">": 3, ">=": 3, "!=": 3,
  "in": 3, "notin": 3, "is": 3, "isnot": 3, "of": 3,
  "..": 4, "..<": 4,
  "&": 5,
  "+": 6, "-": 6,
  "*": 7, "/": 7, "%": 7,
  "div": 7, "mod": 7, "shl": 7, "shr": 7,
  "^": 8
}.toTable()

const UTF8Chars = {'\x80'..'\xff'}
const IdentStartChars = strutils.IdentStartChars + UTF8Chars
const IdentChars = strutils.IdentChars + UTF8Chars

proc error*(scan: Scanner, msg: string) =
  raise (ref RodError)(kind: reSyntax,
                       msg: $scan.ln & ":" & $scan.col & " " & msg,
                       ln: scan.ln, col: scan.col)

proc current(scan: Scanner): char =
  if scan.pos < scan.input.len: scan.input[scan.pos]
  else: '\x00'

proc get(scan: Scanner, n = 1): string =
  result = scan.input[min(scan.pos, scan.input.len)..<
                      min(scan.pos + n, scan.input.len)]

proc advance(scan: var Scanner) =
  inc(scan.pos)
  inc(scan.col)
  if scan.current == '\n':
    inc(scan.ln)
    scan.col = 0
  elif scan.current == '\r':
    scan.col = 0

proc skip(scan: var Scanner) =
  while true:
    case scan.current
    of Whitespace:
      while scan.current in Whitespace:
        scan.advance()
      continue
    of '/':
      if scan.get(2) == "//":
        scan.advance(); scan.advance()
        while scan.current != '\n':
          scan.advance()
        scan.advance()
        continue
    else: discard
    break

proc next*(scan: var Scanner): Token =
  scan.skip()
  let
    ln = scan.ln
    col = scan.col
  case scan.current
  of '\x00': result = Token(kind: tokEnd)
  of OperatorChars:
    var operator = ""
    while scan.current in OperatorChars:
      operator.add(scan.current)
      scan.advance()
    if operator in Operators:
      result = Token(kind: tokOperator,
                     operator: operator, prec: Operators[operator])
    else:
      case operator
      of ".": result = Token(kind: tokDot)
      else: scan.error("Unknown operator: '" & operator & "'")
  of Digits:
    var number = ""
    while scan.current in Digits:
      number.add(scan.current)
      scan.advance()
    if scan.current == '.':
      number.add(scan.current)
      scan.advance()
      while scan.current in Digits:
        number.add(scan.current)
        scan.advance()
    result = Token(kind: tokNumber, numberVal: parseFloat(number))
  of IdentStartChars:
    var ident = $scan.current
    scan.advance()
    while scan.current in IdentChars:
      ident.add(scan.current)
      scan.advance()
    if ident in Operators:
      result = Token(kind: tokOperator, operator: ident, prec: Operators[ident])
    else:
      result = Token(kind: tokIdent, ident: ident)
  else: scan.error("Unexpected character: '" & scan.current & "'")
  result.ln = ln
  result.col = col

proc peek*(scan: var Scanner): Token =
  let
    pos = scan.pos
    ln = scan.ln
    col = scan.col
  result = scan.next()
  scan.pos = pos
  scan.ln = ln
  scan.col = col

proc initScanner*(input: string, file = ""): Scanner =
  result = Scanner(file: file, input: input,
                   ln: 1, col: 0)
