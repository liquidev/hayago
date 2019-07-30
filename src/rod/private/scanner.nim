#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import strutils
import tables

import common

type
  Scanner* = object
    file*, input: string
    pos, ln*, col*: int
  TokenKind* = enum
    # Literals
    tokNumber = "Number"
    tokOperator = "Operator", tokIdent = "Identifier"
    # Punctuation
    tokLPar = "(", tokRPar = ")"
    tokDot = ".", tokComma = ",", tokSemi = ";"
    # Keywords
    tokVar = "var", tokLet = "let"
    # Special
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

const Keywords = {
  "var": tokVar, "let": tokLet
}.toTable()

const UTF8Chars = {'\x80'..'\xff'}
const IdentStartChars = strutils.IdentStartChars + UTF8Chars
const IdentChars = strutils.IdentChars + UTF8Chars

proc error*(scan: Scanner, msg: string) =
  raise (ref RodError)(kind: reSyntax,
                       msg: "in " & scan.file & $scan.ln & ":" & $scan.col &
                            " " & msg,
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

proc linefeed*(scan: var Scanner): bool =
  while true:
    case scan.current
    of Newlines:
      result = true
      while scan.current in Newlines:
        scan.advance()
      continue
    of Whitespace - Newlines:
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

proc skip(scan: var Scanner) =
  discard scan.linefeed()

proc next*(scan: var Scanner): Token =
  scan.skip()
  let
    ln = scan.ln
    col = scan.col
  case scan.current
  of '\x00': result = Token(kind: tokEnd)
  of '(': scan.advance(); result = Token(kind: tokLPar)
  of ')': scan.advance(); result = Token(kind: tokRPar)
  of ',': scan.advance(); result = Token(kind: tokComma)
  of ';': scan.advance(); result = Token(kind: tokSemi)
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
    if ident in Keywords:
      result = Token(kind: Keywords[ident])
    elif ident in Operators:
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

proc expect*(scan: var Scanner, kind: TokenKind): Token {.discardable.} =
  result = scan.next()
  if result.kind != kind:
    scan.error($kind & " expected, got " & $result.kind)

proc expectOp*(scan: var Scanner, op: string) =
  let tok = scan.next()
  if tok.kind != tokOperator or tok.operator != op:
    scan.error('\'' & op & "' expected, got " & $tok.kind)

proc initScanner*(input: string, file = "input"): Scanner =
  result = Scanner(file: file, input: input,
                   ln: 1, col: 0)
