#~~
# rod lexer
# copyright (C) iLiquid, 2019
#~~

## The lexer is fairly basic – it takes a string, and outputs a sequence of
## ``Token``s.

import strutils, strscans
import sequtils

type
  Stream = object
    input: string
    pos: int

proc finished*(str: Stream): bool =
  str.pos == str.input.len

proc peek*(str: Stream, amt: int = 1, offset: int = 0): string =
  if str.pos + amt + offset - 1 < len(str.input):
    result = str.input[str.pos + offset..<str.pos + amt + offset]

proc next*(str: var Stream, amt: int = 1) =
  str.pos += amt

proc consume*(str: var Stream, amt: int = 1): string =
  result = str.peek(amt)
  str.next(amt)

type
  TokenKind* = enum
    # meta-tokens
    rtUnknown
    rtIgnore # removed from the output
    rtEof # always appended to the end of input
    rtInvalid # on error
    # braces
    rtLParen, rtRParen     # ( )
    rtLBracket, rtRBracket # [ ]
    rtLBrace, rtRBrace     # { }
    # math
    rtPlus, rtMinus, rtStar, rtSlash, rtPercent # + - * / %
    # comparison
    rtEq, rtNotEq, rtLess, rtMore, rtLessEq, rtMoreEq # == != < > <= >=
    # bitwise and boolean
    rtExcl, rtDPipe, rtDAmp                           # ! || &&
    rtTilde, rtPipe, rtAmp, rtDLess, rtTLess, rtDMore # ~ | & << <<< >>
    # punctuation
    rtDot, rtComma               # . ,
    rtDDot, rtTDot               # .. ...
    rtColon, rtDColon, rtStmtEnd # : :: ;
    rtLArrow, rtRArrow           # <- ->
    # assignment
    rtAssign # =
    # types
    rtNull, rtTrue, rtFalse # null true false
    rtNum, rtStr
    # flow control
    rtIf, rtElse, rtFor, rtWhile, rtLoop  # if else for while loop
    rtReturn, rtContinue, rtBreak         # return continue break
    rtIn                                  # in
    # declarations
    rtLet, rtFn, rtClass, rtEnum, rtTrait # let fn class enum trait
    rtImpl, rtIs, rtSelf                  # impl is self
    rtPub, rtMut, rtUse                   # pub mut use
    # identifiers
    rtIdent

  Token* = object
    text*: string
    index*, ln*, col*: int
    case kind*: TokenKind
    of rtNum:
      numVal*: float
    of rtStr:
      strVal*: string
    of rtIdent:
      ident*: string
    of rtInvalid:
      character*: char
    else:
      discard
  LexerError* = object of Exception
    invalid: char
    line: int
    column: int

const
  KeywordChars = {'a'..'z'}
  RodOps* = {
    rtPlus, rtMinus, rtStar, rtSlash, rtPercent,
    rtPipe, rtAmp, rtDLess, rtTLess, rtDMore
  }

proc getLn(str: string, idx: int): int =
  for c in str[0..idx]:
    if c == '\n': result += 1

proc getCol(str: string, idx: int): int =
  for c in str[0..idx]:
    result += 1
    if c in NewLines: result = 0

proc `$`*(token: Token): string =
  case token.kind
  of rtNum: result = $token.kind & "(" & $token.numVal & ")"
  of rtStr: result = $token.kind & "(\"" & $token.strVal & "\")"
  of rtIdent: result = $token.kind & "(" & $token.ident & ")"
  of rtInvalid: result = $token.kind & "('" & $token.character & "')"
  else: result = $token.kind

template rule(name: untyped, impl: untyped) {.dirty.} =
  proc name(input: var Stream): Token =
    impl

template literal(lits: varargs[string], tkind: TokenKind) {.dirty.} =
  for lit in lits:
    if input.peek(lit.len) == lit:
      result = Token(kind: tkind)
      result.text = input.consume(lit.len)

template literal(lits: varargs[string], lookahead: set[char], tkind: TokenKind) {.dirty.} =
  for lit in lits:
    if input.peek(lit.len) == lit and input.peek(1, lit.len)[0] notin lookahead:
      result = Token(kind: tkind)
      result.text = input.consume(lit.len)

template litRule(ident: untyped, tkind: TokenKind, lit: varargs[string]) {.dirty.} =
  rule ident: literal(lit, tkind)

template keyword(ident: untyped, tkind: TokenKind, lit: string) {.dirty.} =
  rule ident: literal(lit, KeywordChars, tkind)

# parenthesis
litRule(lParen, rtLParen, "(")
litRule(rParen, rtRParen, ")")
litRule(lBracket, rtLBracket, "[")
litRule(rBracket, rtRBracket, "]")
litRule(lBrace, rtLBrace, "{")
litRule(rBrace, rtRBrace, "}")
# math
litRule(plus, rtPlus, "+")
litRule(minus, rtMinus, "-")
litRule(star, rtStar, "*")
litRule(slash, rtSlash, "/")
litRule(percent, rtPercent, "%")
# comparison
litRule(eq, rtEq, "==")
litRule(notEq, rtNotEq, "!=")
litRule(less, rtLess, "<")
litRule(more, rtMore, ">")
litRule(lessEq, rtLessEq, "<=")
litRule(moreEq, rtMoreEq, ">=")
# boolean
litRule(excl, rtExcl, "!")
litRule(dPipe, rtDPipe, "||")
litRule(dAmp, rtDAmp, "&&")
# bitwise
litRule(tilde, rtTilde, "~")
litRule(pipe, rtPipe, "|")
litRule(amp, rtAmp, "&")
litRule(dLess, rtDLess, "<<")
litRule(tLess, rtTLess, "<<<")
litRule(dMore, rtDMore, ">>")
# punctuation
litRule(dot, rtDot, ".")
litRule(dDot, rtDDot, "..")
litRule(tDot, rtTDot, "...")
litRule(comma, rtComma, ",")
litRule(colon, rtColon, ":")
litRule(dColon, rtDColon, "::")
litRule(stmtEnd, rtStmtEnd, ";")
litRule(lArrow, rtLArrow, "<-")
litRule(rArrow, rtRArrow, "->")
# assignment
litRule(assign, rtAssign, "=")
# types
keyword(tNull, rtNull, "null")
keyword(tTrue, rtTrue, "true")
keyword(tFalse, rtFalse, "false")
rule tNum:
  if input.peek(2) == "0x" or input.peek(2) == "0X":
    var num = ""
    input.next(2)
    while input.peek.toLower[0] in HexDigits:
      num.add(input.consume(1))
    if num != "":
      result = Token(kind: rtNum, numVal: float64 parseHexInt(num))
  else:
    var num = ""
    while input.peek[0] in Digits:
      num.add(input.consume(1))
    if input.peek == "." and input.peek(1, 1)[0] in Digits:
      num.add(input.consume(1))
      while input.peek[0] in Digits:
        num.add(input.consume(1))
    if num != "":
      result = Token(kind: rtNum, numVal: parseFloat(num))
rule tStr:
  # raw literals
  var isRaw = false
  if input.peek == "r\"" or input.peek == "R\"":
    input.next(1)
    isRaw = true

  if input.peek == "\"":
    var str = ""
    input.next()
    while input.peek != "\"":
      # escape sequences
      if not isRaw and input.peek == "\\":
        input.next()
        case input.peek
        of "a": str.add("\a"); input.next()
        of "b": str.add("\b"); input.next()
        of "f": str.add("\f"); input.next()
        of "n": str.add("\n"); input.next()
        of "r": str.add("\r"); input.next()
        of "t": str.add("\t"); input.next()
        of "v": str.add("\v"); input.next()
        of "e": str.add("\e"); input.next()
        of "x":
          input.next()
          str.add(char parseHexInt(input.consume(2)))
        of "u":
          input.next()
          str.add(char parseHexInt(input.consume(4)))
        of "U":
          input.next()
          str.add(char parseHexInt(input.consume(8)))
        else: str.add(input.consume())
      str.add(input.consume())
    input.next()
    result = Token(kind: rtStr, strVal: str)
# flow control
keyword(kIf, rtIf, "if")
keyword(kElse, rtElse, "else")
keyword(kFor, rtFor, "for")
keyword(kWhile, rtWhile, "while")
keyword(kLoop, rtLoop, "loop")
keyword(kReturn, rtReturn, "return")
keyword(kContinue, rtContinue, "continue")
keyword(kBreak, rtBreak, "break")
keyword(kIn, rtIn, "in")
# declarations
keyword(kLet, rtLet, "let")
keyword(kFn, rtFn, "fn")
keyword(kClass, rtClass, "class")
keyword(kEnum, rtEnum, "enum")
keyword(kIs, rtIs, "is")
keyword(kTrait, rtTrait, "trait")
keyword(kImpl, rtImpl, "impl")
keyword(kSelf, rtSelf, "self")
keyword(kPub, rtPub, "pub")
keyword(kMut, rtMut, "mut")
keyword(kUse, rtUse, "use")
# identifiers
rule ident:
  if input.peek[0] in IdentStartChars:
    var ident = ""
    ident.add(input.consume(1))
    while input.peek[0] in IdentChars:
      ident.add(input.consume(1))
    result = Token(kind: rtIdent, ident: ident)
# whitespace
rule comment:
  if input.peek(2) == "//":
    input.next(2)
    while input.peek != "\n":
      input.next()
    result = Token(kind: rtIgnore)
  elif input.peek(2) == "/*":
    input.next(2)
    while input.peek(2) != "*/":
      input.next()
    result = Token(kind: rtIgnore)
rule wsp:
  var isWsp = false
  while input.peek[0] in Whitespace:
    input.next()
    isWsp = true
  if isWsp: result = Token(kind: rtIgnore)
litRule(eof, rtEof, "\4")
# errors
rule invalid:
  result = Token(kind: rtInvalid, index: input.pos, character: input.consume()[0])

template exec(token: untyped) {.dirty.} =
  block token:
    let
      ln = getLn(istr.input, istr.pos)
      col = getCol(istr.input, istr.pos)
    var ruleResult = token(istr)
    ruleResult.index = istr.pos
    ruleResult.ln = ln
    ruleResult.col = col
    if ruleResult.kind != rtUnknown:
      result.add(ruleResult)
      break rules # don't try this at work

template execAll() {.dirty.} =
  block rules:
    # do not change the order, unless fixing a bug
    # whitespace/ignored
    exec wsp; exec comment
    exec eof
    # arrows
    exec lArrow; exec rArrow
    # math
    exec plus; exec minus
    exec star; exec slash
    exec percent
    # comparison
    exec eq; exec notEq
    exec less; exec more
    exec lessEq; exec moreEq
    # boolean
    exec excl
    exec dPipe; exec dAmp
    # bitwise
    exec tilde; exec pipe; exec amp
    exec dLess; exec tLess
    exec dMore
    # punctuation
    exec dDot; exec tDot
    exec dot; exec comma
    exec dColon; exec colon
    exec stmtEnd
    # assignment
    exec assign
    # parenthesis
    exec lParen; exec rParen
    exec lBracket; exec rBracket
    exec lBrace; exec rBrace
    # types
    exec tNull
    exec tTrue; exec tFalse
    exec tNum
    exec tStr
    # flow control
    exec kIf; exec kElse
    exec kFor; exec kWhile; exec kLoop
    exec kReturn; exec kContinue; exec kBreak
    exec kIn
    # declarations
    exec kLet; exec kFn
    exec kClass; exec kEnum; exec kIs
    exec kTrait; exec kImpl
    exec kSelf
    exec kPub; exec kMut; exec kUse
    # identifiers
    exec ident
    # any invalid characters
    exec invalid

proc tokenize*(input: string): seq[Token] =
  var
    istr = Stream(input: input & '\4')
    finished = false
  while not finished:
    execAll()
    let token = result[len(result) - 1]
    case token.kind
    of rtEof: finished = true
    of rtInvalid:
      var
        err = newException(LexerError, "Invalid token '" & token.character & "'")
      raise err

    else: discard
  result = filter(result) do (token: Token) -> bool:
    case token.kind
    of rtIgnore: false
    else: true
