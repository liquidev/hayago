#~~
# rod lexer
# copyright (C) iLiquid, 2019
#~~

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

proc consume*(str: var Stream, amt: int = 1): string =
  result = str.peek(amt)
  str.pos += amt

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
    # assignment
    rtAssign # =
    # types
    rtNull, rtTrue, rtFalse # null true false
    rtNum, rtStr
    # flow control
    rtIf, rtFor, rtWhile, rtLoop  # if for while loop
    rtReturn, rtContinue, rtBreak # return continue break
    # declarations
    rtLet, rtFn, rtClass, rtEnum, rtMixin # let fn class enum mixin
    rtSelf                                # self
    rtPub, rtUse                          # pub use
    # identifiers
    rtIdent

  Token* = object
    text: string
    case kind: TokenKind
    of rtNum:
      numVal: float64
    of rtStr:
      strVal: string
    of rtIdent:
      ident: string
    of rtInvalid:
      character: char
      index: int
    else:
      discard
  LexerError* = object of Exception
    invalid: char
    line: int
    column: int

const
  KeywordChars = {'a'..'z'}

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
# assignment
litRule(assign, rtAssign, "=")
# types
keyword(tNull, rtNull, "null")
keyword(tTrue, rtTrue, "true")
keyword(tFalse, rtFalse, "false")
rule tNum:
  if input.peek(2) == "0x" or input.peek(2) == "0X":
    var num = ""
    discard input.consume(2)
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
  if input.peek == "\"":
    var str = ""
    discard input.consume()
    while input.peek != "\"":
      str.add(input.consume())
    discard input.consume()
    result = Token(kind: rtStr, strVal: str)
# flow control
keyword(kIf, rtIf, "if")
keyword(kFor, rtFor, "for")
keyword(kWhile, rtWhile, "while")
keyword(kLoop, rtLoop, "loop")
keyword(kReturn, rtReturn, "return")
keyword(kContinue, rtContinue, "continue")
keyword(kBreak, rtBreak, "break")
# declarations
keyword(kLet, rtLet, "let")
keyword(kFn, rtFn, "fn")
keyword(kClass, rtClass, "class")
keyword(kEnum, rtEnum, "enum")
keyword(kMixin, rtMixin, "mixin")
keyword(kSelf, rtSelf, "self")
keyword(kPub, rtPub, "pub")
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
    discard input.consume(2)
    while input.peek != "\n":
      discard input.consume()
    result = Token(kind: rtIgnore)
  elif input.peek(2) == "/*":
    discard input.consume(2)
    while input.peek(2) != "*/":
      discard input.consume()
    result = Token(kind: rtIgnore)
rule wsp:
  var isWsp = false
  while input.peek[0] in Whitespace:
    discard input.consume()
    isWsp = true
  if isWsp: result = Token(kind: rtIgnore)
litRule(eof, rtEof, "\4")
# errors
rule invalid:
  result = Token(kind: rtInvalid, index: input.pos, character: input.consume()[0])

template exec(token: untyped) {.dirty.} =
  block token:
    let ruleResult = token(istr)
    if ruleResult.kind != rtUnknown:
      result.add(ruleResult)
      break rules # don't try this at work

template execAll() {.dirty.} =
  block rules:
    # do not change the order, unless fixing a bug
    # whitespace/ignored
    exec wsp; exec comment
    exec eof
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
    exec kIf
    exec kFor; exec kWhile; exec kLoop
    exec kReturn; exec kContinue; exec kBreak
    # declarations
    exec kLet; exec kFn
    exec kClass; exec kEnum; exec kMixin
    exec kSelf
    exec kPub; exec kUse
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
