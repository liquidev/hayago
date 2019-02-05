#~~
# rod parser
# copyright (C) iLiquid, 2019
#~~

import options
import tables
from strutils import indent, toBin
import typeinfo

import lexer

export lexer.Token
export lexer.TokenKind

#~~
# token stream
#~~

type
  TokenStream = object
    tokens: seq[Token]
    pos: int

proc peek(str: TokenStream): Token =
  str.tokens[str.pos]

proc peek(str: TokenStream, amt: int, off: int = 0): seq[Token] =
  str.tokens[off + str.pos..off + str.pos + amt]

proc advance(str: var TokenStream, amt: int) =
  str.pos += amt

proc next(str: var TokenStream) =
  str.advance(1)

proc consume(str: var TokenStream): Token =
  result = str.peek()
  str.next()

proc consume(str: var TokenStream, amt: int): seq[Token] =
  result = str.peek(amt)
  str.advance(amt)

proc textPos(str: TokenStream): tuple[ln, col: int] =
  (str.peek().ln, str.peek().col)

#~~
# parser
#~~

type
  RodParseRule = proc (tokensArg: var TokenStream): RodNode {.nimcall.}

  RodNodeKind* = enum
    ## The AST of rod is similar to Nim's, and in a future language update
    ## macros may be implemented.
    rnEmpty, rnError
    # leaf/terminal nodes
    rnFlags
    rnNull, rnBool, rnNum, rnStr
    rnIdent
    rnOperator
    # common helpers
    rnList
    # operators
    rnPrefix, rnInfix, rnAssign
    # variables
    rnVariable
    # statements
    rnLet
    rnReturn
    rnBlock, rnScript
  RodNode* = object
    pos*: tuple[ln, col: int]
    case kind*: RodNodeKind
    of rnEmpty: discard
    of rnFlags:
      flags*: uint8
    of rnOperator:
      op*: Token
    of rnBool:
      boolVal*: bool
    of rnNum:
      numVal*: float64
    of rnStr:
      strVal*: string
    of rnIdent:
      ident*: string
    else:
      children*: seq[RodNode]
  ParserError = object of Exception
    ln*, col*: int

const
  RodLiterals = { rtNull, rtTrue, rtFalse, rtNum, rtStr }

  RodPrefixOps = { rtPlus, rtMinus, rtExcl, rtTilde }

  RodMultOps = { rtStar, rtSlash, rtPercent }
  RodAddOps = { rtPlus, rtMinus, rtPipe }
  RodBitAndOp = { rtAmp }
  RodRangeOps = { rtDDot, rtTDot }
  RodCompareOps = { rtLess, rtMore, rtLessEq, rtMoreEq, rtEq, rtNotEq }
  RodIsOp = { rtIs }
  RodAndOp = { rtDAmp }
  RodOrOp = { rtDPipe }

  RodOverloadableOps = [
    # standard operators
    @[rtPlus], @[rtMinus], @[rtStar], @[rtSlash], @[rtPercent],
    @[rtPipe], @[rtDPipe], @[rtAmp], @[rtDAmp], @[rtDDot], @[rtTDot],
    @[rtLess], @[rtMore], @[rtLessEq], @[rtMoreEq], @[rtEq], @[rtNotEq],
    @[rtIs], @[rtDAmp], @[rtDPipe],
    # compound operators
    @[rtLBracket, rtRBracket, rtAssign], @[rtLBracket, rtRBracket] # index
  ]

proc toLispStr*(node: RodNode, compact: bool = true): string =
  result.add("(")
  result.add($node.kind)
  case node.kind
  of rnEmpty: discard
  of rnFlags: result.add(" " & toBin(node.flags.BiggestInt, 8))
  of rnOperator: result.add(" " & $node.op.kind)
  of rnBool: result.add(" " & $node.boolVal)
  of rnNum: result.add(" " & $node.numVal)
  of rnStr: result.add(" \"" & node.strVal & "\"")
  of rnIdent: result.add(" " & node.ident)
  else:
    for c in node.children:
      if compact: result.add(" " & c.toLispStr(compact))
      else: result.add("\n" & indent(c.toLispStr(compact), 2))
  result.add(")")

proc `$`*(node: RodNode): string = node.toLispStr

proc error(tokens: var TokenStream, message: string) =
  var exceptn = newException(ParserError, "")
  exceptn.ln = tokens.peek().ln + 1
  exceptn.col = tokens.peek().col
  exceptn.msg = "ln " & $exceptn.ln & ":" & $exceptn.col & ": " & message
  raise exceptn

proc node(tokens: TokenStream,
          kind: RodNodeKind, children: openArray[RodNode] = []): RodNode =
  result = RodNode(
    pos: tokens.textPos(),
    kind: kind
  )
  for child in children:
    result.children.add(child)

proc ident(tokens: TokenStream, name: string): RodNode =
  RodNode(pos: tokens.textPos(), kind: rnIdent, ident: name)

proc flags(tokens: TokenStream, flags: uint8): RodNode =
  RodNode(pos: tokens.textPos(), kind: rnFlags, flags: flags)

var rules* = initTable[string, RodParseRule]()

template sandbox(body: untyped) {.dirty.} =
  let pos = tokens.pos
  result = tokens.node(rnEmpty)
  body
  if result.kind == rnEmpty:
    tokens.pos = pos

template rule(name: string, body: untyped): untyped {.dirty.} =
  rules.add(name) do (tokens: var TokenStream) -> RodNode:
    sandbox: body

template exec(rule: string): untyped {.dirty.} =
  rules[rule](tokens)

proc match(tokens: var TokenStream, kind: TokenKind): bool =
  if tokens.peek().kind == kind:
    discard tokens.consume()
    return true

proc firstMatch(tokensArg: var TokenStream,
                ruleNames: varargs[string]): RodNode =
  for r in ruleNames:
    var tokens: TokenStream
    shallowCopy(tokens, tokensArg)
    let node = exec r
    if node.kind != rnEmpty:
      tokensArg = tokens
      return node

proc delim(tokens: var TokenStream,
           begin, delimiter, fin: TokenKind,
           rule: string): Option[seq[RodNode]] =
  if tokens.peek().kind == begin:
    discard tokens.consume()
    var nodes: seq[RodNode]
    while tokens.peek().kind != rtEof:
      let
        node = exec rule
        next = tokens.consume()
      if node.kind != rnEmpty: nodes.add(node)
      if next.kind == delimiter:
        continue
      elif next.kind == fin:
        break
      else:
        tokens.error("Unexpected token \"" & tokens.peek().text & "\"")
    result = some(nodes)
  else:
    result = none(seq[RodNode])

# ~ syntax rules ~
# There's documentation above syntax rules written in extended PEG.
# Differences:
#  - rules are in camelCase;
#  - literal tokens are '' literals; variable tokens are in CamelCase;
#  - functions are allowed for simplicity:
#    - delim(begin, separator, end) -> rule
#  - there's lookbehind <() and lookahead >()

# literal → 'null' | 'true' | 'false' | Num | Str
rule "literal":
  if tokens.peek().kind in RodLiterals:
    let tok = tokens.consume()
    case tok.kind
    of rtNull:
      result = tokens.node(rnNull)
    of rtTrue, rtFalse:
      result = RodNode(pos: tokens.textPos(),
        kind: rnBool, boolVal: tok.kind == rtTrue)
    of rtNum:
      result = RodNode(pos: tokens.textPos(),
        kind: rnNum, numVal: tok.numVal)
    of rtStr:
      result = RodNode(pos: tokens.textPos(),
        kind: rnStr, strVal: tok.strVal)
    else: discard

# variable → Ident
rule "variable":
  if tokens.peek().kind == rtIdent:
    result = tokens.node(rnVariable,
      [
        tokens.ident(tokens.consume().ident)
      ]
    )

# atom → parenExpr | prefixOp | constructor | call | namespace
rule "atom":
  result = tokens.firstMatch(
    "parenExpr", "literal", "variable"
  )

# prefixOp → PrefixOperator atom
rule "prefixOp":
  if tokens.peek().kind in RodPrefixOps:
    result = tokens.node(rnPrefix,
      [
        RodNode(pos: tokens.textPos(), kind: rnOperator, op: tokens.consume()),
        exec "atom"
      ]
    )
  else:
    result = exec "atom"

# infix(higher, Op) → higher (Op higher)*
template infixOps(name: string, higher: string, ops: set[TokenKind]): untyped =
  rule name:
    var chain = @[exec higher]
    while tokens.peek().kind in ops:
      let
        op = tokens.consume()
        next = exec higher
      chain.add([next, RodNode(pos: tokens.textPos(), kind: rnOperator, op: op)])
    if chain.len == 1: result = chain[0]
    else: result = tokens.node(rnInfix, chain)

infixOps("infixOr", "infixAnd", RodOrOp)
infixOps("infixAnd", "infixIs", RodAndOp)
infixOps("infixIs", "infixCmp", RodIsOp)
infixOps("infixCmp", "infixRange", RodCompareOps)
infixOps("infixRange", "infixBitAnd", RodRangeOps)
infixOps("infixBitAnd", "infixAdd", RodBitAndOp)
infixOps("infixAdd", "infixMult", RodAddOps)
infixOps("infixMult", "prefixOp", RodMultOps)

# infixOp → infixOr
rule "infixOp":
  result = exec "infixOr"

rule "assign":
  var
    vars = tokens.delim(rtLParen, rtComma, rtRParen, "expr")
    assignmentTuple = tokens.node(rnList)
  if vars.isSome(): assignmentTuple.children = vars.get()
  else:
    assignmentTuple.children.add(exec "expr")
  if tokens.match(rtAssign):
    let right = exec "expr"
    result = tokens.node(rnAssign, [assignmentTuple, right])

rule "expr":
  result = tokens.firstMatch("infixOp")

rule "parenExpr":
  if tokens.match(rtLParen):
    let expression = exec "expr"
    if tokens.match(rtRParen):
      result = expression

rule "let":
  if tokens.match(rtLet):
    var flags = 0'u8
    flags += uint8(tokens.match(rtMut)) shl 0
    var assignments = @[exec "assign"]
    while tokens.match(rtComma):
      assignments.add(exec "assign")
    result = tokens.node(rnLet,
      [
        tokens.flags(flags),
        tokens.node(rnList, assignments)
      ])

rule "stmt":
  let statement = tokens.firstMatch("block", "expr", "let")
  if tokens.match(rtStmtEnd) or
     tokens.peek(1, -1)[0].kind == rtRBrace:
    discard tokens.match(rtStmtEnd)
    result = statement
  elif tokens.peek().kind == rtRBrace:
    result = tokens.node(rnReturn, [statement])
  else: tokens.error("Expected a semicolon ';'")

rule "block":
  if tokens.match(rtLBrace):
    var statements: seq[RodNode]
    while not tokens.match(rtRBrace):
      statements.add(exec "stmt")
    result = tokens.node(rnBlock, statements)

rule "decl":
  result = tokens.firstMatch("stmt")

rule "script":
  var statements: seq[RodNode]
  while not tokens.match(rtEof):
    statements.add(exec "decl")
  result = RodNode(kind: rnScript, children: statements)

proc initTokenStream*(tokens: seq[Token]): TokenStream =
  result = TokenStream(tokens: tokens)

proc parse*(tokens: seq[Token], parser: string = "script"): RodNode =
  var tokens = initTokenStream(tokens)
  result = exec parser
