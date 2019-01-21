#~~
# rod parser
# copyright (C) iLiquid, 2019
#~~

import options
import tables
from strutils import indent
import typeinfo

import lexer, stream

export lexer.Token
export lexer.TokenKind

type
  RodParseRule = proc (tokensArg: var Stream[Token]): RodNode {.nimcall.}

  RodNodeKind* = enum
    ## The AST of rod is similar to Nim's, and in a future language update
    ## macros may be implemented.
    rnEmpty, rnError
    # leaf nodes
    rnOperator
    rnNull, rnBool, rnNum, rnStr
    rnIdent
    rnCall, rnCallArgs
    # blocks
    rnBlock
    # scope access
    rnNamespace, rnVariable
    # operators
    rnPrefix, rnInfix
    rnAssign, rnAssignTuple
    # statements
    rnIf, rnWhile, rnFor
    rnForTuple
    rnReturn
    # declarations
    rnLet, rnLetMut
  RodNode* = object
    case kind*: RodNodeKind
    of rnEmpty: discard
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

proc toLispStr*(node: RodNode, compact: bool = true): string =
  result.add("(")
  result.add($node.kind)
  case node.kind
  of rnEmpty: discard
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

proc error(tokens: var Stream[Token], message: string) =
  var exceptn = newException(ParserError, "")
  exceptn.ln = tokens.peek[0].ln
  exceptn.col = tokens.peek[0].col
  exceptn.msg = "ln " & $exceptn.ln & ":" & $exceptn.col & ": " & message
  raise exceptn

proc node(kind: RodNodeKind, children: openArray[RodNode] = []): RodNode =
  result = RodNode(kind: kind)
  for child in children:
    result.children.add(child)

proc ident(name: string): RodNode =
  RodNode(kind: rnIdent, ident: name)

var rules* = initTable[string, RodParseRule]()

template sandbox(body: untyped) {.dirty.} =
  let pos = tokens.pos
  result = node(rnEmpty)
  body
  if result.kind == rnEmpty:
    tokens.pos = pos

template rule(name: string, body: untyped): untyped {.dirty.} =
  rules.add(name) do (tokens: var Stream[Token]) -> RodNode:
    sandbox: body

template exec(rule: string): untyped {.dirty.} =
  rules[rule](tokens)

proc match(tokens: var Stream[Token], kind: TokenKind): bool =
  if tokens.peek[0].kind == kind:
    discard tokens.consume()
    return true

proc firstMatch(tokensArg: var Stream[Token],
                ruleNames: varargs[string]): RodNode =
  for r in ruleNames:
    var tokens: Stream[Token]
    shallowCopy(tokens, tokensArg)
    let node = exec r
    if node.kind != rnEmpty:
      tokensArg = tokens
      return node

proc delim(tokens: var Stream[Token],
           begin, delimiter, fin: TokenKind,
           rule: string): Option[seq[RodNode]] =
  if tokens.peek[0].kind == begin:
    discard tokens.consume()
    var nodes: seq[RodNode]
    while tokens.peek[0].kind != rtEof:
      let
        node = exec rule
        next = tokens.consume[0]
      nodes.add(node)
      if next.kind == delimiter:
        continue
      elif next.kind == fin:
        break
      else:
        tokens.error("Unexpected token")
    result = some(nodes)
  else:
    result = none(seq[RodNode])

rule "literal":
  if tokens.peek[0].kind in RodLiterals:
    let tok = tokens.consume[0]
    case tok.kind
    of rtNull:
      result = RodNode(kind: rnNull)
    of rtTrue, rtFalse:
      result = RodNode(kind: rnBool, boolVal: tok.kind == rtTrue)
    of rtNum:
      result = RodNode(kind: rnNum, numVal: tok.numVal)
    of rtStr:
      result = RodNode(kind: rnStr, strVal: tok.strVal)
    else: discard

rule "ident":
  if tokens.peek[0].kind == rtIdent:
    let tok = tokens.consume[0]
    result = ident(tok.ident)

rule "variable":
  if tokens.peek[0].kind == rtIdent:
    var chain = @[exec "ident"]
    while tokens.match(rtDot):
      chain.add(exec "ident")
    result = node(rnVariable, chain)

rule "namespace":
  let tok = tokens.peek(2)
  if tok[0].kind == rtIdent and tok[1].kind == rtDColon:
    let
      nsp = tokens.consume(2)[0]
      varname = exec "variable"
    result = node(rnNamespace,
      [
        ident(nsp.ident), varname
      ])
  else:
    result = exec "variable"

rule "call":
  let varname = exec "namespace"
  if varname.kind in { rnVariable, rnNamespace }:
    let args = tokens.delim(rtLParen, rtComma, rtRParen, "atom")
    if isSome(args):
      result = node(rnCall,
        [
          varname,
          node(rnCallArgs, args.get)
        ]
      )

rule "atom":
  result = tokens.firstMatch(
    "parenExpr", "prefixOp", "literal", "call", "namespace"
  )

rule "prefixOp":
  if tokens.peek[0].kind in RodPrefixOps:
    result = node(rnPrefix,
      [
        RodNode(kind: rnOperator, op: tokens.consume[0]),
        exec "atom"
      ]
    )

template infixOps(name: string, higher: string, ops: set[TokenKind]): untyped =
  rule name:
    var chain = @[exec higher]
    while tokens.peek[0].kind in ops:
      let
        op = tokens.consume[0]
        next = exec higher
      chain.add([next, RodNode(kind: rnOperator, op: op)])
    if chain.len == 1: result = chain[0]
    else: result = node(rnInfix, chain)

infixOps("infixOr", "infixAnd", RodOrOp)
infixOps("infixAnd", "infixIs", RodAndOp)
infixOps("infixIs", "infixCmp", RodIsOp)
infixOps("infixCmp", "infixRange", RodCompareOps)
infixOps("infixRange", "infixBitAnd", RodRangeOps)
infixOps("infixBitAnd", "infixAdd", RodBitAndOp)
infixOps("infixAdd", "infixMult", RodAddOps)
infixOps("infixMult", "atom", RodMultOps)

rule "infixOp":
  result = exec "infixOr"

rule "assignTuple":
  if tokens.peek[0].kind == rtLParen:
    let args = tokens.delim(rtLParen, rtComma, rtRParen, "namespace")
    if args.isSome(): result = node(rnAssignTuple, args.get())
  else:
    let arg = tokens.firstMatch("namespace")
    result = node(rnAssignTuple, [arg])

rule "assign":
  if tokens.peek[0].kind in { rtIdent, rtLParen }:
    let left = exec "assignTuple"
    if tokens.match(rtAssign):
      let right = exec "expr"
      result = node(rnAssign, [left, right])

rule "let":
  if tokens.match(rtLet):
    let letKind = if tokens.match(rtMut): rnLetMut
                  else: rnLet
    let decl = exec "assign"
    if decl.kind != rnEmpty:
      result = node(letKind, [decl.children[0], decl.children[1]])
    else:
      var vars = @[exec "ident"]
      while tokens.match(rtComma):
        let next = exec "ident"
        if next.kind == rnEmpty: tokens.error("Identifier expected")
        vars.add(next)
      result = node(letKind, [node(rnAssignTuple, vars)])

rule "decl":
  result = tokens.firstMatch("let")

rule "if":
  if tokens.match(rtIf):
    var ifStmt = node(rnIf, [])
    let
      check = exec "expr"
      expression = exec "expr"
    ifStmt.children.add([check, expression])
    while tokens.match(rtElse):
      if tokens.match(rtIf):
        let
          check = exec "expr"
          expression = exec "expr"
        ifStmt.children.add([check, expression])
      else:
        let
          expression = exec "expr"
        ifStmt.children.add(expression)
    result = ifStmt

rule "loop":
  if tokens.match(rtLoop):
    let expression = exec "expr"
    result = node(rnWhile, [RodNode(kind: rnBool, boolVal: true), expression])

rule "while":
  if tokens.match(rtWhile):
    let
      condition = exec "expr"
      expression = exec "expr"
    result = node(rnWhile, [condition, expression])

rule "forTuple":
  if tokens.peek[0].kind == rtLParen:
    let args = tokens.delim(rtLParen, rtComma, rtRParen, "ident")
    if args.isSome(): result = node(rnForTuple, args.get())
  else:
    let arg = tokens.firstMatch("ident")
    result = node(rnForTuple, [arg])

rule "for":
  if tokens.match(rtFor):
    let args = exec "forTuple"
    if tokens.match(rtIn):
      let val = exec "expr"
      let expression = exec "expr"
      result = node(rnFor, [args, val, expression])
    else:
      tokens.error("Unexpected token: " & tokens.peek[0].text)

rule "expr":
  result = tokens.firstMatch("assign", "infixOp", "if", "block")

rule "parenExpr":
  if tokens.match(rtLParen):
    let expression = exec "expr"
    if tokens.match(rtRParen):
      result = expression

rule "stmt":
  let statement = tokens.firstMatch("expr", "decl", "loop", "while", "for")
  if tokens.match(rtStmtEnd) or
     tokens.peek(1, -1)[0].kind == rtRBrace:
    result = statement
  elif tokens.peek[0].kind == rtRBrace:
    result = node(rnReturn, [statement])
  else: tokens.error("Expected a semicolon ';'")

rule "block":
  if tokens.match(rtLBrace):
    var statements: seq[RodNode]
    while not tokens.match(rtRBrace):
      statements.add(exec "stmt")
    result = node(rnBlock, statements)

rule "script":
  var statements: seq[RodNode]
  while tokens.peek[0].kind != rtEof:
    echo tokens.peek[0].kind
    statements.add(exec "stmt")
  result = node(rnBlock, statements)

proc initTokenStream*(tokens: seq[Token]): Stream[Token] =
  result = Stream[Token](input: tokens)

proc parse*(tokens: seq[Token], parser: string = "script"): RodNode =
  var tokens = Stream[Token](input: tokens)
  result = exec parser
