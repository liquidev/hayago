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
    # generic helpers
    rnList
    # literals
    rnVec, rnTable, rnTablePair
    rnInstance, rnFields, rnFieldPair
    rnClosure
    # blocks
    rnBlock
    # scope access
    rnNamespace, rnVariable
    # operators
    rnPrefix, rnInfix
    rnAssign, rnAssignTuple
    rnIndex
    # statements
    rnIf, rnWhile, rnFor
    rnForTuple
    rnReturn
    # declarations
    rnType, rnGeneric, rnTyped
    rnPub
    rnLet, rnLetMut
    rnFn, rnFnArgs
    rnClass, rnImpl, rnSelf
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

proc error(tokens: var Stream[Token], message: string) =
  var exceptn = newException(ParserError, "")
  exceptn.ln = tokens.peek[0].ln + 1
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
      if node.kind != rnEmpty: nodes.add(node)
      if next.kind == delimiter:
        continue
      elif next.kind == fin:
        break
      else:
        tokens.error("Unexpected token \"" & tokens.peek[0].text & "\"")
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

# vec → delim('{', ',', '}') -> expr
rule "vec":
  let elements = tokens.delim(rtLBracket, rtComma, rtRBracket, "expr")
  if elements.isSome():
    result = node(rnVec, elements.get())

# pair → (Ident | constructor) expr
rule "pair":
  var
    key: RodNode
    value: RodNode
  if tokens.peek[0].kind == rtIdent:
    let name = tokens.consume[0]
    key = RodNode(kind: rnStr, strVal: name.ident)
  else:
    key = exec "constructor"
  if tokens.match(rtColon):
    value = exec "expr"
    result = node(rnTablePair, [key, value])
  else:
    tokens.error("Expected a colon ':'")

# table → delim('{', ',', '}') -> pair
rule "table":
  let elements = tokens.delim(rtLBrace, rtComma, rtRBrace, "pair")
  if elements.isSome():
    result = node(rnTable, elements.get())

rule "fieldPair":
  let field = exec "ident"
  if tokens.match(rtColon):
    let value = exec "expr"
    result = node(rnFieldPair, [field, value])
  else:
    result = node(rnFieldPair, [field, node(rnVariable, [field])])

rule "instance":
  let class = exec "type"
  let fields = tokens.delim(rtLBrace, rtComma, rtRBrace, "fieldPair")
  if fields.isSome():
    result = node(rnInstance, [class, node(rnFields, fields.get())])

# closureArgs → delim('|', ',', '|') -> typed
# closure → closureArgs (block | expr)
rule "closure":
  var args: Option[seq[RodNode]]
  if tokens.peek[0].kind == rtPipe:
    args = tokens.delim(rtPipe, rtComma, rtPipe, "typed")
  elif tokens.match(rtDPipe):
    args = some(newSeq[RodNode]())
  if args.isSome():
    # blocks take precedence over table constructors in closures
    let expression = tokens.firstMatch("block", "expr")
    result = node(rnClosure, [node(rnFnArgs, args.get()), expression])

# constructor → literal | vec | table | closure
rule "constructor":
  result = tokens.firstMatch("literal", "vec", "table", "instance", "closure")

# ident → Ident
rule "ident":
  if tokens.peek[0].kind == rtIdent:
    let tok = tokens.consume[0]
    result = ident(tok.ident)
  elif tokens.match(rtSelf):
    result = node(rnSelf)

# variable → ident ('.' ident)*
rule "variable":
  if tokens.peek[0].kind in { rtIdent, rtSelf }:
    var chain = @[exec "ident"]
    while tokens.match(rtDot):
      chain.add(exec "ident")
    result = node(rnVariable, chain)

# namespace → ident '::' variable
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

# callArgs → delim('(', ',', ')') -> expr
# call → namespace callArgs
rule "call":
  let varname = exec "namespace"
  if varname.kind in { rnVariable, rnNamespace }:
    let generic = exec "generic"
    let args = tokens.delim(rtLParen, rtComma, rtRParen, "expr")
    if isSome(args):
      result = node(rnCall,
        [
          varname, generic,
          node(rnCallArgs, args.get)
        ]
      )

# atom → parenExpr | prefixOp | constructor | call | namespace
rule "atom":
  result = tokens.firstMatch(
    "parenExpr", "prefixOp", "constructor", "call", "namespace"
  )

# prefixOp → PrefixOperator atom
rule "prefixOp":
  if tokens.peek[0].kind in RodPrefixOps:
    result = node(rnPrefix,
      [
        RodNode(kind: rnOperator, op: tokens.consume[0]),
        exec "atom"
      ]
    )

# index(left) → left '[' expr ']' | left
proc indexRecursive(tokens: var Stream[Token], left: RodNode): RodNode =
  sandbox:
    if tokens.peek[0].kind == rtLBracket:
      let args = tokens.delim(rtLBracket, rtComma, rtRBracket, "expr")
      if args.isSome():
        var node = @[left]
        node.add(args.get())
        result = indexRecursive(tokens, node(rnIndex, node))
      else:
        tokens.error("Invalid index syntax")
    else:
      result = left

# index → index(atom)
rule "index":
  result = indexRecursive(tokens, exec "atom")

# infix(higher, Op) → higher (Op higher)*
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
infixOps("infixMult", "index", RodMultOps)

# infixOp → infixOr
rule "infixOp":
  result = exec "infixOr"

# assignTuple → delim('(', ',', ')') -> namespace
rule "assignTuple":
  if tokens.peek[0].kind == rtLParen:
    let args = tokens.delim(rtLParen, rtComma, rtRParen, "namespace")
    if args.isSome(): result = node(rnAssignTuple, args.get())
  else:
    let arg = tokens.firstMatch("namespace")
    result = node(rnAssignTuple, [arg])

# assign → assignTuple '=' expr
rule "assign":
  let left = exec "assignTuple"
  if tokens.match(rtAssign):
    let right = exec "expr"
    result = node(rnAssign, [left, right])

# type → ident generic?
rule "type":
  let
    name = exec "ident"
    generic = exec "generic"
  result = node(rnType, [name, generic])

# generic → delim('<', ',', '>') -> type
rule "generic":
  let generic = tokens.delim(rtLess, rtComma, rtMore, "type")
  if generic.isSome():
    result = node(rnGeneric, generic.get())

# typed → ident (':' type)?
rule "typed":
  let ident = exec "ident"
  if tokens.match(rtColon):
    let typename = exec "type"
    result = node(rnTyped, [ident, typename])
  else:
    result = node(rnTyped, [ident])

# let → 'let' (assign | typed (',' typed)*)
rule "let":
  if tokens.match(rtLet):
    let letKind = if tokens.match(rtMut): rnLetMut
                  else: rnLet
    var chain = @[tokens.firstMatch("assign", "typed")]
    while tokens.match(rtComma):
      chain.add(tokens.firstMatch("assign", "typed"))
    result = node(letKind, chain)

# fnArgs(arg) → delim('(', ',', ')') -> arg
# fnT(name, arg) → 'pub'? 'fn' name fnArgs(arg) ('->` fnReturnT)? block
proc fnT(tokens: var Stream[Token], nameRule, argRule: string): RodNode =
  sandbox:
    let pub = if tokens.match(rtPub): node(rnPub)
      else: node(rnEmpty)
    if tokens.match(rtFn):
      let
        name = exec nameRule
        generic = exec "generic"
        args = tokens.delim(rtLParen, rtComma, rtRParen, argRule)
      if args.isSome():
        var returnType = node(rnEmpty)
        if tokens.match(rtRArrow):
          returnType = exec "type"
        let code = exec "block"
        result = node(rnFn, [pub, name, generic, node(rnFnArgs, args.get()), returnType, code])

# fn → fnT(ident, typed)
rule "fn":
  result = fnT(tokens, "ident", "typed")

# class → 'class' type classImpl
rule "class":
  if tokens.match(rtClass):
    let
      name = exec "type"
      impl = exec "classImpl"
    result = node(rnClass, [name, impl])

# classMemberT → (classFn)
# classMember → (classMemberT ';')
#             | (<('}') classMemberT)
rule "classMember":
  let member = tokens.firstMatch("let", "classFn")
  if tokens.match(rtStmtEnd) or
     tokens.peek(1, -1)[0].kind == rtRBrace:
    result = member
  else: tokens.error("Expected a semicolon ';'")

# classImpl → '{' (!'}' classMember)*
rule "classImpl":
  if tokens.match(rtLBrace):
    var members: seq[RodNode]
    while not tokens.match(rtRBrace):
      members.add(exec "classMember")
    result = node(rnImpl, members)

# overloadOp → ???
rule "overloadOp":
  for op in RodOverloadableOps:
    var success = true
    for i, tok in tokens.peek(op.len):
      if op[i] != tok.kind:
        success = false
        break
    if success:
      discard tokens.consume(op.len)
      var opNode: seq[RodNode]
      for tok in op:
        opNode.add(RodNode(kind: rnOperator, op: Token(kind: tok)))
      return node(rnList, opNode)

# classFnName → ident | overloadOp
rule "classFnName":
  result = tokens.firstMatch("ident", "overloadOp")

# self → Self
rule "self":
  if tokens.match(rtSelf):
    result = node(rnSelf)

# classFnArg → self | ident
rule "classFnArg":
  result = tokens.firstMatch("self", "typed")

# classFn → fnT(classFnName, typed)
rule "classFn":
  result = fnT(tokens, "classFnName", "classFnArg")

# decl → let
rule "decl":
  result = tokens.firstMatch("let", "fn", "class")

# if → 'if' expr block ('else' 'if' expr block)* ('else' block)?
rule "if":
  if tokens.match(rtIf):
    var ifStmt = node(rnIf, [])
    let
      check = exec "expr"
      code = exec "block"
    ifStmt.children.add([check, code])
    while tokens.match(rtElse):
      if tokens.match(rtIf):
        let
          check = exec "expr"
          code = exec "block"
        ifStmt.children.add([check, code])
      else:
        let
          code = exec "block"
        ifStmt.children.add(code)
    result = ifStmt

# loop → 'loop' block
rule "loop":
  if tokens.match(rtLoop):
    let expression = exec "block"
    result = node(rnWhile, [RodNode(kind: rnBool, boolVal: true), expression])

# while → 'while' expr block
rule "while":
  if tokens.match(rtWhile):
    let
      condition = exec "expr"
      code = exec "block"
    result = node(rnWhile, [condition, code])

# forTuple → delim('(', ',', ')') -> ident | ident
rule "forTuple":
  if tokens.peek[0].kind == rtLParen:
    let args = tokens.delim(rtLParen, rtComma, rtRParen, "ident")
    if args.isSome(): result = node(rnForTuple, args.get())
  else:
    let arg = tokens.firstMatch("ident")
    result = node(rnForTuple, [arg])

# for → 'for' forTuple 'in' expr block
rule "for":
  if tokens.match(rtFor):
    let args = exec "forTuple"
    if tokens.match(rtIn):
      let val = exec "expr"
      let code = exec "block"
      result = node(rnFor, [args, val, code])
    else:
      tokens.error("Unexpected token: " & tokens.peek[0].text)

# expr → assign | infixOp | if | block
rule "expr":
  result = tokens.firstMatch("assign", "infixOp", "if")

# parenExpr → '(' expr ')'
rule "parenExpr":
  if tokens.match(rtLParen):
    let expression = exec "expr"
    if tokens.match(rtRParen):
      result = expression

# stmtT → (expr | decl | loop | while | for)
# stmt → (stmtT ';')
#      | (<('}') stmtT)
#      | (stmtT >('}') --> { rtReturn self })
rule "stmt":
  let statement = tokens.firstMatch("block", "expr", "decl", "loop", "while", "for")
  if tokens.match(rtStmtEnd) or
     tokens.peek(1, -1)[0].kind == rtRBrace:
    result = statement
  elif tokens.peek[0].kind == rtRBrace:
    result = node(rnReturn, [statement])
  else: tokens.error("Expected a semicolon ';'")

# block → '{' stmt* '}'
rule "block":
  if tokens.match(rtLBrace):
    var statements: seq[RodNode]
    while not tokens.match(rtRBrace):
      statements.add(exec "stmt")
    result = node(rnBlock, statements)

# script → stmt* Eof
rule "script":
  var statements: seq[RodNode]
  while tokens.peek[0].kind != rtEof:
    statements.add(exec "stmt")
  result = node(rnBlock, statements)

proc initTokenStream*(tokens: seq[Token]): Stream[Token] =
  result = Stream[Token](input: tokens)

proc parse*(tokens: seq[Token], parser: string = "script"): RodNode =
  var tokens = Stream[Token](input: tokens)
  result = exec parser
