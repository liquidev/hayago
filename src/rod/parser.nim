#~~
# rod parser
# copyright (C) iLiquid, 2019
#~~

import options
import strutils
import tables
import typeinfo

#~~
# string stream
#~~

type
  StringStream = object
    input: string
    pos: int

proc finished(str: StringStream): bool =
  str.pos == str.input.len

proc peek(str: StringStream, amt: int = 1, offset: int = 0): string =
  if str.pos + amt + offset <= len(str.input):
    result = str.input[str.pos + offset..<str.pos + amt + offset]

proc next(str: var StringStream, amt: int = 1) =
  str.pos += amt

proc consume(str: var StringStream, amt: int = 1): string =
  result = str.peek(amt)
  str.next(amt)

proc peekAny(str: StringStream,
             arr: openarray[string],
             resultLen: var int): bool =
  for t in arr:
    if str.peek(t.len) == t:
      resultLen = t.len
      return true

proc ln(str: StringStream): int =
  for c in str.input[0..str.pos]:
    if c == '\n': result += 1

proc col(str: StringStream): int =
  for c in str.input[0..str.pos]:
    result += 1
    if c in NewLines: result = 0

proc pos(str: StringStream): tuple[ln, col: int] =
  (str.ln, str.col)

#~~
# parser
#~~

type
  RodParseRule = proc (inputArg: var StringStream): RodNode {.nimcall.}

  RodNodeKind* = enum
    ## The AST of rod is similar to Nim's, and in a future language update
    ## macros may be implemented.
    rnEmpty, rnError, rnEof
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
    rnVariable, rnCall
    # statements
    rnLet
    rnReturn
    rnStmt, rnBlock, rnScript
  RodNode* = object
    pos*: tuple[ln, col: int]
    case kind*: RodNodeKind
    of rnEmpty: discard
    of rnFlags:
      flags*: uint8
    of rnOperator:
      op*: string
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
  RodPrefixOps = ["+", "-", "!", "~"]

  RodMultOps = ["*", "/", "%", "<<", ">>"]
  RodAddOps = ["+", "-", "|"]
  RodBitAndOp = ["&"]
  RodRangeOps = ["..", "..."]
  RodCompareOps = ["<", ">", "<=", ">=", "==", "!=", "<=>"]
  RodIsOp = ["is", "in"]
  RodAndOp = ["&&"]
  RodOrOp = ["||"]
  RodAssignOps = ["=", ":="]

  RodSpecialMethods = [
    # overloadable operators
    "+", "-", "*", "/", "%",
    "|", "||", "&", "&&", "..", "...",
    "<", ">", "<=", ">=", "==", "!=", "<=>",
    "is",
    "[]=", "[]",
    # metamethods
    ".ctor", ".dtor",
    ".call", ".contains"
  ]

proc toLispStr*(node: RodNode, compact: bool = true): string =
  result.add("(")
  result.add($node.kind)
  case node.kind
  of rnEmpty: discard
  of rnFlags: result.add(" " & toBin(node.flags.BiggestInt, 8))
  of rnOperator: result.add(" " & $node.op)
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

proc `[]`*(node: RodNode, index: int): RodNode =
  node.children[index]

proc error(input: var StringStream, message: string) =
  var exceptn = newException(ParserError, "")
  exceptn.ln = input.ln + 1
  exceptn.col = input.col
  exceptn.msg = "ln " & $exceptn.ln & ":" & $exceptn.col & ": " & message
  raise exceptn

proc node(input: StringStream,
          kind: RodNodeKind, children: openArray[RodNode] = []): RodNode =
  result = RodNode(
    pos: pos(input),
    kind: kind
  )
  for child in children:
    result.children.add(child)

proc ident(input: StringStream, name: string): RodNode =
  RodNode(pos: pos(input), kind: rnIdent, ident: name)

proc flags(input: StringStream, flags: uint8): RodNode =
  RodNode(pos: pos(input), kind: rnFlags, flags: flags)

var rules* = initTable[string, RodParseRule]()

template sandbox(body: untyped) {.dirty.} =
  let pos = input.pos
  result = input.node(rnEmpty)
  body
  if result.kind == rnEmpty:
    input.pos = pos

template rule(name: string, body: untyped): untyped {.dirty.} =
  rules.add(name) do (input: var StringStream) -> RodNode:
    sandbox: body

template exec(rule: string): untyped {.dirty.} =
  rules[rule](input)

const
  Whitespace = [" ", "\t", "\v", "\r", "\n", "\f"]

proc ign(input: var StringStream): bool {.discardable.} =
  let startPos = input.pos
  while not input.finished:
    var len = 0
    if input.peekAny(Whitespace, len):
      while input.peekAny(Whitespace, len):
        input.next(len)
      continue
    elif input.peek(2) == "//":
      input.next(2)
      while not input.finished:
        input.next()
        if input.peek(1) == "\n" or
          input.peek(1) == "\r" or
          input.peek(2) == "\r\n" or
          input.peek(2) == "\n\r":
          input.next()
          break
      continue
    elif input.peek(2) == "/*":
      input.next(2)
      while input.peek(2) != "*/":
        input.next()
      input.next(2)
      continue
    break
  result = input.pos != startPos

template ign: untyped {.dirty.} =
  input.ign()

proc match(input: var StringStream, str: string): bool =
  if input.peek(str.len) == str:
    discard input.consume(str.len)
    return true

proc firstMatch(input: var StringStream,
                ruleNames: varargs[string]): RodNode =
  var lastErr: ref ParserError
  for r in ruleNames:
    try:
      sandbox:
        let ruleResult = exec r
        if ruleResult.kind != rnEmpty:
          return ruleResult
    except ParserError as pe:
      lastErr = pe
  if lastErr != nil:
    raise lastErr

proc delim(input: var StringStream,
           begin, delimiter, fin: string,
           rule: string): Option[seq[RodNode]] =
  if input.match(begin):
    ign()
    var nodes: seq[RodNode]
    while not input.finished:
      ign()
      let node = exec rule
      if node.kind != rnEmpty: nodes.add(node)
      if input.match(delimiter):
        continue
      elif input.match(fin):
        break
      else:
        input.error("Unexpected character \"" & input.peek() & "\"")
    result = some(nodes)
  else:
    result = none(seq[RodNode])

# [!] WARNING: MESSY CODE INCOMING [!]
# Since rod switched to a lexless compiler, this code has gotten unbearably
# messy. It needs rewriting to an easier-to-maintain DSL (macros FTW),
# but this hasn't been done yet.
# TODO: re-write the parser to use a DSL

# ~ syntax rules ~
# There's documentation above syntax rules written in extended PEG.
# Differences:
#  - rules are in camelCase;
#  - literals are quoted;
#  - functions are allowed for simplicity:
#    - delim(begin, separator, end) -> rule
#  - there's lookbehind <() and lookahead >()

# eof → .
rule "eof":
  result = input.node(rnEof)

# null → 'null'
rule "null":
  if input.match("null"):
    result = RodNode(pos: pos(input), kind: rnNull)

# bool → 'true' | 'false'
rule "bool":
  if input.match("true"):
    result = RodNode(pos: pos(input), kind: rnBool, boolVal: true)
  elif input.match("true"):
    result = RodNode(pos: pos(input), kind: rnBool, boolVal: false)

rule "num":
  # TODO: clean up repetitive code
  var numStr = ""
  if input.peek().len > 0:
    while input.peek()[0] in Digits:
      numStr.add(input.consume())
    if input.match(".") and input.peek()[0] in Digits:
      numStr.add(".")
      while input.peek()[0] in Digits:
        numStr.add(input.consume())
    if numStr == "":
      if input.match("NaN"): numStr = "nan"
      elif input.match("Inf"): numStr = "inf"
      elif input.match("-Inf"): numStr = "-inf"
    if numStr != "":
      return RodNode(pos: pos(input),
        kind: rnNum, numVal: parseFloat(numStr))

rule "str":
  if input.match("\""):
    var str = ""
    while not input.match("\""):
      str.add(input.consume())

# literal → null | bool | num | str
rule "literal":
  result = input.firstMatch("null", "bool", "num", "str")

# ident → [a-zA-Z_] [a-zA-Z_0-9]+
rule "ident":
  if input.peek().len > 0:
    if input.peek()[0] in IdentStartChars:
      var ident = input.consume()
      while input.peek()[0] in IdentChars:
        ident.add(input.consume())
      result = input.ident(ident)

# variable → ident
rule "variable":
  let ident = exec "ident"
  if ident.kind != rnEmpty:
    result = input.node(rnVariable, [ident])

rule "call":
  let
    name = exec "ident"
    args = input.delim("(", ",", ")", "expr")
  if args.isSome():
    result = input.node(rnCall, [
      name,
      input.node(rnList, args.get())
    ])

# atom → parenExpr | prefixOp | constructor | call | namespace
rule "atom":
  result = input.firstMatch(
    "parenExpr", "literal", "call", "variable"
  )

# prefixOp → PrefixOperator atom
rule "prefixOp":
  var opLen = 0
  if input.peekAny(RodPrefixOps, opLen):
    let opNode = RodNode(pos: pos(input),
      kind: rnOperator, op: input.consume(opLen))
    ign()
    result = input.node(rnPrefix,
      [
        opNode,
        exec "atom"
      ]
    )
  else:
    result = exec "atom"

# infix(higher, Op) → higher (Op higher)*
template infixOps(name: string, higher: string, ops: openarray[string]): untyped =
  rule name:
    var
      chain = @[exec higher]
      opLen = 0
    ign()
    while input.peekAny(ops, opLen):
      let op = input.consume()
      ign()
      let next = exec higher
      chain.add([next, RodNode(pos: pos(input), kind: rnOperator, op: op)])
    if chain.len == 1: result = chain[0]
    else: result = input.node(rnInfix, chain)

rule "assign":
  let left = exec "infixOr"
  ign()
  if input.match("="):
    ign()
    let right = exec "infixOr"
    result = input.node(rnAssign, [left, right])
  else:
    result = left

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
  result = exec "assign"

rule "expr":
  result = input.firstMatch("infixOp")

rule "parenExpr":
  if input.match("("):
    ign()
    let expression = exec "expr"
    ign()
    if input.match(")"):
      result = expression

rule "exprStmt":
  let expression = exec "expr"
  if expression.kind != rnEmpty:
    result = input.node(rnStmt, [expression])
    ign()
    if not input.match(";"):
      input.error("Expected a semicolon ';'")

rule "let":
  if input.match("let"):
    if ign():
      var flags = 0'u8
      flags += uint8(input.match("mut")) shl 0
      ign()
      var assignments = @[exec "assign"]
      ign()
      while input.match(","):
        ign()
        assignments.add(exec "assign")
      ign()
      if input.match(";"):
        result = input.node(rnLet,
          [
            input.flags(flags),
            input.node(rnList, assignments)
          ])
      else:
        input.error("Expected a semicolon ';'")

rule "stmt":
  result = input.firstMatch("let", "block", "exprStmt")

rule "block":
  if input.match("{"):
    var statements: seq[RodNode]
    ign()
    while not input.match("}"):
      statements.add(exec "stmt")
      ign()
    result = input.node(rnBlock, statements)

rule "decl":
  result = input.firstMatch("stmt")

rule "script":
  var
    statements: seq[RodNode]
    prevPos: int
  while not input.finished:
    prevPos = input.pos
    ign()
    statements.add(input.firstMatch("decl", "eof"))
    ign()
    if input.pos == prevPos:
      input.error("Unexpected input: '" & input.peek() & "'")
  ign()
  result = RodNode(kind: rnScript, children: statements)

proc parse*(input: string, parser: string = "script"): RodNode =
  var input = StringStream(input: input)
  result = exec parser
