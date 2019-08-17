#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import macros
import strutils

import scanner

type
  NodeKind* = enum
    nkScript, nkBlock
    nkBool, nkNumber, nkString, nkIdent
    nkPrefix, nkInfix
    nkVar, nkLet
    nkIf, nkWhile, nkFor
  Node* = ref object
    ln*, col*: int
    file*: string
    case kind*: NodeKind
    of nkBool:
      boolVal*: bool
    of nkNumber:
      numberVal*: float
    of nkString:
      stringVal*: string
    of nkIdent:
      ident*: string
    else:
      children*: seq[Node]

const LeafNodes = {nkBool, nkNumber, nkString, nkIdent, nkPrefix, nkInfix}

proc `[]`*(node: Node, index: int): Node =
  result = node.children[index]

proc `$`*(node: Node, showLineInfo = false): string =
  case node.kind
  of nkBool: result = $node.boolVal
  of nkNumber: result = $node.numberVal
  of nkString: result = escape(node.stringVal)
  of nkIdent: result = node.ident
  else:
    result = (if showLineInfo: $node.ln & ":" & $node.col & " " else: "") &
             "(" & (case node.kind
                    of nkPrefix, nkInfix: ""
                    else: $node.kind & " ")
    for i, child in node.children:
      if child.kind notin LeafNodes and node.children.len > 1:
        result.add("\n")
        result.add(indent(`$`(child, showLineInfo), 2))
      else:
        if i > 0:
          result.add(" ")
        result.add(`$`(child, showLineInfo))
    result.add(")")

template ruleGuard(body) =
  let
    ln = scan.ln
    col = scan.col
  body
  result.ln = ln
  result.col = col
  result.file = scan.file

macro rule(pc) =
  pc[3].insert(1,
    newIdentDefs(ident"scan", newNimNode(nnkVarTy).add(ident"Scanner")))
  if pc[6].kind != nnkEmpty:
    pc[6] = newCall("ruleGuard", newStmtList(pc[6]))
  result = pc

proc precedence(token: Token): int =
  result =
    case token.kind
    of tokOperator: token.prec
    else: 0

proc parseExpr*(prec = 0): Node {.rule.}

proc parseParExpr*(): Node {.rule.} =
  result = parseExpr(scan)
  if scan.next().kind != tokRPar:
    scan.error("Right paren ')' expected")

proc parseBlock*(): Node {.rule.}

proc parseIf*(): Node {.rule.} =
  var children = @[
    parseExpr(scan),
    parseBlock(scan)
  ]
  while scan.peek().kind == tokElif:
    discard scan.next()
    children.add([
      parseExpr(scan),
      parseBlock(scan)
    ])
  if scan.peek().kind == tokElse:
    discard scan.next()
    children.add(parseBlock(scan))
  result = Node(kind: nkIf, children: children)

proc parsePrefix*(token: Token): Node {.rule.} =
  case token.kind
  of tokTrue: result = Node(kind: nkBool, boolVal: true)
  of tokFalse: result = Node(kind: nkBool, boolVal: false)
  of tokNumber: result = Node(kind: nkNumber, numberVal: token.numberVal)
  of tokString: result = Node(kind: nkString, stringVal: token.stringVal)
  of tokIdent: result = Node(kind: nkIdent, ident: token.ident)
  of tokOperator: result = Node(kind: nkPrefix,
                                children: @[Node(kind: nkIdent,
                                                 ident: token.operator),
                                            parsePrefix(scan, scan.next())])
  of tokLPar: result = parseParExpr(scan)
  of tokIf: result = parseIf(scan)
  else: scan.error("Unexpected token: " & $token.kind)

proc parseInfix*(left: Node, token: Token): Node {.rule.} =
  case token.kind
  of tokOperator:
    if token.operator notin ["not", "->", "$"]:
      result = Node(kind: nkInfix,
                    children: @[Node(kind: nkIdent, ident: token.operator),
                                left, parseExpr(scan, token.prec)])
  else: scan.error("Unexpected token: " & $token.kind)

proc parseExpr*(prec = 0): Node {.rule.} =
  var token = scan.next()
  result = parsePrefix(scan, token)
  if result == nil:
    scan.error("Unexpected token: " & $token.kind)
  while prec < precedence(scan.peek()):
    token = scan.next()
    if token.kind == tokEnd:
      break
    result = parseInfix(scan, result, token)

proc parseWhile*(): Node {.rule.} =
  discard scan.next()
  let
    cond = parseExpr(scan)
    body = parseBlock(scan)
  result = Node(kind: nkWhile, children: @[cond, body])

proc parseStmt*(): Node {.rule.} =
  result =
    case scan.peek().kind
    of tokLBrace: parseBlock(scan)
    of tokVar, tokLet:
      var node = Node(kind: if scan.next().kind == tokVar: nkVar
                            else: nkLet)
      while true:
        let name = scan.expect(tokIdent)
        scan.expectOp("=")
        let val = scan.parseExpr()
        node.children.add(Node(kind: nkInfix, children: @[
                            Node(kind: nkIdent, ident: "="),
                            Node(kind: nkIdent, ident: name.ident), val]))
        if scan.peek().kind == tokComma:
          discard scan.next()
          continue
        break
      if node.children.len < 1:
        scan.error("Variable declaration expected")
      node
    of tokWhile: parseWhile(scan)
    else: parseExpr(scan)

proc parseBlock*(): Node {.rule.} =
  if scan.next().kind != tokLBrace:
    scan.error("Left brace '{' expected")
  var stmts: seq[Node]
  while scan.peek().kind != tokRBrace:
    if scan.atEnd:
      scan.error("Missing right brace '}'")
    stmts.add(parseStmt(scan))
    if not scan.linefeed() and scan.peek().kind != tokRBrace:
      scan.error("Line feed expected after statement")
  discard scan.next()
  result = Node(kind: nkBlock, children: stmts)

proc parseScript*(): Node {.rule.} =
  var stmts: seq[Node]
  while not scan.atEnd:
    stmts.add(parseStmt(scan))
    if not scan.linefeed():
      scan.error("Line feed expected after statement")
  result = Node(kind: nkScript, children: stmts)
