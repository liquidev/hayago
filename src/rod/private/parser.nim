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
    nkNumber, nkIdent
    nkPrefix = "prefix", nkInfix = "infix"
    nkVar, nkLet
  Node* = ref object
    ln*, col*: int
    file*: string
    case kind*: NodeKind
    of nkNumber:
      numberVal*: float
    of nkIdent:
      ident*: string
    else:
      children*: seq[Node]

const LeafNodes = {nkNumber, nkIdent, nkPrefix, nkInfix}

proc `[]`*(node: Node, index: int): Node =
  result = node.children[index]

proc `$`*(node: Node): string =
  case node.kind
  of nkNumber: result = $node.numberVal
  of nkIdent: result = node.ident
  else:
    result = "(" & (
      case node.kind
      of nkPrefix, nkInfix: ""
      else: $node.kind & " ")
    for i, child in node.children:
      if node.kind notin LeafNodes and child.children.len > 1:
        result.add("\n")
        result.add(indent($child, 2))
      else:
        if i > 0:
          result.add(" ")
        result.add($child)
    result.add(")")

template ruleGuard(body) =
  body
  result.ln = scan.ln
  result.col = scan.col
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

proc parsePrefix*(token: Token): Node {.rule.} =
  case token.kind
  of tokNumber: result = Node(kind: nkNumber, numberVal: token.numberVal)
  of tokIdent: result = Node(kind: nkIdent, ident: token.ident)
  of tokOperator: result = Node(kind: nkPrefix,
                                children: @[Node(kind: nkIdent,
                                                 ident: token.operator),
                                            parsePrefix(scan, scan.next())])
  else: scan.error("Prefix expected, got " & $token.kind)

proc parseInfix*(left: Node, token: Token): Node {.rule.} =
  case token.kind
  of tokOperator:
    if token.operator notin ["not", "->", "$"]:
      result = Node(kind: nkInfix,
                    children: @[Node(kind: nkIdent, ident: token.operator),
                                left, parseExpr(scan, token.prec)])
  else: scan.error("Infix expected, got " & $token.kind)

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

proc parseStmt*(): Node {.rule.} =
  result =
    case scan.peek().kind
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
    else: parseExpr(scan)
