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
  Node* = ref object
    ln*, col*: int
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
      if child.kind notin LeafNodes and child.children.len > 1:
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
  else: discard

proc parseInfix*(left: Node, token: Token): Node {.rule.} =
  case token.kind
  of tokOperator:
    if token.operator != "not":
      result = Node(kind: nkInfix,
                    children: @[Node(kind: nkIdent, ident: token.operator),
                                left, parseExpr(scan, token.prec)])
  else: discard

proc parseExpr*(prec = 0): Node {.rule.} =
  var token = scan.next()
  result = parsePrefix(scan, token)
  while prec < precedence(scan.peek()):
    token = scan.next()
    if token.kind == tokEnd:
      break
    result = parseInfix(scan, result, token)
