#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

import macros

import scanner

type
  RodNodeKind* = enum
    # terminal nodes
    rnkNone
    rnkNull, rnkBool, rnkNum, rnkStr
    rnkOp
    # non-terminal nodes
    rnkList
  RodNode* = ref object
    case kind*: RodNodeKind
    # terminal nodes
    of rnkBool: boolVal*: bool
    of rnkNum:  numVal*: float
    of rnkStr:  strVal*: string
    of rnkOp:   opToken*: RodToken
    # non-terminal nodes
    else: sons*: seq[RodNode]
    pos*: int
  RodBranchNodeKind* = concept k
    RodNode(kind: k).sons is seq[RodNode]

proc `$`*(node: RodNode): string =
  case node.kind
  of rnkNull: result = "#null"
  of rnkBool: result = "#" & $node.boolVal
  of rnkNum:  result = $node.numVal
  of rnkStr:  result = '"' & node.strVal & '"'
  of rnkOp:   result = node.opToken.op
  else:
    result.add("(")
    result.add(($node.kind)[3..<len($node.kind)])
    for node in node.sons:
      result.add(" ")
      result.add($node)
    result.add(")")

proc emptyNode*(): RodNode = RodNode(kind: rnkNone)

proc nullNode*(): RodNode = RodNode(kind: rnkNull)

proc boolNode*(val: bool): RodNode = RodNode(kind: rnkBool, boolVal: val)

proc numNode*(val: float): RodNode = RodNode(kind: rnkNum, numVal: val)

proc strNode*(val: string): RodNode = RodNode(kind: rnkStr, strVal: val)

proc opNode*(op: RodToken): RodNode = RodNode(kind: rnkOp, opToken: op)

proc node*(kind: RodBranchNodeKind, children: varargs[RodNode]): RodNode =
  result = RodNode(kind: kind)
  result.sons.add(children)

template sandbox(body: untyped): untyped {.dirty.} =
  let pos = scan.pos
  result = emptyNode()
  body
  if result.kind == rnkNone:
    scan.pos = pos
  else:
    result.pos = pos

macro rule(body: untyped): untyped =
  ## The rule macro adds a `scan: var RodScanner` parameter to the target proc
  ## and wraps its body in a `sandbox` call.
  ## The macro is supposed to be used as a pragma ``{.rule.}`` .
  let
    procStmts = body[6]
    procArgs = body[3]
    sandboxCall = newCall(ident("sandbox"), procStmts)
    newStmts = newStmtList(sandboxCall)
  procArgs.insert(1,
    newIdentDefs(
      ident("scan"),
      newNimNode(nnkVarTy)
        .add(ident("RodScanner"))))
  body[6] = newStmts
  return body

proc parseLiteral*(): RodNode {.rule.} =
  var atom: RodToken
  if scan.expect(atom, rtkNull, rtkBool, rtkNum, rtkStr):
    case atom.kind
    of rtkNull: result = nullNode()
    of rtkBool: result = boolNode(atom.boolVal)
    of rtkNum: result = numNode(atom.numVal)
    of rtkStr: result = strNode(atom.strVal)
    else: discard
