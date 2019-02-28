#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

import deques
import macros

import scanner

type
  RodNodeKind* = enum
    #~ terminal nodes
    rnkNone
    rnkNull, rnkBool, rnkNum, rnkStr
    rnkOp
    #~ non-terminal nodes
    # generic
    rnkList
    # operations
    rnkPrefix, rnkInfix
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

converter isNotNone(node: RodNode): bool =
  result = node.kind != rnkNone

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
  scan.ignore()
  body
  scan.ignore()
  if result.kind == rnkNone:
    scan.pos = pos
  else:
    result.pos = pos

macro rule(body: untyped): untyped =
  ## The rule macro adds a `scan: var RodScanner` parameter to the target proc
  ## and wraps its body in a `sandbox` call. It also sets ``RodNode`` as the
  ## proc's return type.
  ## The macro is supposed to be used as a pragma ``{.rule.}`` .
  let
    procStmts = body[6]
    procArgs = body[3]
    sandboxCall = newCall(ident("sandbox"), procStmts)
    newStmts = newStmtList(sandboxCall)
  procArgs[0] = ident("RodNode")
  procArgs.insert(1,
    newIdentDefs(
      ident("scan"),
      newNimNode(nnkVarTy)
        .add(ident("RodScanner"))))
  # support forward declarations:
  if procStmts.kind != nnkEmpty: body[6] = newStmts
  return body

proc parseLiteral*() {.rule.} =
  var atom: RodToken
  if scan.expect(atom, rtkNull, rtkBool, rtkNum, rtkStr):
    case atom.kind
    of rtkNull: result = nullNode()
    of rtkBool: result = boolNode(atom.boolVal)
    of rtkNum: result = numNode(atom.numVal)
    of rtkStr: result = strNode(atom.strVal)
    else: discard

proc parseExpr*() {.rule.}

proc parseAtom*() {.rule.} =
  result = node(rnkNone)
  result = scan.parseLiteral()
  if not result:
    var lpToken: RodToken
    if scan.expect(lpToken, rtkLParen):
      result = scan.parseExpr()
      var rpToken: RodToken
      if not scan.expect(rpToken, rtkRParen):
        scan.err("Missing right paren ')'")

proc parsePrefix*() {.rule.} =
  var opToken: RodToken
  if scan.expect(opToken, rtkOp):
    let
      opNode = opNode(opToken)
      rightNode = scan.parseAtom()
    result = node(rnkPrefix, rightNode, opNode)
  else:
    result = scan.parseAtom()

proc parseInfix*() {.rule.} =
  ## Infix operations are parsed to reverse Polish notation using the
  ## shunting-yard algorithm. Read more at:
  ## https://en.wikipedia.org/wiki/Shunting-yard_algorithm
  ## This implementation is largely simplified, as the rest of the recursive
  ## descent parser already does its job with parsing function calls and
  ## parenthesized expressions.
  var
    queue = @[scan.parsePrefix()]
    opStack = initDeque[RodToken]()
    nextAtom: RodNode
    nextOp: RodToken
    wasOp: bool
  while true: # nim devs, add `do...while`, please. `while true` looks scary!
    if not wasOp and scan.expect(nextOp, rtkOp):
      while opStack.len > 0 and
            (nextOp.leftAssoc and nextOp.prec <= opStack.peekLast().prec or
             not nextOp.leftAssoc and nextOp.prec < opStack.peekLast().prec):
        queue.add(opNode(opStack.popLast()))
      opStack.addLast(nextOp)
      wasOp = true
      continue
    wasOp = false
    nextAtom = scan.parsePrefix() # I know, I said atom, excuse that
    if nextAtom.kind != rnkNone:
      queue.add(nextAtom)
      continue
    break
  while opStack.len > 0:
    queue.add(opNode(opStack.popLast()))
  if queue.len == 0: result = node(rnkNone)
  elif queue.len == 1: result = queue[0]
  else:
    result = node(rnkInfix, queue)

proc parseExpr*() {.rule.} =
  result = scan.parseInfix()
