#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

import deques
import macros

import scanner
import strutils

export scanner.TextPos

type
  RodNodeKind* = enum
    #~ terminal nodes
    rnkNone
    rnkNull, rnkBool, rnkNum, rnkStr
    rnkOp, rnkIdent
    #~ non-terminal nodes
    # generic
    rnkList
    rnkScript, rnkStmt
    # operations
    rnkPrefix, rnkInfix
    rnkAssign
    # variables
    rnkVar
    # declarations
    rnkLet
  RodNode* = ref object
    case kind*: RodNodeKind
    # terminal nodes
    of rnkBool:  boolVal*: bool
    of rnkNum:   numVal*: float
    of rnkStr:   strVal*: string
    of rnkOp:    opToken*: RodToken
    of rnkIdent: ident*: string
    # non-terminal nodes
    else: sons*: seq[RodNode]
    pos*: int
    textPos*: TextPos
  RodBranchNodeKind* = concept k
    RodNode(kind: k).sons is seq[RodNode]

proc `$`*(node: RodNode, pretty: bool = true): string =
  case node.kind
  of rnkNull:  result = "#null"
  of rnkBool:  result = "#" & $node.boolVal
  of rnkNum:   result = $node.numVal
  of rnkStr:   result = '"' & node.strVal & '"'
  of rnkOp:    result = node.opToken.op
  of rnkIdent: result = node.ident
  else:
    result.add("(")
    result.add(($node.kind)[3..<len($node.kind)])
    for node in node.sons:
      if pretty:
        let stringified = `$`(node, pretty)
        if stringified[0] == '(':
          result.add("\n")
          result.add(indent(`$`(node, pretty), 2))
        else:
          result.add(" ")
          result.add(stringified)
      else:
        result.add(" ")
        result.add($node)
    result.add(")")

proc `[]`*(node: RodNode, index: int): RodNode =
  node.sons[index]

converter isNotNone(node: RodNode): bool =
  result = node.kind != rnkNone

proc emptyNode*(): RodNode = RodNode(kind: rnkNone)

proc nullNode*(): RodNode = RodNode(kind: rnkNull)

proc boolNode*(val: bool): RodNode = RodNode(kind: rnkBool, boolVal: val)

proc numNode*(val: float): RodNode = RodNode(kind: rnkNum, numVal: val)

proc strNode*(val: string): RodNode = RodNode(kind: rnkStr, strVal: val)

proc opNode*(op: RodToken): RodNode = RodNode(kind: rnkOp, opToken: op)

proc identNode*(id: string): RodNode = RodNode(kind: rnkIdent, ident: id)

proc node*(kind: RodBranchNodeKind, children: varargs[RodNode]): RodNode =
  result = RodNode(kind: kind)
  result.sons.add(children)

template sandbox(body: untyped): untyped {.dirty.} =
  let
    pos = scan.pos
    textPos = scan.textPos
  result = emptyNode()
  scan.ignore()
  body
  scan.ignore()
  if result.kind == rnkNone:
    scan.pos = pos
  else:
    result.pos = pos
    result.textPos = textPos

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
  if scan.expect(atom, [rtkNull, rtkBool, rtkNum, rtkStr]):
    case atom.kind
    of rtkNull: result = nullNode()
    of rtkBool: result = boolNode(atom.boolVal)
    of rtkNum: result = numNode(atom.numVal)
    of rtkStr: result = strNode(atom.strVal)
    else: discard

proc parseVar*() {.rule.} =
  var identToken: RodToken
  if scan.expect(identToken, [rtkIdent]):
    result = node(rnkVar, identNode(identToken.ident))

proc parseExpr*() {.rule.}

proc parseAtom*() {.rule.} =
  result = node(rnkNone)
  result = scan.parseLiteral()
  if not result: result = scan.parseVar()
  if not result:
    if scan.expect([rtkLParen]):
      result = scan.parseExpr()
      if not scan.expect([rtkRParen]):
        scan.err("Missing right paren ')'")

proc parsePrefix*() {.rule.} =
  var opToken: RodToken
  if scan.expect(opToken, [rtkOp]):
    if not scan.ignore():
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
    if not wasOp and scan.expect(nextOp, [rtkOp]):
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

proc parseAssign*() {.rule.} =
  let left = scan.parseExpr()
  if scan.expect([rtkEq]):
    let right = scan.parseExpr()
    if right.kind != rnkNone:
      result = node(rnkAssign, left, right)
  else:
    result = left

proc parseLet*() {.rule.} =
  if scan.expect([rtkLet]):
    if scan.ignore():
      let assign = scan.parseAssign()
      if assign[0].kind != rnkVar:
        scan.err("Left-hand side of variable declaration must be an identifier")
      if scan.expect([rtkEndStmt]):
        result = node(rnkLet, assign[0], assign[1])
      else:
        scan.err("Semicolon ';' expected after variable declaration")

proc parseExpr*() {.rule.} =
  result = scan.parseInfix()

proc parseStmt*() {.rule.} =
  result = scan.parseExpr()
  if result:
    if not (scan.expect([rtkEndStmt]) or scan.peekBack().kind == rtkRBrace):
      scan.err("Semicolon ';' expected after expression statement")
    result = node(rnkStmt, result)

proc parseDecl*() {.rule.} =
  result = scan.parseLet()
  if not result: result = scan.parseStmt()

proc parseScript*() {.rule.} =
  var nodes: seq[RodNode]
  while not scan.atEnd():
    nodes.add(scan.parseDecl())

  result = node(rnkScript, nodes)
