#~~
# the rod programming language
# copyright (C) iLiquid, 2019
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
    rnkScript, rnkBlock, rnkStmt
    # operations
    rnkPrefix, rnkInfix
    rnkAssign
    rnkCall
    # flow control
    rnkIf, rnkIfBranch
    rnkLoop, rnkWhile, rnkFor
    rnkBreak, rnkContinue
    # variables
    rnkVar
    # declarations
    rnkLet
  RodNodeObj {.acyclic.} = object
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
  RodNode* = ref RodNodeObj
  RodBranchNodeKind* = concept k
    RodNode(kind: k).sons is seq[RodNode]

const
  ExprNodes = {
    rnkNull, rnkBool, rnkNum, rnkStr,
    rnkPrefix, rnkInfix, rnkAssign,
    rnkCall,
    rnkIf,
    rnkVar
  }

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
        result.add(`$`(node, pretty))
    result.add(")")

proc `[]`*(node: RodNode, index: int): RodNode =
  node.sons[index]

proc first*(node: RodNode): RodNode =
  node.sons[0]

proc last*(node: RodNode): RodNode =
  node.sons[^1]

converter isNotNone*(node: RodNode): bool =
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
  if scan.expect(atom, [rtkNull, rtkTrue, rtkFalse, rtkNum, rtkStr]):
    case atom.kind
    of rtkNull: result = nullNode()
    of rtkTrue: result = boolNode(true)
    of rtkFalse: result = boolNode(false)
    of rtkNum: result = numNode(atom.numVal)
    of rtkStr: result = strNode(atom.strVal)
    else: discard

proc parseVar*() {.rule.} =
  var identToken: RodToken
  if scan.expect(identToken, [rtkIdent]):
    result = node(rnkVar, identNode(identToken.ident))

proc parseExpr*(prec: int) {.rule.}

proc parseExpr*() {.rule.} =
  result = scan.parseExpr(0)

proc parseBlock*(implicitReturn: static[bool]) {.rule.}

proc parseDo*() {.rule.} =
  if scan.expectKw(rtkDo):
    result = scan.parseBlock(true)
    if not result:
      scan.err("Missing block in do block")

# Gosh, I went through 3 different iterations of if statement parsing
# until I landed on this one. The main advantage is that it's really small,
# and pretty fast (the other ones' performance didn't satisfy me)
proc parseIf*(isStmt: static[bool], allowElse: static[bool] = true) {.rule.} =
  if scan.expectKw(rtkIf):
    var ifStmt = @[
      scan.parseExpr(),
      scan.parseBlock(not isStmt)
    ]
    if not ifStmt[0]: scan.err("If condition expected")
    if not ifStmt[1]: scan.err("If branch expected")
    when allowElse:
      var branches = @[node(rnkIfBranch, ifStmt)]
      while scan.expectKw(rtkElse):
        var branch = scan.parseIf(isStmt, false)
        if not branch: branch = scan.parseBlock(not isStmt)
        if branch: branches.add(branch)
      result = node(rnkIf, branches)
    else:
      result = node(rnkIfBranch, ifStmt)

proc parseAtom*() {.rule.} =
  result = scan.parseLiteral()
  if not result: result = scan.parseIf(false)
  if not result: result = scan.parseDo()
  if not result: result = scan.parseVar()
  if not result:
    if scan.expectLit(rtkLParen):
      result = scan.parseExpr()
      if not scan.expectLit(rtkRParen):
        scan.err("Missing right paren ')'")

proc parseCall*(left: RodNode) {.rule.} =
  if scan.expectLit(rtkLParen):
    var args: seq[RodNode]
    while not scan.atEnd():
      let arg = scan.parseExpr()
      if arg: args.add(arg)
      if not scan.expectLit(rtkComma):
        break
    if scan.expectLit(rtkRParen):
      result = scan.parseCall(node(rnkCall, left, node(rnkList, args)))
    else:
      scan.err("Missing right paren ')'")
  else:
    result = left

proc parsePrefix*() {.rule.} =
  var opToken: RodToken
  if scan.expect(opToken, [rtkOp]):
    if not scan.ignore():
      let
        opNode = opNode(opToken)
        rightNode = scan.parseCall(scan.parseAtom())
      result = node(rnkPrefix, rightNode, opNode)
  else:
    result = scan.parseCall(scan.parseAtom())

# I can't believe how Pratt parsing can be done in this few lines of code.
# https://gist.github.com/liquid600pgm/5d0673f40223a312cc5f91e969660060#parsing
# This gist contains some useful programming resources.
# Check out the 'Pratt parsing' link here for some insights on how it can be
# implemented.

proc parseInfix*(left: RodNode, op: RodToken) {.rule.} =
  let right = scan.parseExpr(op.prec - (1 - ord(op.leftAssoc)))
  result = node(rnkInfix, left, opNode(op), right)

proc parseExpr*(prec: int) {.rule.} =
  var op: RodToken
  result = scan.parsePrefix()
  op = scan.peekToken(rtkOp)
  let nextPrec =
    if op.kind == rtkOp: op.prec
    else: 0
  while prec < nextPrec:
    op = scan.nextToken(rtkOp)
    if op.kind == rtkOp:
      result = scan.parseInfix(result, op)
    else:
      break

proc parseAssign*() {.rule.} =
  let left = scan.parseExpr()
  if scan.expectLit(rtkEq):
    let right = scan.parseExpr()
    if right.kind != rnkNone:
      result = node(rnkAssign, left, right)
  else:
    result = left

proc parseLet*() {.rule.} =
  # ahh, variable declarations. so simple yet so complex
  if scan.expectKw(rtkLet):
    var assignments: seq[RodNode]
    while true:
      let assign = scan.parseAssign()
      var left, right: RodNode
      if assign.kind != rnkNone:
        if assign.kind == rnkVar:
          left = assign
          right = nullNode()
        elif assign.kind == rnkAssign:
          if assign[0].kind != rnkVar:
            scan.err(
              "Left-hand side of variable assignment must be an identifier")
          left = assign[0]
          right = assign[1]
        else:
          scan.err("Identifier expected")
      assignments.add(node(rnkAssign, left, right))
      if not scan.expectLit(rtkComma):
        break
    if scan.expectLit(rtkEndStmt):
      result = node(rnkLet, assignments)
    else:
      scan.err("Semicolon ';' expected after variable declaration")

proc parseBreak*() {.rule.} =
  if scan.expectKw(rtkBreak):
    if scan.expectLit(rtkEndStmt):
      result = node(rnkBreak)
    else:
      scan.err("Semicolon ';' expected after break statement")

proc parseContinue*() {.rule.} =
  if scan.expectKw(rtkContinue):
    if scan.expectLit(rtkEndStmt):
      result = node(rnkContinue)
    else:
      scan.err("Semicolon ';' expected after continue statement")

proc parseLoop*() {.rule.} =
  if scan.expectKw(rtkLoop):
    let loopBlock = scan.parseBlock(false)
    if not loopBlock:
      scan.err("Missing block in loop statement")
    if scan.expectKw(rtkWhile):
      let loopCheck = scan.parseExpr()
      if not loopCheck:
        scan.err("Missing condition in loop...while statement")
      if scan.expectLit(rtkEndStmt):
        result = node(rnkLoop, loopBlock, loopCheck)
      else:
        scan.err("Semicolon ';' expected after loop...while statement")
    else:
      result = node(rnkLoop, loopBlock)

proc parseWhile*() {.rule.} =
  if scan.expectKw(rtkWhile):
    let loopCheck = scan.parseExpr()
    if not loopCheck:
      scan.err("Missing condition in while loop")
    let loopBlock = scan.parseBlock(false)
    if not loopBlock:
      scan.err("Missing block in while loop")
    result = node(rnkWhile, loopCheck, loopBlock)

proc parseFor*() {.rule.} =
  if scan.expectKw(rtkFor):
    let loopLocal = scan.parseVar()
    if not loopLocal:
      scan.err("Missing local in for loop")
    if scan.expectKw(rtkIn):
      let loopIter = scan.parseExpr()
      if not loopIter:
        scan.err("Missing iterator in for loop")

proc parseStmt*() {.rule.} =
  result = scan.parseIf(true)
  if not result: result = scan.parseLoop()
  if not result: result = scan.parseWhile()
  if not result: result = scan.parseBreak()
  if not result: result = scan.parseContinue()
  if not result:
    result = scan.parseAssign()
    if result:
      if not (scan.expectLit(rtkEndStmt)):
        if scan.peekToken([rtkRBrace]).kind != rtkRBrace:
          scan.err("Semicolon ';' expected after expression statement")
      else:
        result = node(rnkStmt, result)
  if not result: result = scan.parseBlock(false)

proc parseDecl*() {.rule.} =
  result = scan.parseLet()
  if not result: result = scan.parseStmt()

proc parseBlock*(implicitReturn: static[bool]) {.rule.} =
  if scan.expectLit(rtkLBrace):
    var nodes: seq[RodNode]
    while not scan.atEnd():
      let decl = scan.parseDecl()
      nodes.add(decl)
      if scan.expectLit(rtkRBrace):
        break
    when implicitReturn:
      if nodes[^1].kind notin ExprNodes:
        scan.err("Block must have a result")
    result = node(rnkBlock, nodes)

proc parseScript*() {.rule.} =
  var
    nodes: seq[RodNode]
    guard = 0
  while not scan.atEnd():
    nodes.add(scan.parseDecl())
    if scan.pos != guard:
      guard = scan.pos
    else:
      scan.err("Unexpected character '" & scan.peek(1) & "'")

  result = node(rnkScript, nodes)
