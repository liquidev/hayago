#~~
# the rod programming language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#~~

## rod uses a recursive descent parser. It's very straightforward, but it isn't
## the fastest in its class—many optimizations could be done (like a full
## table-based Pratt parser for expressions), but that's out of scope for now.

import deques
import macros

import scanner
import strutils

export scanner.TextPos

#~~
# Nodes
#~~

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
    rnkField, rnkMethod
    # flow control
    rnkIf, rnkIfBranch
    rnkLoop, rnkWhile
    rnkBreak, rnkContinue
    rnkReturn
    # variables
    rnkVar
    # declarations
    rnkLet, rnkFn
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
  RodExprNodes* = {
    rnkNull, rnkBool, rnkNum, rnkStr,
    rnkPrefix, rnkInfix, rnkAssign,
    rnkCall,
    rnkIf,
    rnkVar
  }

proc `$`*(node: RodNode, pretty = true, showPos = false): string =
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
        let stringified = `$`(node, pretty, showPos)
        if stringified[0] == '(':
          result.add("\n")
          result.add(indent(`$`(node, pretty, showPos), 2))
        else:
          result.add(" ")
          result.add(stringified)
      else:
        result.add(" ")
        result.add(`$`(node, pretty, showPos))
    result.add(")")
  if showPos:
    result.add(" at " & $node.textPos.ln & ":" & $node.textPos.col)

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

proc by*(node: RodNode, scan: RodScanner): RodNode =
  result = node
  result.pos = scan.pos
  result.textPos = scan.textPos

proc methodNode*(scan: RodScanner, target: RodNode,
                 name: string, args: varargs[RodNode]): RodNode =
  result = node(rnkMethod,
    target,
    identNode(name).by(scan), node(rnkList, args).by(scan))
  .by(scan)

#~~
# The parser
#~~

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
  ## Adds a `scan: var RodScanner` parameter to the target proc and wraps its \
  ## body in a ``sandbox`` call. It also sets the proc's return type to \
  ## ``RodNode``.
  ## The macro is supposed to be used as a pragma (``{.rule.}``).
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

proc delim(scan: var RodScanner,
           start, del, fin: RodTokenKind,
           destKind: RodNodeKind,
           parser: proc (scan: var RodScanner): RodNode): RodNode =
  if scan.expectLit(start):
    var elem = @[parser(scan)]
    while scan.expectLit(del) and not scan.atEnd():
      elem.add(parser(scan))
      if scan.expectLit(fin):
        break
    if elem.len > 0 and elem[0]:
      result = node(destKind, elem).by(scan)
    else:
      result = node(destKind).by(scan)
  else:
    result = emptyNode().by(scan)

proc parseLiteral*() {.rule.} =
  var atom: RodToken
  if scan.expect(atom, [rtkNum, rtkStr]):
    case atom.kind
    of rtkNum: result = numNode(atom.numVal)
    of rtkStr: result = strNode(atom.strVal)
    else: discard
  elif scan.expectKw(rtkNull):
    result = nullNode()
  elif scan.expectKw(rtkTrue):
    result = boolNode(true)
  elif scan.expectKw(rtkFalse):
    result = boolNode(false)

proc parseIdent*() {.rule.} =
  var identToken: RodToken
  if scan.expect(identToken, [rtkIdent]):
    result = identNode(identToken.ident)

proc parseVar*() {.rule.} =
  var identToken: RodToken
  if scan.expect(identToken, [rtkIdent]):
    result = node(rnkVar, identNode(identToken.ident))

proc parseExpr*(prec: int) {.rule.}

proc parseExpr*() {.rule.} =
  result = scan.parseExpr(0)

type
  BlockParseMode = enum
    bpExpr
    bpStmt
    bpFn

proc parseBlock*(mode: static[BlockParseMode]) {.rule.}

proc parseDo*() {.rule.} =
  if scan.expectKw(rtkDo):
    result = scan.parseBlock(bpExpr)
    if not result:
      scan.err("Missing block in do block")

# Gosh, I went through 3 different iterations of if statement parsing
# until I landed on this one. The main advantage is that it's really small,
# and pretty fast (the other ones' performance didn't satisfy me)
proc parseIf*(isStmt: static[bool], allowElse: static[bool] = false) {.rule.} =
  if scan.expectKw(rtkIf):
    var ifStmt = @[
      scan.parseExpr(),
      scan.parseBlock(
        if isStmt: bpStmt
        else: bpExpr
      )
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
    var args = scan.delim(rtkLParen, rtkComma, rtkRParen, rnkList, parseExpr)
    if args:
      result = scan.parseCall(node(rnkCall, left, args))
    else:
      scan.err("Missing right paren ')'")
  else:
    result = left

proc parseAccess*(left: RodNode) {.rule.} =
  if scan.expectOp(rtkDot):
    let fieldName = scan.parseIdent()
    if fieldName:
      let field = node(rnkField, left, fieldName)
      let call = scan.parseCall(field)
      if call.kind == rnkCall:
        result = scan.parseAccess(
          node(rnkMethod, left, fieldName, call[1]).by(scan))
      else:
        result = field
    else:
      scan.err("Field access or method call expected")
  else:
    result = left

proc parsePrefix*() {.rule.} =
  var opToken: RodToken
  if scan.expect(opToken, [rtkOp]):
    if not scan.ignore():
      let
        opNode = opNode(opToken)
        rightNode = scan.parseAccess(scan.parseCall(scan.parseAtom()))
      result = node(rnkPrefix, rightNode, opNode)
  else:
    result = scan.parseAccess(scan.parseCall(scan.parseAtom()))

# I can't believe how Pratt parsing can be done in this few lines of code.
# rod's Pratt parser consists of two following functions. Unfortunately, a more
# optimized table-based solution is impossible, because rod uses context-based
# scanning, which doesn't play well with Pratt parsing.
# A better, faster solution might be considered in the future.

proc parseInfix*(left: RodNode, op: RodToken) {.rule.} =
  let right = scan.parseExpr(op.prec - (1 - ord(op.leftAssoc)))
  result = node(rnkInfix, left, opNode(op), right)

proc parseExpr*(prec: int) {.rule.} =
  var op: RodToken
  result = scan.parsePrefix()
  discard scan.expect(op, [rtkOp], peek = true)
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
  if scan.expectOp(rtkEq):
    let right = scan.parseExpr()
    if right.kind != rnkNone:
      result = node(rnkAssign, left, right)
  else:
    result = left

proc parseLet*() {.rule.} =
  if scan.expectKw(rtkLet):
    var assignments: seq[RodNode]
    while true:
      let left = scan.parseVar()
      if not left:
        scan.err("Variable name expected")
      if scan.expectOp(rtkEq):
        let right = scan.parseExpr()
        if not right:
          scan.err("Expression expected")
        assignments.add(node(rnkAssign, left, right))
      else:
        assignments.add(node(rnkAssign, left, nullNode()))
      if not scan.expectLit(rtkComma):
        break
    if assignments.len > 0:
      if scan.expectLit(rtkEndStmt):
        result = node(rnkLet, assignments)
      else:
        scan.err("Semicolon ';' expected after variable declaration")
    else:
      scan.err("Assignments expected")

proc parseFn*() {.rule.} =
  if scan.expectKw(rtkFn):
    let fnName = scan.parseIdent()
    if not fnName: scan.err("Function name expected")
    let args = scan.delim(rtkLParen, rtkComma, rtkRParen, rnkList, parseExpr)
    if not args: scan.err("Function arguments expected")
    let impl = scan.parseBlock(bpFn)
    if not impl: scan.err("Function implementation expected")
    result = node(rnkFn, fnName, args, impl)

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

proc parseReturn*() {.rule.} =
  if scan.expectKw(rtkReturn):
    let exp = scan.parseExpr()
    if exp:
      result = node(rnkReturn, exp)
    else:
      result = node(rnkReturn)
    if not scan.expectLit(rtkEndStmt):
      scan.err("Semicolon ';' expected after return statement")

proc parseLoop*() {.rule.} =
  if scan.expectKw(rtkLoop):
    let loopBlock = scan.parseBlock(bpStmt)
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
    let loopBlock = scan.parseBlock(bpStmt)
    if not loopBlock:
      scan.err("Missing block in while loop")
    result = node(rnkWhile, loopCheck, loopBlock)

proc parseFor*() {.rule.} =
  ## A ``for`` loop is just some syntactic sugar over a ``while`` loop.
  ## This is why it produces a block node, and not a distinct for node.
  if scan.expectKw(rtkFor):
    let loopLocal = scan.parseVar()
    if not loopLocal:
      scan.err("Missing local in for loop")
    if scan.expectKw(rtkIn):
      let loopIter = scan.parseExpr()
      if not loopIter:
        scan.err("Missing iterator in for loop")
      let iterV = node(rnkVar, identNode(":iter")).by(scan)
      var loopBlock = scan.parseBlock(bpStmt)
      loopBlock.sons.insert(
        node(rnkLet, node(rnkAssign, loopLocal,
          scan.methodNode(iterV, "get"))
        ).by(scan),
        0
      )
      loopBlock.sons.add(
        node(rnkStmt, scan.methodNode(iterV, "next")).by(scan)
      )
      result = node(rnkBlock,
        node(rnkLet, node(rnkAssign, iterV,
          scan.methodNode(loopIter, "iterate")).by(scan)).by(scan),
        node(rnkWhile,
          scan.methodNode(iterV, "has_next"),
          loopBlock).by(scan)
      )
    else:
      scan.err("Missing 'in' in for loop")

proc parseStmt*() {.rule.} =
  result = scan.parseIf(true)
  if not result: result = scan.parseLoop()
  if not result: result = scan.parseWhile()
  if not result: result = scan.parseFor()
  if not result: result = scan.parseBreak()
  if not result: result = scan.parseContinue()
  if not result: result = scan.parseReturn()
  if not result:
    result = scan.parseAssign()
    if result:
      if not scan.expectLit(rtkEndStmt):
        if not scan.expectLit(rtkRBrace, peek = true):
          scan.err("Semicolon ';' expected after expression statement")
      else:
        result = node(rnkStmt, result)
  if not result: result = scan.parseBlock(bpStmt)

proc parseDecl*() {.rule.} =
  result = scan.parseLet()
  if not result: result = scan.parseStmt()

proc parseBlock*(mode: static[BlockParseMode]) {.rule.} =
  if scan.expectLit(rtkLBrace):
    var nodes: seq[RodNode]
    while not scan.atEnd():
      let decl = scan.parseDecl()
      nodes.add(decl)
      if scan.expectLit(rtkRBrace):
        break
    when mode == bpExpr:
      if nodes[^1].kind notin RodExprNodes:
        scan.err("Block must have a result")
    elif mode == bpFn:
      if nodes[^1].kind in RodExprNodes:
        nodes[^1] = node(rnkReturn, nodes[^1])
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
