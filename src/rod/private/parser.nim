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
    nkEmpty
    nkScript, nkBlock
    nkBool, nkNumber, nkString, nkIdent
    nkPrefix, nkInfix, nkDot, nkIndex
    nkVar, nkLet
    nkIf, nkWhile, nkFor
    nkBreak, nkContinue
    nkCall
    nkGeneric
    nkObject, nkObjFields, nkObjConstr
  Node* = ref object
    ln*, col*: int
    file*: string
    case kind*: NodeKind
    of nkEmpty: discard
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

const LeafNodes = {
  nkEmpty, nkBool, nkNumber, nkString, nkIdent, nkPrefix, nkInfix
}

proc `[]`*(node: Node, index: int): Node =
  result = node.children[index]

proc `$`*(node: Node, showLineInfo = false): string =
  case node.kind
  of nkEmpty: result = "<empty>"
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
    of tokLBrk, tokDot, tokLPar: 10
    else: 0

proc parseExpr(prec = 0): Node {.rule.}

proc parseParExpr(): Node {.rule.} =
  result = parseExpr(scan)
  if scan.next().kind != tokRPar:
    scan.error("Right paren ')' expected")

proc parseBlock(): Node {.rule.}

proc parseIf(): Node {.rule.} =
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

proc parseType(typeExpr: Node): Node {.rule.} =
  if typeExpr.kind == nkIdent:
    result = typeExpr
  elif typeExpr.kind == nkIndex:
    if typeExpr[0].kind != nkIdent:
      scan.error("Generic type must begin with type name")
    var params: seq[Node]
    for n in typeExpr.children:
      params.add(parseType(scan, n))
    result = Node(kind: nkGeneric, children: params)
  else:
    scan.error("Type expected")

proc parsePrefix(token: Token): Node {.rule.} =
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

proc parseInfix(left: Node, token: Token): Node {.rule.} =
  case token.kind
  of tokOperator: # Binary operator
    if token.operator notin ["not", "->", "$"]:
      result = Node(kind: nkInfix,
                    children: @[Node(kind: nkIdent, ident: token.operator),
                                left, parseExpr(scan, token.prec)])
  of tokLBrk: # Index operator '[]'
    result = Node(kind: nkIndex,
                  children: @[left, parseExpr(scan, 10)])
    while scan.peek().kind == tokComma:
      discard scan.next()
      result.children.add(parseExpr(scan, 10))
    scan.expect(tokRBrk)
  of tokDot: # Dot operator '.'
    result = Node(kind: nkDot,
                  children: @[left, parseExpr(scan, 10)])
  of tokLPar: # Call or object constructor
    if scan.pattern([tokIdent, tokColon]):
      # Object constructor
      var fields = @[parseType(scan, left)]
      while true:
        if scan.atEnd:
          scan.error("Missing right paren ')'")
        let field = scan.expect(tokIdent, "Field name expected")
        scan.expect(tokColon, "Colon ':' expected")
        let value = parseExpr(scan)
        fields.add(Node(kind: nkObjFields, children:
                          @[Node(kind: nkIdent, ident: field.ident),
                            value]))
        let next = scan.next().kind
        case next
        of tokComma: continue
        of tokRPar: break
        else:
          scan.error("Comma ',' or right paren ')' expected")
      result = Node(kind: nkObjConstr, children: fields)
    else:
      # Call (TODO)
      discard
  else: scan.error("Unexpected token: " & $token.kind)

proc parseExpr(prec = 0): Node {.rule.} =
  var token = scan.next()
  result = parsePrefix(scan, token)
  if result == nil:
    scan.error("Unexpected token: " & $token.kind)
  while prec < precedence(scan.peek()):
    token = scan.next()
    if token.kind == tokEnd:
      break
    result = parseInfix(scan, result, token)

proc parseVar(): Node {.rule.} =
  result = Node(kind: if scan.next().kind == tokVar: nkVar
                      else: nkLet)
  while true:
    let name = scan.expect(tokIdent)
    var ty = Node(kind: nkEmpty)
    if scan.peek().kind == tokColon:
      discard scan.next()
      # Parse with prec = 9 to avoid any binary operators
      ty = parseType(scan, parseExpr(scan, prec = 9))
    scan.expectOp("=")
    let val = scan.parseExpr()
    result.children.add(Node(kind: nkInfix, children: @[
                          Node(kind: nkIdent, ident: "="),
                          Node(kind: nkIdent, ident: name.ident), val, ty]))
    if scan.peek().kind == tokComma:
      discard scan.next()
      continue
    break
  if result.children.len < 1:
    scan.error("Variable declaration expected")

proc parseWhile(): Node {.rule.} =
  discard scan.next()
  let
    cond = parseExpr(scan)
    body = parseBlock(scan)
  result = Node(kind: nkWhile, children: @[cond, body])

proc parseObject(): Node {.rule.} =
  discard scan.next()
  let name = parseType(scan, parseExpr(scan))
  scan.expect(tokLBrace)
  var fields: seq[Node]
  fields.add(name)
  while scan.peek().kind != tokRBrace:
    if scan.atEnd:
      scan.error("Missing right brace '}'")
    if scan.peek().kind != tokIdent:
      scan.error("Field name expected")
    var fieldNames: seq[Node]
    while scan.peek().kind == tokIdent:
      fieldNames.add(Node(kind: nkIdent, ident: scan.next().ident))
      var nextTok = scan.next().kind
      case nextTok
      of tokComma: continue
      of tokColon: break
      else: scan.error("Comma ',' or colon ':' expected")
    var ty = parseType(scan, parseExpr(scan))
    fieldNames.add(ty)
    fields.add(Node(kind: nkObjFields, children: fieldNames))
    if not scan.linefeed() and scan.peek().kind != tokRBrace:
      scan.error("Line feed expected after object field")
  discard scan.next()
  result = Node(kind: nkObject, children: fields)

proc parseStmt(): Node {.rule.} =
  result =
    case scan.peek().kind
    of tokLBrace: parseBlock(scan)
    of tokVar, tokLet: parseVar(scan)
    of tokObject: parseObject(scan)
    of tokWhile: parseWhile(scan)
    of tokBreak: Node(kind: nkBreak)
    of tokContinue: Node(kind: nkContinue)
    else: parseExpr(scan)

proc parseBlock(): Node {.rule.} =
  scan.expect(tokLBrace)
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
