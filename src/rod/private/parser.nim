#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

## The parser. Rules are commented with npeg-like syntax.

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
    nkProc
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
  ## Stringify a node into a lisp representation.
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
  ## Helper used by {.rule.} to update line info appropriately for nodes.
  let
    ln = scan.ln
    col = scan.col
  body
  result.ln = ln
  result.col = col
  result.file = scan.file

macro rule(pc) =
  ## Adds a ``scan`` parameter to a proc and wraps its body in a call to
  ## ``ruleGuard``.
  pc[3].insert(1,
    newIdentDefs(ident"scan", newNimNode(nnkVarTy).add(ident"Scanner")))
  if pc[6].kind != nnkEmpty:
    pc[6] = newCall("ruleGuard", newStmtList(pc[6]))
  result = pc

proc precedence(token: Token): int =
  ## Returns the infix precedence of a token.
  result =
    case token.kind
    of tokOperator: token.prec
    of tokLBrk, tokDot, tokLPar: 10
    else: 0

proc parseExpr(prec = 0): Node {.rule.}

proc parseParExpr(): Node {.rule.} =
  ## Parses an expression in parentheses.
  # parExpr <- '(' expr ')'
  result = parseExpr(scan)
  if scan.next().kind != tokRPar:
    scan.error("Right paren ')' expected")

proc parseBlock(): Node {.rule.}

proc parseIf(): Node {.rule.} =
  ## Parses an if expression.
  # if <- 'if' expr block *('elif' expr block) ?(else block)
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

# XXX: This is a pretty bad solution as it requires two different procs to be
# changed if something changes in the codebase
proc parseType(): Node {.rule.} =
  ## Parses a type.
  # type <- Ident ?('[' commaList(type) ']')
  let tok = scan.next()
  case tok.kind
  of tokIdent:
    result = Node(kind: nkIdent, ident: tok.ident)
  else:
    scan.error("Type expected")
  if scan.peek().kind == tokLBrk:
    discard scan.next()
    var gParams = @[result]
    while true:
      if scan.atEnd:
        scan.error("Right bracket ']' expected")
      gParams.add(parseType(scan))
      let next = scan.next()
      case next.kind
      of tokComma: continue
      of tokRBrk: break
      else:
        scan.error("Comma ',' or right bracket ']' expected")
    result = Node(kind: nkGeneric, children: gParams)

proc intoType(typeExpr: Node): Node {.rule.} =
  ## Converts an expression into a type.
  if typeExpr.kind == nkIdent:
    result = typeExpr
  elif typeExpr.kind == nkIndex:
    if typeExpr[0].kind != nkIdent:
      scan.error("Generic type must begin with type name")
    var params: seq[Node]
    for n in typeExpr.children:
      params.add(intoType(scan, n))
    result = Node(kind: nkGeneric, children: params)

proc parsePrefix(token: Token): Node {.rule.} =
  ## Parses a prefix expression.
  # prefix <- 'true' | 'false' | Number | String | Ident |
  #           Operator prefix | parExpr | if
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
  ## Parses an infix expression.
  # infix <- InfixOperator expr ^ (operator.prec) |
  #          '[' expr ']' ^ 10 |
  #          '.' expr ^ 10 |
  #          '(' (commaList(expr) | commaList(Ident ':' expr)) ')' ^ 10
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
      var fields = @[intoType(scan, left)]
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
  ## Parses an expression.
  # expr <- prefix *infix
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
  ## Parses a variable declaration.
  # var <- ('var' | 'let') Ident ?(':' expr(prec > 9)) '=' expr
  result = Node(kind: if scan.next().kind == tokVar: nkVar
                      else: nkLet)
  while true:
    let name = scan.expect(tokIdent)
    var ty = Node(kind: nkEmpty)
    if scan.peek().kind == tokColon:
      discard scan.next()
      ty = parseType(scan)
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
  ## Parses a while loop.
  # while <- 'while' expr block
  discard scan.next()
  let
    cond = parseExpr(scan)
    body = parseBlock(scan)
  result = Node(kind: nkWhile, children: @[cond, body])

proc parseObject(): Node {.rule.} =
  ## Parses an object declaration.
  # identDefs <- commaList(Ident) ':' type
  # object <- 'object' type '{' *identDefs '}'
  discard scan.next()
  let name = parseType(scan)
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
    var ty = parseType(scan)
    fieldNames.add(ty)
    fields.add(Node(kind: nkObjFields, children: fieldNames))
    if not scan.linefeed() and scan.peek().kind != tokRBrace:
      scan.error("Line feed expected after object field")
  discard scan.next()
  result = Node(kind: nkObject, children: fields)

proc parseBreak(): Node {.rule.} =
  ## Parses a break statement.
  # break <- 'break'
  discard scan.next()
  result = Node(kind: nkBreak)

proc parseContinue(): Node {.rule.} =
  ## Parses a continue statement.
  # continue <- 'continue'
  discard scan.next()
  result = Node(kind: nkContinue)

proc parseStmt(): Node {.rule.} =
  ## Parses a statement.
  # stmt <- block | var | object | proc | while | break | continue | expr
  result =
    case scan.peek().kind
    of tokLBrace: parseBlock(scan)
    of tokVar, tokLet: parseVar(scan)
    of tokObject: parseObject(scan)
    of tokWhile: parseWhile(scan)
    of tokBreak: parseBreak(scan)
    of tokContinue: parseContinue(scan)
    else: parseExpr(scan)

proc parseBlock(): Node {.rule.} =
  ## Parses a block.
  # block <- '{' *(stmt '\n') ?stmt '}'
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
  ## Parses a script.
  # script <- *(stmt '\n') ?stmt
  var stmts: seq[Node]
  while not scan.atEnd:
    stmts.add(parseStmt(scan))
    if not scan.linefeed():
      scan.error("Line feed expected after statement")
  result = Node(kind: nkScript, children: stmts)

