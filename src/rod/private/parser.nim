#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

# Parser procs (``parse*``) are annotated with pseudo-npeg rules.

import macros
import strformat
import strutils

import scanner

type
  NodeKind* = enum
    # building blocks
    nkEmpty        # empty node
    nkScript       # full script
    nkBlock        # a block - {...}
    nkIdentDefs    # identifier definitions - a, b: s = x
    nkFormalParams # formal params - (a: s, ...) -> t
    # literals
    nkBool         # bool literal
    nkNumber       # number literal
    nkString       # string literal
    nkIdent        # identifier
    # expressions
    nkPrefix       # prefix operator - op expr
    nkInfix        # infix operator - left op right
    nkDot          # dot expression - left.right
    nkColon        # colon expression - left: right
    nkIndex        # index expression - left[a, ...]
    nkCall         # call - left(a, ...)
    nkIf           # if expression - if expr {...} elif expr {...} else {...}
    # types
    nkProcTy       # procedure type - proc (...) -> t
    # statements
    nkVar          # var declaration - var a = x
    nkLet          # let declaration - let a = x
    nkWhile        # while loop - while cond {...}
    nkFor          # for loop - for x in y {...}
    nkBreak        # break statement - break
    nkContinue     # continue statement - continue
    # declarations
    nkObject       # object declaration - object name[T, ...] {...}
    nkProc         # procedure declaration - proc name(a: s, ...) -> t {...}
  Node* = ref object ## An AST node.
    ln*, col*: int ## Line information used for compile errors
    file*: string
    case kind*: NodeKind ## The kind of the node
    of nkEmpty: ## Empty node
      discard
    of nkBool: ## Bool literal
      boolVal*: bool
    of nkNumber: ## Number literal
      numberVal*: float
    of nkString: ## String literal
      stringVal*: string
    of nkIdent: ## Identifier (may get merged with nkString)
      ident*: string
    else: ## Any other branch nodes
      children*: seq[Node]

const LeafNodes = {
  nkEmpty, nkBool, nkNumber, nkString, nkIdent
}

proc `[]`*(node: Node, index: int | BackwardsIndex): Node =
  result = node.children[index]

proc `[]=`*(node: Node, index: int | BackwardsIndex, child: Node) =
  node.children[index] = child

proc add*(node, child: Node): Node {.discardable.} =
  node.children.add(child)
  result = node

proc add*(node: Node, children: openarray[Node]): Node {.discardable.} =
  node.children.add(children)
  result = node

proc `$`*(node: Node): string =
  ## Stringify a node into a lisp representation.
  case node.kind
  of nkEmpty: result = "Empty"
  of nkBool: result = "Bool " & $node.boolVal
  of nkNumber: result = "Number " & $node.numberVal
  of nkString: result = "String " & escape(node.stringVal)
  of nkIdent: result = "Ident " & node.ident
  else:
    result = ($node.kind)[2..^1]
    var children = ""
    for i, child in node.children:
      children.add('\n' & $child)
    result.add(children.indent(2))

proc newNode*(scan: Scanner, kind: NodeKind): Node =
  ## Construct a new node.
  result = Node(kind: kind,
                ln: scan.ln, col: scan.col,
                file: scan.file)

proc newEmpty*(scan: Scanner): Node =
  ## Construct a new empty node.
  result = scan.newNode(nkEmpty)

proc newBlock*(scan: Scanner, children: varargs[Node]): Node =
  ## Construct a new block.
  result = scan.newNode(nkBlock)

proc newTree*(scan: Scanner, kind: NodeKind, children: varargs[Node]): Node =
  ## Construct a new branch node with the given kind.
  assert kind notin LeafNodes, "kind must denote a branch node"
  result = scan.newNode(kind)
  result.add(children)

proc newBoolLit*(scan: Scanner, val: bool): Node =
  ## Construct a new bool literal.
  result = scan.newNode(nkBool)
  result.boolVal = val

proc newNumberLit*(scan: Scanner, val: float): Node =
  ## Construct a new number literal.
  result = scan.newNode(nkNumber)
  result.numberVal = val

proc newStringLit*(scan: Scanner, val: string): Node =
  ## Construct a new string literal.
  result = scan.newNode(nkString)
  result.stringVal = val

proc newIdent*(scan: Scanner, ident: string): Node =
  ## Construct a new ident node.
  result = scan.newNode(nkIdent)
  result.ident = ident

proc newIdentDefs*(scan: Scanner, names: openarray[Node], ty: Node,
                   value = scan.newEmpty()): Node =
  ## Construct a new nkIdentDefs node.
  result = scan.newTree(nkIdentDefs, names)
  result.add([ty, value])

template ruleGuard(body) =
  ## Helper used by {.rule.} to update line info appropriately for nodes.
  when declared(result):
    let
      ln = scan.ln
      col = scan.col
  body
  when declared(result):
    result.ln = ln
    result.col = col
    result.file = scan.file

macro rule(pc) =
  ## Adds a ``scan`` parameter to a proc and wraps its body in a call to
  ## ``ruleGuard``.
  pc[3].insert(1,
    newIdentDefs(ident"scan", newTree(nnkVarTy, ident"Scanner")))
  if pc[6].kind != nnkEmpty:
    pc[6] = newCall("ruleGuard", newStmtList(pc[6]))
  result = pc

const
  PrecColon = 10
  PrecCall = 11

proc precedence(token: Token): int =
  ## Returns the infix precedence of a token.
  result =
    case token.kind
    of tokOperator: token.prec
    of tokColon: PrecColon
    of tokLBrk, tokDot, tokLPar: PrecCall
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
  result = scan.newTree(nkIf, children)

proc parseProcHead*(anon: bool, name, formalParams: var Node) {.rule.}

proc parseType(): Node {.rule.} =
  ## Parses a type.
  # type <- expr(9) | anonProcHead
  if scan.peek().kind == tokProc:
    var name, params: Node
    parseProcHead(scan, anon = true, name, params)
    result = scan.newTree(nkProcTy, params)
  else:
    result = parseExpr(scan, prec = 9)

proc parseIdentDefs(): Node {.rule.} =
  ## Parses identifier definitions.
  # identDefs <- Ident *(',' Ident) (':' expr(9) | '=' expr |
  #                                  ':' expr(9) '=' expr)
  result = scan.newNode(nkIdentDefs)
  var delim: Token
  while true:
    let
      identTok = scan.expect(tokIdent)
      identNode = scan.newIdent(identTok.ident)
    result.add(identNode)
    delim = scan.next()
    case delim.kind
    of tokComma: continue
    of tokColon, tokOperator: break
    else:
      scan.error("Comma ',', colon ':', or assignment '=' expected")
  if delim.kind == tokOperator and delim.operator != "=":
    scan.error(fmt"Assignment operator '=' expected, got '{delim.ident}'")
  var
    ty = scan.newEmpty()
    value = scan.newEmpty()
  case delim.kind
  of tokColon:
    ty = parseType(scan)
  of tokOperator:
    value = parseExpr(scan)
  else: assert false, "unreachable"
  if delim.kind == tokColon and scan.peek().kind == tokOperator:
    scan.expectOp("=")
    value = parseExpr(scan)
  result.add([ty, value])

proc parseCommaList*(scan: var Scanner, start, term: static TokenKind,
                     results: var seq[Node],
                     rule: proc (scan: var Scanner): Node): bool =
  ## Parses a comma-separated list.
  # commaList(s, t, rule) <- s ?rule *(',' rule) t
  if scan.peek().kind == start:
    discard scan.next()
  else:
    return false
  while true:
    if scan.atEnd:
      scan.error(fmt"Missing '{term}'")
    results.add(rule(scan))
    case scan.next().kind
    of tokComma: continue
    of term: break
    else:
      scan.error("',' or '{term}' expected")
  result = true

proc parseProcHead*(anon: bool, name, formalParams: var Node) {.rule.} =
  ## Parse a procedure header.
  # anonProcHead <- 'proc' '(' identDefs ')' -> type
  # procHead <- 'proc' Ident '(' identDefs ')' -> type
  scan.expect(tokProc)
  if not anon:
    let ident = scan.expect(tokIdent, "Proc name expected")
    name = scan.newIdent(ident.ident)
  formalParams = scan.newTree(nkFormalParams, scan.newEmpty())
  if scan.peek().kind == tokLPar:
    var params: seq[Node]
    if not parseCommaList(scan, tokLPar, tokRPar, params, parseIdentDefs):
      scan.error("Proc params expected")
    formalParams.add(params)
  if scan.peek().kind == tokOperator and scan.peek().operator == "->":
    discard scan.next()
    formalParams[0] = parseType(scan)

proc parsePrefix(token: Token): Node {.rule.} =
  ## Parses a prefix expression.
  # prefix <- 'true' | 'false' | Number | String | Ident |
  #           Operator prefix | parExpr | if
  case token.kind
  of tokTrue: result = scan.newBoolLit(true)
  of tokFalse: result = scan.newBoolLit(false)
  of tokNumber: result = scan.newNumberLit(token.numberVal)
  of tokString: result = scan.newStringLit(token.stringVal)
  of tokIdent: result = scan.newIdent(token.ident)
  of tokOperator: result = scan.newTree(nkPrefix, scan.newIdent(token.operator),
                                        parsePrefix(scan, scan.next()))
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
      result = scan.newTree(nkInfix, scan.newIdent(token.operator),
                            left, parseExpr(scan, token.prec))
  of tokLBrk: # index operator '[]'
    result = scan.newTree(nkIndex, left, parseExpr(scan, prec = PrecCall))
    while scan.peek().kind == tokComma:
      discard scan.next()
      result.add(parseExpr(scan, prec = PrecCall))
    scan.expect(tokRBrk)
  of tokDot: # dot operator '.'
    result = scan.newTree(nkDot, left, parseExpr(scan, prec = PrecCall))
  of tokColon: # colon expression
    result = scan.newTree(nkColon, left, parseExpr(scan, prec = PrecColon))
  of tokLPar: # call or object constructor
    result = scan.newNode(nkCall)
    while true:
      if scan.atEnd:
        scan.error("Missing right paren ')'")
      let val = parseExpr(scan)
      result.add(val)
      let next = scan.next().kind
      case next
      of tokComma: continue
      of tokRPar: break
      else:
        scan.error("Comma ',' or right paren ')' expected")
  else: scan.error("Unexpected token: " & $token.kind)

proc parseExpr(prec = 0): Node {.rule.} =
  ## Parses an expression.
  # expr <- prefix *infix
  # expr(x) parses infix expressions of precedence > x
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
  # var <- ('var' | 'let') Ident ?(':' expr(9)) '=' expr
  result = scan.newNode(if scan.next().kind == tokVar: nkVar
                        else: nkLet)
  result.add(parseIdentDefs(scan))
  if result.children.len < 1:
    scan.error("Variable declaration expected")

proc parseWhile(): Node {.rule.} =
  ## Parses a while loop.
  # while <- 'while' expr block
  discard scan.next()
  let
    cond = parseExpr(scan)
    body = parseBlock(scan)
  result = scan.newTree(nkWhile, cond, body)

proc parseObject(): Node {.rule.} =
  ## Parses an object declaration.
  # identDefs <- commaList(Ident) ':' type
  # object <- 'object' type '{' *identDefs '}'
  discard scan.next()
  let name = parseExpr(scan, prec = 9)
  scan.expect(tokLBrace)
  var fields: seq[Node]
  fields.add(name)
  while scan.peek().kind != tokRBrace:
    if scan.atEnd:
      scan.error("Missing right brace '}'")
    fields.add(parseIdentDefs(scan))
    if not scan.linefeed() and scan.peek().kind != tokRBrace:
      scan.error("Line feed expected after object field")
  discard scan.next()
  result = scan.newTree(nkObject, fields)

proc parseBreak(): Node {.rule.} =
  ## Parses a break statement.
  # break <- 'break'
  discard scan.next()
  result = scan.newNode(nkBreak)

proc parseContinue(): Node {.rule.} =
  ## Parses a continue statement.
  # continue <- 'continue'
  discard scan.next()
  result = scan.newNode(nkContinue)

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
  result = scan.newTree(nkBlock, stmts)

proc parseScript*(): Node {.rule.} =
  ## Parses a script.
  # script <- *(stmt '\n') ?stmt
  var stmts: seq[Node]
  while not scan.atEnd:
    stmts.add(parseStmt(scan))
    if not scan.linefeed():
      scan.error("Line feed expected after statement")
  result = scan.newTree(nkScript, stmts)

