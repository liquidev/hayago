#--
# the hayago scripting language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

# Parser procs (``parse*``) are annotated with pseudo-npeg rules.

import std/macros
import std/strformat

import ast
import scanner

template ruleGuard(body) =
  ## Helper used by {.rule.} to update line info appropriately for nodes.
  when declared(result):
    let
      ln = scan.ln
      col = scan.col
  body
  when declared(result):
    if result != nil:
      result.ln = ln
      result.col = col
      result.file = scan.file

macro rule(pc) =
  ## Adds a ``scan`` parameter to a proc and wraps its body in a call to
  ## ``ruleGuard``.
  pc[3].insert(1, newIdentDefs(ident"scan", newTree(nnkVarTy, ident"Scanner")))
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
  result = newTree(nkIf, children)

proc parseProcHead*(anon: bool, name,
                    genericParams, formalParams: var Node) {.rule.}

proc parseType(): Node {.rule.} =
  ## Parses a type.
  # type <- expr(9) | anonProcHead
  if scan.peek().kind == tokProc:
    discard scan.next()
    var name, genericParams, formalParams: Node
    parseProcHead(scan, anon = true, name, genericParams, formalParams)
    if genericParams.kind != nkEmpty:
      scan.error("Generic params are not allowed in proc types")
    result = newTree(nkProcTy, formalParams)
  else:
    result = parseExpr(scan, prec = 9)

proc parseIdentDefs(): Node {.rule.} =
  ## Parses identifier definitions.
  # identDefs <- Ident *(',' Ident) (':' expr(9) | '=' expr |
  #                                  ':' expr(9) '=' expr)
  result = newNode(nkIdentDefs)
  var delim: Token
  while true:
    let
      identTok = scan.expect(tokIdent)
      identNode = newIdent(identTok.ident)
    result.add(identNode)
    delim = scan.peek()
    case delim.kind
    of tokComma:
      discard scan.next()
      continue
    of tokColon, tokOperator:
      discard scan.next()
      break
    else: break
  if delim.kind == tokOperator and delim.operator != "=":
    scan.error(fmt"Assignment operator '=' expected, got '{delim.ident}'")
  var
    ty = newEmpty()
    value = newEmpty()
  case delim.kind
  of tokColon:
    ty = parseType(scan)
  of tokOperator:
    value = parseExpr(scan)
  else: discard
  if delim.kind == tokColon and scan.peek().kind == tokOperator:
    scan.expectOp("=")
    value = parseExpr(scan)
  result.add([ty, value])

proc parseCommaList(scan: var Scanner, start, term: static TokenKind,
                    results: var seq[Node],
                    rule: proc (scan: var Scanner): Node): bool =
  ## Parses a comma-separated list.
  # commaList(s, t, rule) <- s ?rule *(',' rule) t
  when start != tokNone:
    if scan.peek().kind == start:
      discard scan.next()
    else:
      return false
  while true:
    if scan.atEnd:
      scan.error(fmt"Missing '{term}'")
    elif scan.peek().kind == term:
      discard scan.next()
      break
    results.add(rule(scan))
    case scan.next().kind
    of tokComma: continue
    of term: break
    else:
      scan.error(fmt"',' or '{term}' expected")
  result = true

proc parseProcHead(anon: bool, name,
                   genericParams, formalParams: var Node) {.rule.} =
  ## Parse a procedure header.
  # anonProcHead <- commaList('(', ')', identDefs) -> type
  # procHead <- Ident commaList('(', ')', identDefs) -> type
  if not anon:
    let ident = scan.expect(tokIdent, "Proc name expected")
    name = newIdent(ident.ident)
  else:
    name = newEmpty()
  if scan.peek().kind == tokLBrk:
    genericParams = newTree(nkGenericParams)
    var params: seq[Node]
    discard parseCommaList(scan, tokLBrk, tokRBrk, params, parseIdentDefs)
    genericParams.add(params)
  else:
    genericParams = newEmpty()
  formalParams = newTree(nkFormalParams, newEmpty())
  if scan.peek().kind == tokLPar:
    var params: seq[Node]
    if not parseCommaList(scan, tokLPar, tokRPar, params, parseIdentDefs):
      scan.error("Proc params expected")
    formalParams.add(params)
  if scan.peek().kind == tokOperator and scan.peek().operator == "->":
    discard scan.next()
    formalParams[0] = parseType(scan)

proc parseProc(anon: bool): Node {.rule.} =
  ## Parse a procedure or anonymous procedure.
  # anonProc <- anonProcHead block
  # proc <- procHead block
  var name, genericParams, formalParams: Node
  parseProcHead(scan, anon, name, genericParams, formalParams)
  let body = parseBlock(scan)
  result = newTree(nkProc, name, genericParams, formalParams, body)

proc parsePrefix(token: Token): Node {.rule.} =
  ## Parses a prefix expression.
  # prefix <- 'true' | 'false' | Number | String | Ident |
  #           Operator prefix | parExpr | if
  case token.kind
  of tokTrue: result = newBoolLit(true)
  of tokFalse: result = newBoolLit(false)
  of tokNumber: result = newNumberLit(token.numberVal)
  of tokString: result = newStringLit(token.stringVal)
  of tokIdent: result = newIdent(token.ident)
  of tokOperator: result = newTree(nkPrefix, newIdent(token.operator),
                                   parseExpr(scan, prec = 9))
  of tokLPar: result = parseParExpr(scan)
  of tokIf: result = parseIf(scan)
  of tokProc: result = parseProc(scan, anon = true)
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
      result = newTree(nkInfix, newIdent(token.operator),
                       left, parseExpr(scan, token.prec))
  of tokLBrk: # index operator '[]'
    result = newTree(nkIndex, left, parseExpr(scan, prec = PrecCall))
    while scan.peek().kind == tokComma:
      discard scan.next()
      result.add(parseExpr(scan, prec = PrecCall))
    scan.expect(tokRBrk)
  of tokDot: # dot operator '.'
    result = newTree(nkDot, left, parseExpr(scan, prec = PrecCall))
  of tokColon: # colon expression
    result = newTree(nkColon, left, parseExpr(scan, prec = PrecColon))
  of tokLPar: # call or object constructor
    result = newTree(nkCall, left)
    discard scan.parseCommaList(tokNone, tokRPar,
                                result.children) do (scan: var Scanner) -> Node:
      result = parseExpr(scan)
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
  result = newNode(
    if scan.next().kind == tokVar: nkVar
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
  result = newTree(nkWhile, cond, body)

proc parseFor(): Node {.rule.} =
  ## Parses a for loop.
  # for <- 'for' Ident 'in' expr block
  discard scan.next()
  let loopVar = scan.expect(tokIdent, "Loop variable expected")
  scan.expectOp("in")
  let
    iter = parseExpr(scan)
    body = parseBlock(scan)
  result = newTree(nkFor, newIdent(loopVar.ident), iter, body)

proc parseObject(): Node {.rule.} =
  ## Parses an object declaration.
  # identDefs <- commaList(Ident) ':' type
  # object <- 'object' type '{' *identDefs '}'
  result = newNode(nkObject)
  discard scan.next()
  let name = newIdent(scan.expect(tokIdent, "Object name expected").ident)
  var genericParams = newEmpty()
  if scan.peek().kind == tokLBrk:
    genericParams = newTree(nkGenericParams)
    var params: seq[Node]
    discard parseCommaList(scan, tokLBrk, tokRBrk, params, parseIdentDefs)
    genericParams.add(params)
  result.add([name, genericParams])
  scan.expect(tokLBrace)
  var fields = newNode(nkRecFields)
  while scan.peek().kind != tokRBrace:
    if scan.atEnd:
      scan.error("Missing right brace '}'")
    fields.add(parseIdentDefs(scan))
    if not scan.linefeed() and scan.peek().kind != tokRBrace:
      scan.error("Line feed expected after object field")
  discard scan.next()
  result.add(fields)

proc parseIterator(): Node {.rule.} =
  ## Parse an iterator declaration.
  discard scan.next()
  var name, genericParams, formalParams: Node
  parseProcHead(scan, anon = false, name, genericParams, formalParams)
  let body = parseBlock(scan)
  result = newTree(nkIterator, name, genericParams, formalParams, body)

proc parseBreak(): Node {.rule.} =
  ## Parses a break statement.
  # break <- 'break'
  discard scan.next()
  result = newNode(nkBreak)

proc parseContinue(): Node {.rule.} =
  ## Parses a continue statement.
  # continue <- 'continue'
  discard scan.next()
  result = newNode(nkContinue)

proc parseReturn(): Node {.rule.} =
  ## Parses a return statement.
  # return <- 'return' ?expr
  discard scan.next()
  result = newNode(nkReturn)
  if not scan.peekLinefeed():
    result.add(parseExpr(scan))
  else:
    result.add(newEmpty())

proc parseYield(): Node {.rule.} =
  ## Parses a yield statement.
  ## yield <- 'yield' expr
  discard scan.next()
  result = newTree(nkYield, parseExpr(scan))

proc parseStmt(): Node {.rule.} =
  ## Parses a statement.
  # stmt <- block |
  #         var | object | proc | iterator |
  #         while |
  #         break | continue | return |
  #         expr
  result =
    case scan.peek().kind
    of tokLBrace: parseBlock(scan)
    of tokVar, tokLet: parseVar(scan)
    of tokProc: discard scan.next(); parseProc(scan, anon = false)
    of tokIterator: parseIterator(scan)
    of tokObject: parseObject(scan)
    of tokWhile: parseWhile(scan)
    of tokFor: parseFor(scan)
    of tokBreak: parseBreak(scan)
    of tokContinue: parseContinue(scan)
    of tokReturn: parseReturn(scan)
    of tokYield: parseYield(scan)
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
  result = newTree(nkBlock, stmts)

proc parseScript*(): Node {.rule.} =
  ## Parses a script.
  # script <- *(stmt '\n') ?stmt
  var stmts: seq[Node]
  while not scan.atEnd:
    stmts.add(parseStmt(scan))
    if not scan.linefeed():
      scan.error("Line feed expected after statement")
  result = newTree(nkScript, stmts)

