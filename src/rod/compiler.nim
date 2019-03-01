#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

import macros
import sets

import chunk
import opcode
import parser
import value

#~~
# Scopes
#~~

type
  Local = ref object
    name: string
  Scope = ref object
    locals: seq[Local]

#~~
# Compiler
#~~

type
  RodCompiler* = ref object
    scopes: seq[Scope]
  CompileError* = object of Exception

proc pushScope(cp: var RodCompiler) =
  cp.scopes.add(Scope(locals: @[]))

proc scope(cp: RodCompiler): Scope =
  cp.scopes[cp.scopes.len - 1]

proc scopeOffset(cp: RodCompiler, scope: Scope): int =
  for sc in cp.scopes:
    if sc != scope: result += sc.locals.len
    else: break

proc popScope(cp: var RodCompiler) =
  discard cp.scopes.pop()

proc isVar(cp: RodCompiler, chunk: RodChunk,
           name: string): bool =
  if cp.scopes.len > 0:
    for sc in cp.scopes:
      for loc in sc.locals:
        if loc.name == name:
          return true
  else:
    return name in chunk.symbols

proc resolveVar(cp: RodCompiler, chunk: var RodChunk,
                name: string): tuple[global: bool, id: uint16] =
  if cp.scopes.len > 0:
    var
      localId = 0
      found = false
    for sc in cp.scopes:
      let scopeOffset = cp.scopeOffset(sc)
      for j, loc in sc.locals:
        if loc.name == name:
          found = true
          localId = scopeOffset + j
    if not found:
      let scopeOffset = cp.scopeOffset(cp.scope)
      localId = scopeOffset + cp.scope.locals.len
      cp.scope.locals.add(Local(name: name))
    result = (false, uint16 localId)
  else:
    let globalId = chunk.sym(name)
    result = (true, globalId)

proc err(node: RodNode, msg: string) =
  let
    msg = $node.textPos.ln & ":" & $node.textPos.col & ": " & msg
  raise newException(CompileError, msg)

proc newCompiler*(): RodCompiler =
  RodCompiler()

#~~
# Compiler rules
#~~

var rules: array[low(RodNodeKind)..high(RodNodeKind),
  proc (cp: var RodCompiler, chunk: var RodChunk, node: RodNode) {.nimcall.}]

proc compile*(cp: var RodCompiler, chunk: var RodChunk, node: RodNode) =
  rules[node.kind](cp, chunk, node)

template rule(nodeKind: RodNodeKind, body: untyped) {.dirty.} =
  rules[nodeKind] =
    proc (cp: var RodCompiler, chunk: var RodChunk,
          node: RodNode) {.nimcall.} =
      body

rule rnkNone:
  node.err("Malformed AST (rnkNone node occured)")

rule rnkNull:
  chunk.emitOp(roPushConst)
  chunk.emitU16(chunk.id(RodNull))

rule rnkBool:
  chunk.emitOp(roPushConst)
  chunk.emitU16(chunk.id(node.boolVal))

rule rnkNum:
  chunk.emitOp(roPushConst)
  chunk.emitU16(chunk.id(node.numVal))

rule rnkStr:
  chunk.emitOp(roPushConst)
  chunk.emitU16(chunk.id(node.strVal))

rule rnkVar:
  let rvar = cp.resolveVar(chunk, node[0].ident)
  if rvar.global: chunk.emitOp(roPushGlobal)
  else: chunk.emitOp(roPushLocal)
  chunk.emitU16(rvar.id)

rule rnkPrefix:
  cp.compile(chunk, node[0])
  chunk.emitOp(roPushGlobal)
  chunk.emitU16(chunk.sym(node[1].opToken.op))
  chunk.emitOp(roCall)

rule rnkInfix:
  for n in node.sons:
    if n.kind == rnkOp:
      chunk.emitOp(roPushGlobal)
      chunk.emitU16(chunk.sym(n.opToken.op))
      chunk.emitOp(roCall)
    else:
      cp.compile(chunk, n)

rule rnkStmt:
  cp.compile(chunk, node[0])
  chunk.emitOp(roDiscard)

rule rnkLet:
  let rvar = cp.resolveVar(chunk, node[0][0].ident)
  cp.compile(chunk, node[1])
  if rvar.global: chunk.emitOp(roPopGlobal)
  else: chunk.emitOp(roPopLocal)
  chunk.emitU16(rvar.id)

rule rnkScript:
  for n in node.sons:
    cp.compile(chunk, n)
