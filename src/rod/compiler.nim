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

proc resolveVar(cp: RodCompiler, chunk: var RodChunk,
                name: string): tuple[global: bool, id: uint16] =
  if cp.scopes.len > 0:
    var
      localId = 0
      found = false
    for sc in cp.scopes:
      let scopeOffset = cp.scopeOffset(sc)
      for i, loc in sc.locals:
        if loc.name == name:
          found = true
          localId = scopeOffset + i
    if found:
      return (false, uint16 localId)
  let globalId = chunk.sym(name)
  result = (true, globalId)

proc newVar(cp: var RodCompiler, chunk: var RodChunk,
            name: string): tuple[global: bool, id: uint16] =
  if cp.scopes.len > 0:
    let localId = cp.scopeOffset(cp.scope) + cp.scope.locals.len
    cp.scope.locals.add(Local(name: name))
    return (false, uint16 localId)
  else:
    let globalId = chunk.sym(name)
    result = (true, globalId)

proc emitPushVar(chunk: var RodChunk, cp: RodCompiler, name: string) =
  let rvar = cp.resolveVar(chunk, name)
  if rvar.global: chunk.emitOp(roPushGlobal)
  else: chunk.emitOp(roPushLocal)
  chunk.emitU16(rvar.id)

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
  let oldPos = chunk.currentPos
  chunk.currentPos = node.textPos
  rules[node.kind](cp, chunk, node)
  chunk.currentPos = oldPos

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
  chunk.emitPushVar(cp, node[0].ident)

rule rnkCall:
  for n in node[1].sons:
    cp.compile(chunk, n)
  cp.compile(chunk, node[0])
  chunk.emitOp(roCallFn)
  chunk.emitU8(uint8 node[1].sons.len)

rule rnkPrefix:
  cp.compile(chunk, node[0])
  chunk.emitOp(roPushMethod)
  chunk.emitU16(chunk.sym(node[1].opToken.op))
  chunk.emitOp(roCallMethod)
  chunk.emitU8(0)

rule rnkInfix:
  cp.compile(chunk, node[0])
  chunk.emitOp(roPushMethod)
  chunk.emitU16(chunk.sym(node[1].opToken.op))
  cp.compile(chunk, node[2])
  chunk.emitOp(roCallMethod)
  chunk.emitU8(1)

rule rnkStmt:
  cp.compile(chunk, node[0])
  chunk.emitOp(roDiscard)

rule rnkLet:
  for a in node.sons:
    cp.compile(chunk, a[1])
    let rvar = cp.newVar(chunk, a[0][0].ident)
    if rvar.global: chunk.emitOp(roPopGlobal)
    else: chunk.emitOp(roPopLocal)
    chunk.emitU16(rvar.id)

rule rnkBlock:
  cp.pushScope()
  for n in node.sons:
    cp.compile(chunk, n)
  cp.popScope()

rule rnkScript:
  for n in node.sons:
    cp.compile(chunk, n)
  chunk.emitOp(roReturn)
