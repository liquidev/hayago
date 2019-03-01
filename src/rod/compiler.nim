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

proc pushScope*(cp: var RodCompiler) =
  cp.scopes.add(Scope(locals: @[]))

proc popScope*(cp: var RodCompiler) =
  discard cp.scopes.pop()

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
  chunk.emitOp(roPushGlobal)
  chunk.emitU16(chunk.sym(node[0].ident))

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
