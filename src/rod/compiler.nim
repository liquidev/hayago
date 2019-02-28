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
# Compiler
#~~

type
  RodCompiler* = ref object

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

rule rnkPrefix:
  cp.compile(chunk, node.sons[0])
  chunk.emitOp(roCall)
  chunk.emitU16(chunk.sym(node.sons[1].opToken.op))
