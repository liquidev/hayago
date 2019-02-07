#~~
# rod compiler
# copyright (C) iLiquid, 2019
#~~

from strutils import `toHex`

import deques
import tables

import parser
import util
import value
import vm

#~~
# opcodes and config
#~~

type
  RodOpcode* = enum
    ## A rod opcode, used in bytecode.
    # stack operations
    ropPushConst
    ropPushGlobal
    ropPopGlobal
    # calls
    ropCallForeign
    ropReturn

#~~
# chunk
#~~

type
  RodChunk* = ref object
    module*: RodModule

    bytecode*: seq[uint8]
    lines*: seq[tuple[len: int, num: int]]

    consts*: seq[RodValue]
  RodModule* = ref object
    symbols*: seq[string]
    globals*: TableRef[string, RodVar]

proc `[]`(chunk: RodChunk, idx: int): uint8 =
  chunk.bytecode[idx]

proc newModule*(): RodModule =
  RodModule()

proc newChunk*(module: RodModule): RodChunk =
  RodChunk(
    module: module,
    bytecode: @[],
    lines: @[],
    consts: @[]
  )

proc constId(chunk: var RodChunk, val: RodValue): int16 =
  for i, c in chunk.consts:
    case c.kind
    of rvNull:
      if val.kind == rvNull: return (int16 i)
    of rvBool:
      if val.kind == rvBool and c.boolVal == val.boolVal: return (int16 i)
    of rvNum:
      if val.kind == rvNum and c.numVal == val.numVal: return (int16 i)
    of rvStr:
      if val.kind == rvStr and c.strVal == val.strVal: return (int16 i)
    else: discard
  result = int16 len(chunk.consts)
  chunk.consts.add(val)

proc symbolId(chunk: var RodChunk, symbol: string): int16 =
  for i, s in chunk.module.symbols:
    if s == symbol: return (int16 i)
  result = int16 len(chunk.module.symbols)
  chunk.module.symbols.add(symbol)

proc write*(chunk: var RodChunk, bytes: varargs[uint8]) =
  chunk.bytecode.add(bytes)

proc write*(chunk: var RodChunk, opcode: RodOpcode) =
  chunk.write(uint8 opcode)

proc write*[T: SomeNumber](chunk: var RodChunk, num: T) =
  chunk.write(cast[array[sizeof(T), uint8]](num))

proc readOpcode*(chunk: RodChunk, pos: int): RodOpcode =
  RodOpcode chunk.bytecode[pos]

proc read*[T: SomeNumber](chunk: RodChunk, pos: int): T =
  var bytes: array[sizeof(T), uint8]
  for i in 0..<sizeof(T):
    bytes[i] = chunk[pos + i]
  cast[T](bytes)

proc disassemble*(chunk: RodChunk): string =
  var i = 0
  while i < chunk.bytecode.len:
    let opcode = RodOpcode chunk[i]
    result.add(toHex(int32 i))
    result.add("  ")
    result.add(($opcode)[3..<len($opcode)])
    result.add("\t")
    i += 1
    case opcode
    of ropPushConst:
      result.add("<" & $chunk.consts[read[int16](chunk, i)] & ">")
      i += 2
    of ropCallForeign:
      result.add(toHex(read[int16](chunk, i)))
      i += 2
    else: discard
    result.add("\n")

proc `$`*(chunk: RodChunk): string =
  result.add("bytecode: ")
  for i, c in chunk.bytecode:
    result.add(toHex(c))
    if i < len(chunk.bytecode) - 1: result.add(' ')
  result.add("\ndisassembly:\n")
  result.add(disassemble(chunk))
  result.add("\nconstants:")
  for id, constant in chunk.consts:
    result.add("\n  [" & toHex(int16 id) & "] " & $constant)

#~~
# compiler instance
#~~

type
  RodCompiler* = ref object
    vm: RodVM
  RodCompileError* = object of Exception

proc newCompiler*(vm: RodVM): RodCompiler =
  result = RodCompiler(
    vm: vm
  )

proc error(cp: var RodCompiler, message: string) =
  # let
  #   ln = cp.peek().ln
  #   col = cp.peek().col
  #   message = "line " & $ln & " col " & $col & ": " & message
  # raise newException(RodCompileError, message)
  discard

#~~
# compilation
#~~

type
  RuleFn = proc (cp: var RodCompiler,
                 node: RodNode, chunk: var RodChunk): string

var rules = newTable[set[RodNodeKind], RuleFn]()

proc compile(cp: var RodCompiler,
             chunk: var RodChunk, node: RodNode): string {.discardable.} =
  for k, r in rules:
    if node.kind in k:
      return r(cp, node, chunk)
  warn("compiling " & $node.kind & " is not yet implemented")

proc compile*(cp: var RodCompiler, module: RodModule,
              node: RodNode): RodChunk =
  result = module.newChunk()
  discard cp.compile(result, node)

template rule(nodeKind: set[RodNodeKind], body: untyped): untyped {.dirty.} =
  rules.add(nodeKind) do (cp: var RodCompiler,
                          node: RodNode, chunk: var RodChunk) -> string:
    body

proc writeCall(cp: RodCompiler, chunk: var RodChunk, sig: string) =
  if cp.vm.foreignFnSignatures.hasKey(sig):
    chunk.write(ropCallForeign)
    chunk.write(cp.vm.foreignFnSignatures[sig])

rule {rnNull, rnBool, rnNum, rnStr}:
  chunk.write(ropPushConst)
  let val = case node.kind
    of rnNull: nullVal()
    of rnBool: boolVal(node.boolVal)
    of rnNum: numVal(node.numVal)
    of rnStr: strVal(node.strVal)
    else: nullVal()
  chunk.write(chunk.constId(val))
  result = val.typeName()

rule {rnPrefix}:
  result = cp.compile(chunk, node.children[1])
  let
    op = node.children[0].op
    sig = signature(result, op, 1)
  cp.writeCall(chunk, sig)

rule {rnInfix}:
  for n in node.children:
    if n.kind == rnOperator:
      let
        op = n.op
        sig = signature(result, op, 2)
      cp.writeCall(chunk, sig)
    else:
      if result == "":
        result = cp.compile(chunk, n)
      else:
        cp.compile(chunk, n)

rule {rnLet}:
  let
    flags = node.children[0].flags
    isMut = flags and 0b00000001
  echo node

rule {rnScript}:
  for n in node.children:
    cp.compile(chunk, n)
