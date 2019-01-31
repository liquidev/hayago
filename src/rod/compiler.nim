#~~
# rod compiler
# copyright (C) iLiquid, 2019
#~~

## Rod uses a direct compiler. That means all tokens are turned directly into
## bytecode using recursive descent parsing – except we don't produce
## an AST, to reduce memory usage.

from strutils import `toHex`
import tables

import lexer
import value
import vm

#~~
# opcodes and config
#~~

type
  RodOpcode* = enum
    ## A rod opcode, used in bytecode.
    ropReturn
    # stack operations
    ropPushNull, ropPushTrue, ropPushFalse
    ropPushConst
    # calls
    ropCallForeign

#~~
# chunk
#~~

type
  RodChunk* = ref object
    bytecode*: seq[uint8]
    lines*: seq[tuple[len: int, num: int]]
    consts*: seq[RodValue]
  RodModule* = ref object

proc `[]`(chunk: RodChunk, idx: int): uint8 =
  chunk.bytecode[idx]

proc newChunk*(): RodChunk =
  RodChunk(
    bytecode: @[],
    lines: @[],
    consts: @[]
  )

proc newModule*(): RodModule =
  RodModule()

proc constId(chunk: var RodChunk, val: RodValue): int16 =
  for i, c in chunk.consts:
    case c.kind
    of rvNum:
      if c.kind == rvNum and c.numVal == val.numVal: return (int16 i)
    else: discard
  result = int16 len(chunk.consts)
  chunk.consts.add(val)

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
    tokens: seq[Token]
    pos: int
  RodCompileError* = object of Exception

proc newCompiler*(vm: RodVM, tokens: seq[Token]): RodCompiler =
  RodCompiler(
    vm: vm,
    tokens: tokens,
    pos: 0
  )

proc peek(cp: RodCompiler): Token =
  cp.tokens[cp.pos]

proc next(cp: var RodCompiler) =
  cp.pos += 1

proc consume(cp: var RodCompiler): Token =
  result = cp.peek()
  cp.next()

proc match(cp: var RodCompiler, kind: TokenKind, tok: var Token): bool =
  if cp.peek().kind == kind:
    tok = cp.consume()
    result = true

proc match(cp: var RodCompiler, kind: TokenKind): bool =
  var tok: Token
  cp.match(kind, tok)

proc error(cp: var RodCompiler, message: string) =
  let
    ln = cp.peek().ln
    col = cp.peek().col
    message = "line " & $ln & " col " & $col & ": " & message
  raise newException(RodCompileError, message)

#~~
# recursive descent parser
#~~

type
  RuleFn* = proc (cp: var RodCompiler, chunk: var RodChunk): bool

proc constant*(cp: var RodCompiler, chunk: var RodChunk): RodValue =
  case cp.peek().kind
  of rtNull:
    chunk.write(ropPushNull)
    result = nullVal()
  of rtTrue:
    chunk.write(ropPushTrue)
    result = boolVal(true)
  of rtFalse:
    chunk.write(ropPushFalse)
    result = boolVal(true)
  of rtNum:
    result = numVal(cp.peek().numVal)
    chunk.write(ropPushConst)
    chunk.write(chunk.constId(result))
  of rtStr:
    result = strVal(cp.peek().strVal)
    chunk.write(ropPushConst)
    chunk.write(chunk.constId(result))
  else: return
  cp.next()

proc atom*(cp: var RodCompiler, chunk: var RodChunk): RodValue =
  result = cp.constant(chunk)

proc prefixOp*(cp: var RodCompiler, chunk: var RodChunk): RodValue =
  if cp.peek().kind in { rtPlus, rtMinus, rtExcl, rtTilde }:
    let
      op = cp.consume()
      right = cp.prefixOp(chunk)
      sign = signature(right.typeName(), op.text, 1)
    if cp.vm.foreignFnSignatures.hasKey(sign):
      let id = cp.vm.foreignFnSignatures[sign]
      chunk.write(ropCallForeign)
      chunk.write(id)
      return right
    else:
      cp.error(
        "Type " & right.typeName() & " doesn't implement the " & op.text &
        " operator")
  else:
    return cp.atom(chunk)



#~~
# utility procs
#~~

proc compile*(cp: var RodCompiler,
              tokens: seq[Token],
              rule: RuleFn): RodChunk =
  result = newChunk()
  cp.tokens = tokens
  discard rule(cp, result)

proc compile*(cp: var RodCompiler, tokens: seq[Token]): RodChunk =
  # result = cp.compile(tokens, script)
  discard
