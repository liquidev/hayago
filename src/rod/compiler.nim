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
    ropCallForeign

#~~
# chunk
#~~

type
  RodChunk* = ref object
    bytecode*: seq[uint8]
    consts*: seq[RodValue]
  RodModule* = ref object

proc `$`*(chunk: RodChunk): string =
  result.add("bytecode: ")
  for i, c in chunk.bytecode:
    result.add(toHex(c))
    if i < len(chunk.bytecode) - 1: result.add(' ')
  result.add("\nconstants:")
  for id, constant in chunk.consts:
    result.add("\n  [" & toHex(int32 id) & "] " & $constant)

proc newChunk*(): RodChunk =
  RodChunk(
    bytecode: @[],
    consts: @[]
  )

proc newModule*(): RodModule =
  RodModule()

proc constId(chunk: var RodChunk, val: RodValue): int32 =
  for i, c in chunk.consts:
    case c.kind
    of rvNum:
      if c.kind == rvNum and c.numVal == val.numVal: return (int32 i)
    else: discard
  result = int32 len(chunk.consts)
  chunk.consts.add(val)

proc write*(chunk: var RodChunk, bytes: varargs[uint8]) =
  chunk.bytecode.add(bytes)

proc write*(chunk: var RodChunk, opcode: RodOpcode) =
  chunk.write(uint8 opcode)

proc write*[T: SomeNumber](chunk: var RodChunk, num: T) =
  chunk.write(cast[array[sizeof(T), uint8]](num))

#~~
# compiler instance
#~~

type
  RodCompiler* = ref object
    # config
    foreignFnSignatures*: TableRef[string, int]
    # stream
    tokens: seq[Token]
    pos: int

proc baseCompiler*(): RodCompiler =
  RodCompiler(
    tokens: @[],
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

#~~
# recursive descent parser
#~~

type
  RuleFn = proc (cp: var RodCompiler, chunk: var RodChunk): bool

var rules: seq[RuleFn] = @[]
var ruleIds = initTable[string, int]()

template rule(name: string, body: untyped): untyped {.dirty.} =
  ruleIds.add(name, len(rules))
  rules.add(proc (cp: var RodCompiler, chunk: var RodChunk): bool =
    body)

rule "constant":
  case cp.peek().kind
  of rtNull: chunk.write(ropPushNull)
  of rtTrue: chunk.write(ropPushTrue)
  of rtFalse: chunk.write(ropPushFalse)
  of rtNum:
    chunk.write(ropPushConst)
    chunk.write(chunk.constId(numVal(cp.peek().numVal)))
  of rtStr:
    chunk.write(ropPushConst)
    chunk.write(chunk.constId(strVal(cp.peek().strVal)))
  else: return false
  cp.next()
  return true

proc compile*(cp: var RodCompiler,
              tokens: seq[Token],
              rule: string): RodChunk =
  result = newChunk()
  discard rules[ruleIds[rule]](cp, result)

proc compile*(cp: var RodCompiler, tokens: seq[Token]): RodChunk =
  result = cp.compile(tokens, "script")
