#~~
# rod compiler
# copyright (C) iLiquid, 2019
#~~

from strutils import `toHex`

import deques
import tables
from strutils import alignLeft

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
    ropPushConst ## pushes a constant from the module onto the stack
    ropPushLocal ## pushes a local variable onto the stack
    ropStoreLocal ## stores a local variable without popping it from the stack
    ropPopLocal ## pops from the stack and stores the result in a local
    ropPopDiscard ## pops from the stack and discards the result
    # calls
    ropCallForeignFn ## calls a foreign function
    ropCallForeignMtd ## calls a foreign class method
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
    result.add(alignLeft(($opcode)[3..<len($opcode)], 16))
    i += 1
    case opcode
    of ropPushConst:
      result.add("<" & $chunk.consts[read[int16](chunk, i)] & ">")
      i += 2
    of ropPushLocal, ropPopLocal, ropStoreLocal:
      result.add("$" & $read[int32](chunk, i))
      i += 4
    of ropCallForeignFn, ropCallForeignMtd:
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
    contexts: Deque[RodCompilerCtx]
  RodCompilerCtx = ref object
    localVars: seq[RodVar]
    localCtx: Deque[TableRef[string, int]]
  CompilerError* = object of Exception

#~~ context separation ~~#

proc pushCtx(cp: var RodCompiler) =
  cp.contexts.addLast(RodCompilerCtx(
    localVars: @[],
    localCtx: initDeque[TableRef[string, int]]()
  ))

proc ctx(cp: RodCompiler): RodCompilerCtx =
  cp.contexts.peekLast()

proc popCtx(cp: var RodCompiler) =
  cp.contexts.popLast()

#~~ local variables ~~#

proc pushLocals(cp: var RodCompiler) =
  cp.ctx.localCtx.addLast(newTable[string, int]())

proc locals(cp: RodCompiler): TableRef[string, int] =
  cp.ctx.localCtx.peekLast()

proc popLocals(cp: var RodCompiler) =
  cp.ctx.localCtx.popLast()

proc nextLocal(cp: RodCompiler): int =
  for ctx in cp.ctx.localCtx.items():
    result += ctx.len

proc localId(cp: var RodCompiler, name: string): int =
  var success = false
  for ctx in cp.ctx.localCtx.items():
    if ctx.hasKey(name):
      result = ctx[name]
      success = true
  if not success:
    cp.locals.add(name, cp.nextLocal)

proc newLocal(cp: var RodCompiler, id: int, variable: RodVar) =
  if cp.ctx.localVars.len <= id:
    cp.ctx.localVars.setLen(id + 1)
  cp.ctx.localVars[id] = variable

proc isLocal(cp: var RodCompiler, name: string): bool =
  for ctx in cp.ctx.localCtx:
    if ctx.hasKey(name): return true

#~~ various utilities ~~#

proc newCompiler*(vm: RodVM): RodCompiler =
  result = RodCompiler(
    vm: vm,
    contexts: initDeque[RodCompilerCtx]()
  )
  result.pushCtx()

proc error(msg: string, node: RodNode) =
  let
    ln = node.pos.ln
    col = node.pos.col
    message = $ln & ":" & $col & ": " & msg
  raise newException(CompilerError, message)

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

proc writeCall(cp: RodCompiler, chunk: var RodChunk,
               class: string, sig: string) =
  if class == "void":
    if cp.vm.foreignFnSignatures.hasKey(sig):
      chunk.write(ropCallForeignFn)
      chunk.write(cp.vm.foreignFnSignatures[sig])
  else:
    if cp.vm.foreignClassSignatures.hasKey(class) and
       cp.vm.getForeignClass(class).fnSignatures.hasKey(sig):
      chunk.write(ropCallForeignMtd)
      chunk.write(cp.vm.getForeignClass(class).getFnId(sig))

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
    sig = signature(op, 1)
  cp.writeCall(chunk, result, sig)

rule {rnInfix}:
  for n in node.children:
    if n.kind == rnOperator:
      let
        op = n.op
        sig = signature(op, 2)
      cp.writeCall(chunk, result, sig)
    else:
      if result == "":
        result = cp.compile(chunk, n)
      else:
        cp.compile(chunk, n)

rule {rnCall}:
  let
    mtd = node[0].ident
    sig = signature(mtd, node[1].children.len)
  for arg in node[1].children:
    cp.compile(chunk, arg)
  cp.writeCall(chunk, "void", sig)

rule {rnVariable}:
  if cp.isLocal(node[0].ident):
    let id = cp.localId(node[0].ident)
    chunk.write(ropPushLocal)
    chunk.write(int32 id)
  else:
    error("Variable '" & node[0].ident & "' is not declared",
          node)

rule {rnAssign}:
  if node[0].kind == rnVariable:
    if cp.isLocal(node[0][0].ident):
      let local = cp.ctx.localVars[cp.locals[node[0][0].ident]]
      if local.isMutable:
        let id = cp.localId(node[0][0].ident)
        cp.compile(chunk, node[1])
        chunk.write(ropStoreLocal)
        chunk.write(int32 id)
      else:
        error("Variable '" & node[0][0].ident & "' is immutable and cannot " &
              "be reassigned", node)
    else:
      error("Cannot assign to undeclared variable '" &
            node[0][0].ident & "'",
            node)
  else:
    error("Invalid left-hand of assignment", node)

rule {rnStmt}:
  cp.compile(chunk, node[0])
  chunk.write(ropPopDiscard)

rule {rnLet}:
  let
    flags = node[0].flags
    isMut = flags and 0b00000001
  for decl in node[1].children:
    if decl.kind == rnAssign:
      let
        left = decl[0]
        right = decl[1]
      if left.kind != rnVariable:
        error("Left-hand side of an assignment must be a variable", node)
      let id = cp.localId(left[0].ident)
      cp.newLocal(id, RodVar(
        isMutable: bool isMut,
        typeName: cp.compile(chunk, right)
      ))
      chunk.write(ropPopLocal)
      chunk.write(int32 id)
    elif decl.kind == rnVariable:
      discard
    else:
      error("Expected one or more assignments", node)

rule {rnScript}:
  cp.pushLocals()
  for n in node.children:
    cp.compile(chunk, n)
  cp.popLocals()
