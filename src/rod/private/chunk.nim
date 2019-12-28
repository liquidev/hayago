#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import tables

import parser
import value

type
  Opcode* = enum ## An opcode, used for execution.
    # Stack
    opcPushTrue = "pushTrue"
    opcPushFalse = "pushFalse"
    opcPushN = "pushN" ## Push number
    opcPushG = "pushG" ## Push global
    opcPopG = "popG" ## Pop global
    opcPushL = "pushL" ## Push local
    opcPopL = "popL" ## Pop local
    opcConstrObj = "constrObj" ## Construct object
    opcPushF = "pushF" ## Push field
    opcPopF = "popF" ## Pop field
    opcDiscard = "discard" ## Discard values
    # Arithmetic operations
    opcNegN = "negN" ## Negate number
    opcAddN = "addN" ## Add numbers
    opcSubN = "subN" ## Subtract numbers
    opcMultN = "multN" ## Multiply numbers
    opcDivN = "divN" ## Divide numbers
    # Logic operations
    opcInvB = "invB" ## Invert bool
    # Relational operations
    opcEqB = "eqB" ## Equal bools
    opcEqN = "eqN" ## Equal numbers
    opcLessN = "lessN" ## Number less than
    opcLessEqN = "lessEqN" ## Number less than or equal
    opcGreaterN = "greaterN" ## Number greater than
    opcGreaterEqN = "greaterEqN" ## Number greater than or equal
    # Execution
    opcJumpFwd = "jumpFwd" ## Jump forward
    opcJumpFwdT = "jumpFwdT" ## Jump forward if true
    opcJumpFwdF = "jumpFwdF" ## Jump forward if false
    opcJumpBack = "jumpBack" ## Jump backward
    opcHalt = "halt"

  LineInfo* = tuple ## Line information.
    ln, col: int
    runLength: int
  Chunk* = ref object ## A chunk of bytecode.
    module*: Module ## The module this chunk belongs to.
    code*: seq[uint8] ## The raw bytecode.
    ln*, col*: int ## The current line info used when emitting bytecode.
    lineInfo: seq[LineInfo] ## A list of run-length encoded line info.
    strings*: seq[string] ## A table of strings used in this chunk.

  Scope* = ref object of RootObj ## A local scope.
    syms*: Table[string, Sym]
  Module* = ref object of Scope ## \
      ## A module representing the global scope of a single source file.
    name*: string ## The name of the module.
  SymKind* = enum ## The kind of a symbol.
    skVar ## A ``var`` variable.
    skLet ## A ``let`` variable.
    skType ## A type.
    skProc ## A proc.
  TypeKind* = enum ## The kind of a type.
    tkPrimitive
    tkObject
  Sym* = ref object ## A symbol.
    name*: Node ## The name of the symbol.
    impl*: Node ## \
      ## The implementation of the symbol. May be ``nil`` if the 
      ## symbol is generated.
    case kind*: SymKind
    of skVar, skLet:
      varTy*: Sym ## The type of the variable. 
      varSet*: bool ## Is the variable set?
      varLocal*: bool ## Is the variable local?
      varStackPos*: int ## The position of a local variable on the stack.
    of skType:
      case tyKind*: TypeKind
      of tkPrimitive: discard
      of tkObject:
        objFields*: Table[string, ObjectField]
    of skProc:
      procId*: int ## The unique number of the proc.
      procParams*: seq[ProcParam] ## The proc's parameters.
      procReturnTy*: Sym ## The return type of the proc.
      procChunk*: Chunk ## The chunk of the proc.
  ObjectField* = tuple
    id: int
    name: Node
    ty: Sym
  ProcParam* = tuple ## A single param of a proc.
    name: Node
    ty: Sym

const skVars* = {skVar, skLet}

proc addLineInfo*(chunk: var Chunk, n: int) =
  ## Add ``n`` line info entries to the chunk.
  if chunk.lineInfo.len > 0:
    if chunk.lineInfo[^1].ln == chunk.ln and
       chunk.lineInfo[^1].col == chunk.col:
      inc(chunk.lineInfo[^1].runLength, n)
      return
  chunk.lineInfo.add((chunk.ln, chunk.col, n))

proc getString*(chunk: var Chunk, str: string): uint16 =
  ## Get a string ID from a chunk, adding it to the chunk's string list if it
  ## isn't already stored.
  ## TODO: This is an `O(n)` operation, depending on the number of strings
  ## already stored in the chunk. Optimize this.
  if str notin chunk.strings:
    result = chunk.strings.len.uint16
    chunk.strings.add(str)
  else:
    result = chunk.strings.find(str).uint16

proc emit*(chunk: var Chunk, opc: Opcode) =
  ## Emit an opcode.
  chunk.addLineInfo(1)
  chunk.code.add(opc.uint8)

proc emit*(chunk: var Chunk, u8: uint8) =
  ## Emit a ``uint8``.
  chunk.addLineInfo(1)
  chunk.code.add(u8)

proc emit*(chunk: var Chunk, u16: uint16) =
  ## Emit a ``uint16``.
  chunk.addLineInfo(2)
  chunk.code.add([uint8 u16 and 0x00ff'u16,
                  uint8 (u16 and 0xff00'u16) shr 8])

proc emit*(chunk: var Chunk, val: Value) =
  ## Emit a Value.
  chunk.addLineInfo(ValueSize)
  chunk.code.add(val.rawBytes)

proc emitHole*(chunk: var Chunk, size: int): int =
  ## Emit a hole, to be filled later by ``fillHole``.
  result = chunk.code.len
  chunk.addLineInfo(size)
  for i in 1..size:
    chunk.code.add(0x00)

proc fillHole*(chunk: var Chunk, hole: int, val: uint8) =
  ## Fill a hole with an 8-bit value.
  chunk.code[hole] = val

proc fillHole*(chunk: var Chunk, hole: int, val: uint16) =
  ## Fill a hole with a 16-bit value.
  chunk.code[hole] = uint8(val and 0x00ff)
  chunk.code[hole + 1] = uint8((val and 0xff00) shr 8)

proc getOpcode*(chunk: Chunk, i: int): Opcode =
  ## Get the opcode at position ``i``.
  result = chunk.code[i].Opcode

proc getU8*(chunk: Chunk, i: int): uint8 =
  ## Gets the ``uint8`` at position ``i``.
  result = chunk.code[i]

proc getU16*(chunk: Chunk, i: int): uint16 =
  ## Get the ``uint16`` at position ``i``.
  result = chunk.code[i].uint16 or chunk.code[i + 1].uint16 shl 8

proc getValue*(chunk: Chunk, i: int): Value =
  ## Get a constant Value at position ``i``.
  var
    bytes: array[ValueSize, uint8]
    raw = cast[ptr UncheckedArray[uint8]](chunk.code[i].unsafeAddr)
  for i in low(bytes)..high(bytes):
    bytes[i] = raw[i]
  result =
    Value(rawBytes: bytes)

proc getLineInfo*(chunk: Chunk, i: int): LineInfo =
  ## Get the line info at position ``i``. **Warning:** This is very slow,
  ## because it has to walk the entire chunk decoding the run length encoded
  ## line info!
  var n = 0
  for li in chunk.lineInfo:
    for r in 1..li.runLength:
      if n == i:
        return li
      inc(n)

proc initChunk*(): Chunk =
  ## Create a new chunk.
  result = Chunk()

proc `$`*(sym: Sym): string =
  ## Stringify a symbol.
  case sym.kind
  of skVar:
    result = "var of type " & $sym.varTy.name
  of skLet:
    result = "let of type " & $sym.varTy.name
  of skType:
    result = "type = "
    case sym.tyKind
    of tkPrimitive:
      result.add("primitive")
    of tkObject:
      result.add("object {")
      for name, field in sym.objFields:
        result.add(" " & name & ": " & $field.ty.name & ";")
      result.add(" }")
  of skProc:
    result = "proc " & $sym.procId & "("
    for i, param in sym.procParams:
      result.add($param.name & ": " & $param.ty)
      if i != sym.procParams.len - 1:
        result.add(", ")

proc newSym*(kind: SymKind, name: Node, impl: Node = nil): Sym =
  ## Create a new symbol from a Node.
  result = Sym(name: name, impl: impl, kind: kind)

proc genSym*(kind: SymKind, name: string): Sym =
  ## Generate a new symbol from a string name.
  result = Sym(name: Node(kind: nkIdent, ident: name), kind: kind)

proc newType*(kind: TypeKind, name: Node): Sym =
  ## Create a new type symbol from a Node.
  result = Sym(name: name, kind: skType, tyKind: kind)

proc genType*(kind: TypeKind, name: string): Sym =
  ## Generate a new type symbol from a string name.
  result = Sym(name: Node(kind: nkIdent, ident: name), kind: skType,
               tyKind: kind)

proc `$`*(module: Module): string =
  ## Stringifies a module.
  result = "module " & module.name & ":"
  for name, sym in module.syms:
    result.add("\n")
    result.add("  ")
    result.add(name)
    result.add(": ")
    result.add($sym)

proc addPrimitiveType*(module: var Module, name: string) =
  ## Add a simple (primitive) type into a module.
  var sym = genType(tkPrimitive, name)
  module.syms.add(name, sym)

proc sym*(module: Module, name: string): Sym =
  ## Lookup the symbol ``name`` from a module.
  result = module.syms[name]

proc load*(module: var Module, other: Module) =
  ## Import the module ``other`` into ``module``.
  for name, sym in other.syms:
    module.syms.add(name, sym)

var rodSystem = Module(name: "system") ## The ``system`` module of rod.
rodSystem.addPrimitiveType("void")
rodSystem.addPrimitiveType("bool")
rodSystem.addPrimitiveType("number")

proc initModule*(name: string): Module =
  ## Initialize a new module. This implicitly loads ``system`` into the newly
  ## created module.
  result = Module(name: name)
  result.load(rodSystem)
