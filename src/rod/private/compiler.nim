#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import macros
import tables

import chunk
import common
import parser
import types
import value

type
  Register* = uint8
  Variable = ref object
    name: string
    ty: RodType
    global, mutable, isSet: bool
    value: Register
  Compiler* = object
    globals: Table[string, Variable]
    registers: array[0..255, bool]
    types: Table[string, RodType]

proc error(node: Node, msg: string) =
  raise (ref RodError)(kind: reCompile, ln: node.ln, col: node.col,
                       msg: $node.ln & ':' & $node.col & ' ' & msg)

proc alloc*(compiler: var Compiler): Register =
  for i, r in compiler.registers:
    if not r:
      compiler.registers[i] = true
      return i.uint8

proc free(compiler: var Compiler, reg: Register) =
  compiler.registers[reg] = false

proc addType*(compiler: var Compiler, name: string) =
  let ty = (id: compiler.types.len, name: name)
  compiler.types.add(name, ty)

template compilerGuard(body) =
  chunk.ln = node.ln
  chunk.col = node.col
  body

macro compiler(pc) =
  pc[3].insert(1,
    newIdentDefs(ident"chunk", newNimNode(nnkVarTy).add(ident"Chunk")))
  pc[3].insert(1,
    newIdentDefs(ident"compiler", newNimNode(nnkVarTy).add(ident"Compiler")))
  if pc[6].kind != nnkEmpty:
    pc[6] = newCall("compilerGuard", pc[6])
  result = pc

proc getConst(chunk: var Chunk, val: RodValue): uint16 =
  for i, c in chunk.consts[val.kind]:
    if c == val: return i.uint16
  result = chunk.consts[val.kind].len.uint16
  chunk.consts[val.kind].add(val)

proc declareVar*(compiler: var Compiler,
                 name: string, mut: bool): Variable =
  result = Variable(name: name, global: true, mutable: mut)
  compiler.globals.add(name, result)

proc getVar*(compiler: Compiler, node: Node): Variable =
  if compiler.globals.hasKey(node.ident):
    result = compiler.globals[node.ident]
  else:
    node.error("Attempt to reference undeclared variable '" & node.ident & '\'')

proc setVar*(compiler: Compiler, chunk: var Chunk, node: Node, val: Register) =
  var variable = compiler.getVar(node)
  if not variable.mutable and variable.isSet:
    node.error("Cannot reassign 'let' variable")
  chunk.emit(opcMoveRV, val, chunk.getConst(node.ident.rod))
  variable.isSet = true

proc compileValue*(node: Node, dest: Register): RodType {.compiler.}

proc moveConst(node: Node, dest: Register): RodType {.compiler.} =
  case node.kind
  of nkNumber:
    chunk.emit(opcMoveN, dest, chunk.getConst(node.numberVal.rod))
    result = compiler.types["number"]
  else: discard

proc prefix(node: Node, dest: Register): RodType {.compiler.} =
  var typeMismatch = false
  let
    source = compiler.alloc()
    sourceTy = compiler.compileValue(chunk, node[1], source)
  if sourceTy == compiler.types["number"]:
    case node[0].ident
    of "+": discard # + is a noop
    of "-": chunk.emit(opcNegN, dest, source, 0)
    else: typeMismatch = true
    if not typeMismatch: result = sourceTy
  else: typeMismatch = true
  if typeMismatch:
    node.error("No overload of '" & node[0].ident & "' available for <" &
                 sourceTy.name & ">")
  compiler.free(source)

proc infix(node: Node, dest: Register): RodType {.compiler.} =
  var typeMismatch = false
  let
    a = compiler.alloc()
    b = compiler.alloc()
    tyA = compiler.compileValue(chunk, node[1], a)
    tyB = compiler.compileValue(chunk, node[2], b)
  if tyA == compiler.types["number"] and tyB == compiler.types["number"]:
    case node[0].ident
    of "+": chunk.emit(opcAddN, dest, a, b)
    of "-": chunk.emit(opcSubN, dest, a, b)
    of "*": chunk.emit(opcMultN, dest, a, b)
    of "/": chunk.emit(opcDivN, dest, a, b)
    else: typeMismatch = true
    if not typeMismatch: result = tyA
  else: typeMismatch = true
  if typeMismatch:
    node.error("No overload of '" & node[0].ident & "' available for <" &
               tyA.name & ", " & tyB.name & ">")
  compiler.free(b)
  compiler.free(a)

proc compileValue*(node: Node, dest: Register): RodType {.compiler.} =
  result =
    case node.kind
    of nkNumber: compiler.moveConst(chunk, node, dest)
    of nkPrefix: compiler.prefix(chunk, node, dest)
    of nkInfix: compiler.infix(chunk, node, dest)
    else: compiler.types["error type"]

proc compileStmt*(node: Node) {.compiler.} =
  case node.kind
  of nkLet, nkVar:
    for decl in node.children:
      var variable = compiler.declareVar(decl[1].ident, node.kind == nkVar)
      let value = compiler.alloc()
      variable.ty = compiler.compileValue(chunk, decl[2], value)
      compiler.setVar(chunk, decl[1], value)
      if variable.global:
        compiler.free(value)
  else:
    let temp = compiler.alloc()
    discard compiler.compileValue(chunk, node, temp)
    compiler.free(temp)

proc initTypes(compiler: var Compiler) =
  compiler.addType("error type")
  compiler.addType("number")

proc initCompiler*(): Compiler =
  result = Compiler()
  result.initTypes()
