#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import macros
import strformat
import tables

import chunk
import common
import parser
import types
import value

type
  Loop = object
    before: int
    breaks: seq[int]
  Scope = object
    locals: Table[string, Variable]
    types: Table[string, RodType]
  CodeGen* = object
    scopes: seq[Scope]
    loops: seq[Loop]

proc error(node: Node, msg: string) =
  raise (ref RodError)(kind: reCompile, ln: node.ln, col: node.col,
                       msg: fmt"{node.file} {node.ln}:{node.col} {msg}")

template genGuard(body) =
  chunk.ln = node.ln
  chunk.col = node.col
  body

macro codegen(pc) =
  pc[3].insert(1,
    newIdentDefs(ident"module", newNimNode(nnkVarTy).add(ident"Module")))
  pc[3].insert(1,
    newIdentDefs(ident"chunk", newNimNode(nnkVarTy).add(ident"Chunk")))
  pc[3].insert(1,
    newIdentDefs(ident"gen", newNimNode(nnkVarTy).add(ident"CodeGen")))
  if pc[6].kind != nnkEmpty:
    pc[6] = newCall("genGuard", pc[6])
  result = pc

proc getConst(chunk: var Chunk, val: RodValue): uint16 =
  for i, c in chunk.consts[val.kind]:
    if c == val: return i.uint16
  result = chunk.consts[val.kind].len.uint16
  chunk.consts[val.kind].add(val)

proc pushScope(gen: var CodeGen) =
  gen.scopes.add(Scope())

proc popScope(gen: var CodeGen, chunk: var Chunk) =
  let v = gen.scopes[^1].locals.len
  if v > 0:
    chunk.emit(opcNDiscard)
    chunk.emit(v.uint8)
  discard gen.scopes.pop()

proc declareVar(gen: var CodeGen, module: var Module,
                name: string, mut: bool, ty: RodType) =
  if gen.scopes.len > 0:
    gen.scopes[^1].locals.add(name, Variable(
      name: name, isMutable: mut, ty: ty,
      kind: vkLocal,
      stackPos: gen.scopes[^1].locals.len,
      scope: gen.scopes.len))
  else:
    module.globals.add(name, Variable(name: name, isMutable: mut, ty: ty,
                                      kind: vkGlobal))

proc getVar(gen: CodeGen, module: Module, node: Node): Variable =
  if gen.scopes.len > 0:
    for i in countdown(gen.scopes.len - 1, 0):
      if node.ident in gen.scopes[i].locals:
        return gen.scopes[i].locals[node.ident]
  else:
    if module.globals.hasKey(node.ident):
      return module.globals[node.ident]
  node.error(fmt"Attempt to reference undeclared variable '{node.ident}'")

proc popVar(gen: var CodeGen, chunk: var Chunk, module: var Module,
            node: Node) =
  var
    variable: Variable
    scopeIndex: int
  if gen.scopes.len > 0:
    for i in countdown(gen.scopes.len - 1, 0):
      if node.ident in gen.scopes[i].locals:
        variable = gen.scopes[i].locals[node.ident]
        scopeIndex = i
        break
  else:
    if module.globals.hasKey(node.ident):
      variable = module.globals[node.ident]
  if not variable.isMutable and variable.isSet:
    node.error(fmt"Attempt to assign to 'let' variable '{node.ident}'")
  else:
    if variable.kind == vkLocal:
      if variable.isSet:
        chunk.emit(opcPopL)
        chunk.emit(variable.stackPos.uint8)
      gen.scopes[scopeIndex].locals[node.ident].isSet = true
    else:
      chunk.emit(opcPopG)
      chunk.emit(chunk.getConst(node.ident.rod))
      module.globals[node.ident].isSet = true
    return
  node.error(fmt"Attempt to assign to undeclared variable '{node.ident}'")

proc pushVar(gen: CodeGen, chunk: var Chunk, variable: Variable) =
  if variable.kind == vkLocal:
    chunk.emit(opcPushL)
    chunk.emit(variable.stackPos.uint8)
  else:
    chunk.emit(opcPushG)
    chunk.emit(chunk.getConst(variable.name.rod))

proc typeName(node: Node): string =
  case node.kind
  of nkIdent: result = node.ident
  of nkIndex:
    result = "["
    for i, child in node.children:
      result.add(child.typeName)
      if i < node.children.len - 1:
        result.add(", ")
    result.add("]")
  else: discard

proc getTy(gen: CodeGen, module: Module, name: Node): RodType =
  let oname = name.typeName
  if gen.scopes.len > 0:
    for i in countdown(gen.scopes.len - 1, 0):
      if oname in gen.scopes[i].types:
        return gen.scopes[i].types[oname]
  if oname in module.types:
    return module.ty(oname)
  name.error(fmt"Attempt to reference non-existent type '{oname}'")

proc genExpr(node: Node): RodType {.codegen.}

proc pushConst(node: Node): RodType {.codegen.} =
  case node.kind
  of nkBool:
    if node.boolVal == true: chunk.emit(opcPushTrue)
    else: chunk.emit(opcPushFalse)
    result = module.ty("bool")
  of nkNumber:
    chunk.emit(opcPushN)
    chunk.emit(chunk.getConst(node.numberVal.rod))
    result = module.ty("number")
  else: discard

proc prefix(node: Node): RodType {.codegen.} =
  var
    typeMismatch = false
  let ty = gen.genExpr(chunk, module, node[1])
  if ty == module.ty("number"):
    case node[0].ident
    of "+": discard # + is a noop
    of "-": chunk.emit(opcNegN)
    else: typeMismatch = true
    if not typeMismatch: result = ty
  elif ty == module.ty("bool"):
    case node[0].ident
    of "not": chunk.emit(opcInvB)
    else: typeMismatch = true
    if not typeMismatch: result = ty
  else: typeMismatch = true
  if typeMismatch:
    node.error(fmt"No overload of '{node[0].ident}' available for <{ty.name}>")

proc infix(node: Node): RodType {.codegen.} =
  if node[0].ident notin ["=", "or", "and"]:
    var typeMismatch = false
    let
      aTy = gen.genExpr(chunk, module, node[1])
      bTy = gen.genExpr(chunk, module, node[2])
    if aTy == bTy and aTy == module.ty("number"):
      case node[0].ident
      of "+": chunk.emit(opcAddN)
      of "-": chunk.emit(opcSubN)
      of "*": chunk.emit(opcMultN)
      of "/": chunk.emit(opcDivN)
      of "==": chunk.emit(opcEqN)
      of "!=": chunk.emit(opcEqN); chunk.emit(opcInvB)
      of "<": chunk.emit(opcLessN)
      of "<=": chunk.emit(opcLessEqN)
      of ">": chunk.emit(opcGreaterN)
      of ">=": chunk.emit(opcGreaterEqN)
      else: typeMismatch = true
      if not typeMismatch: result =
        case node[0].ident
        of "+", "-", "*", "/": module.ty("number")
        of "==", "!=", "<", "<=", ">", ">=": module.ty("bool")
        else: module.ty("void")
    elif aTy == bTy and aTy == module.ty("bool"):
      case node[0].ident
      of "==": chunk.emit(opcEqB)
      of "!=": chunk.emit(opcEqB); chunk.emit(opcInvB)
      else: typeMismatch = true
      if not typeMismatch: result = aTy
    else: typeMismatch = true
    if typeMismatch:
      node.error(fmt"No overload of '{node[0].ident}' available for" &
                 fmt"<{aTy.name}, {bTy.name}>")
  else:
    case node[0].ident
    of "=":
      case node[1].kind
      of nkIdent:
        var variable = gen.getVar(module, node[1])
        let valTy = gen.genExpr(chunk, module, node[2])
        if valTy == variable.ty:
          gen.popVar(chunk, module, node[1])
        else:
          node.error(fmt"Type mismatch: cannot assign value of type " &
                     fmt"<{valTy.name}> to a variable of type " &
                     fmt"<{variable.ty.name}>")
        result = module.ty("void")
      else: node.error(fmt"Cannot assign to '{($node.kind)[2..^1]}'")
    of "or":
      let aTy = gen.genExpr(chunk, module, node[1])
      chunk.emit(opcJumpFwdT)
      let hole = chunk.emitHole(2)
      chunk.emit(opcDiscard)
      let bTy = gen.genExpr(chunk, module, node[2])
      if aTy != module.ty("bool") or bTy != module.ty("bool"):
        node.error("Operands of 'or' must be booleans")
      chunk.fillHole(hole, uint16(chunk.code.len - hole + 1))
      result = module.ty("bool")
    of "and":
      let aTy = gen.genExpr(chunk, module, node[1])
      chunk.emit(opcJumpFwdF)
      let hole = chunk.emitHole(2)
      chunk.emit(opcDiscard)
      let bTy = gen.genExpr(chunk, module, node[2])
      if aTy != module.ty("bool") or bTy != module.ty("bool"):
        node.error("Operands of 'and' must be booleans")
      chunk.fillHole(hole, uint16(chunk.code.len - hole + 1))
      result = module.ty("bool")
    else: discard

proc genBlock(node: Node, isStmt: bool): RodType {.codegen.}

proc genIf(node: Node, isStmt: bool): RodType {.codegen.} =
  var
    pos = 0
    jumpsToEnd: seq[int]
    ifTy: RodType
    hadElse = false
  while pos < node.children.len:
    if node[pos].kind != nkBlock:
      if gen.genExpr(chunk, module, node[pos]) != module.ty("bool"):
        node[pos].error("'if' condition must be a boolean")
      inc(pos)
      chunk.emit(opcJumpFwdF)
      let afterBlock = chunk.emitHole(2)
      chunk.emit(opcDiscard)
      let blockTy = gen.genBlock(chunk, module, node[pos], isStmt)
      if not isStmt:
        if ifTy == nil:
          ifTy = blockTy
        else:
          if blockTy != ifTy:
            node[pos].error(fmt"Type mismatch: <{ifTy.name}> expected, but " &
                            fmt"got <{blockTy.name}>")
      if pos < node.children.len - 1:
        chunk.emit(opcJumpFwd)
        jumpsToEnd.add(chunk.emitHole(2))
      inc(pos)
      chunk.fillHole(afterBlock, uint16(chunk.code.len - afterBlock + 1))
    else:
      chunk.emit(opcDiscard)
      let blockTy = gen.genBlock(chunk, module, node[pos], isStmt)
      if not isStmt:
        if blockTy != ifTy:
          node[pos].error(fmt"Type mismatch: <{ifTy.name}> expected, but got " &
                          fmt"<{blockTy.name}>")
      hadElse = true
      inc(pos)
  if not hadElse:
    chunk.emit(opcDiscard)
  for hole in jumpsToEnd:
    chunk.fillHole(hole, uint16(chunk.code.len - hole + 1))
  result =
    if isStmt: module.ty("void")
    else: module.ty("void")

proc genExpr(node: Node): RodType {.codegen.} =
  case node.kind
  of nkBool, nkNumber:
    result = gen.pushConst(chunk, module, node)
  of nkIdent:
    var variable = gen.getVar(module, node)
    gen.pushVar(chunk, variable)
    result = variable.ty
  of nkPrefix:
    result = gen.prefix(chunk, module, node)
  of nkInfix:
    result = gen.infix(chunk, module, node)
  of nkIf:
    result = gen.genIf(chunk, module, node, false)
  else: node.error("Value does not have a valid type")

proc genWhile(node: Node) {.codegen.} =
  var
    isWhileTrue = false
    afterLoop: int
  let beforeLoop = chunk.code.len
  gen.loops.add(Loop(before: beforeLoop))
  if node[0].kind == nkBool:
    if node[0].boolVal == true: isWhileTrue = true
    else: return # while false is a noop
  if not isWhileTrue:
    if gen.genExpr(chunk, module, node[0]) != module.ty("bool"):
      node[0].error("'while' condition must be a boolean")
    chunk.emit(opcJumpFwdF)
    afterLoop = chunk.emitHole(2)
    chunk.emit(opcDiscard)
  discard gen.genBlock(chunk, module, node[1], true)
  chunk.emit(opcJumpBack)
  chunk.emit(uint16(chunk.code.len - beforeLoop - 1))
  if not isWhileTrue:
    chunk.fillHole(afterLoop, uint16(chunk.code.len - afterLoop + 1))
  for brk in gen.loops[^1].breaks:
    chunk.fillHole(brk, uint16(chunk.code.len - brk + 1))
  discard gen.loops.pop()

proc genBreak(node: Node) {.codegen.} =
  if gen.loops.len == 0:
    node.error("'break' can only be used in a loop")
  chunk.emit(opcNDiscard)
  chunk.emit(gen.scopes[^1].locals.len.uint8)
  chunk.emit(opcJumpFwd)
  gen.loops[^1].breaks.add(chunk.emitHole(2))

proc genContinue(node: Node) {.codegen.} =
  if gen.loops.len < 1:
    node.error("'continue' can only be used in a loop")
  chunk.emit(opcNDiscard)
  chunk.emit(gen.scopes[^1].locals.len.uint8)
  chunk.emit(opcJumpBack)
  chunk.emit(uint16(chunk.code.len - gen.loops[^1].before))

proc genObject(node: Node) {.codegen.} =
  var ty = RodType(kind: tkObject, name: node[0].typeName)
  for fields in node.children[1..^1]:
    let fieldsTy = gen.getTy(module, fields.children[^1])
    var fieldNames: seq[string]
    for name in fields.children[0..^2]:
      fieldNames.add(name.ident)
    for name in fieldNames:
      ty.objFields.add(name, (ty.objFields.len, name, fieldsTy))
  if gen.scopes.len > 0:
    gen.scopes[^1].types.add(ty.name, ty)
  else:
    module.types.add(ty.name, ty)

proc genStmt(node: Node) {.codegen.} =
  case node.kind
  of nkLet, nkVar:
    for decl in node.children:
      let valTy = gen.genExpr(chunk, module, decl[2])
      if decl[3].kind != nkEmpty:
        let expectedTy = gen.getTy(module, decl[3])
        if valTy != expectedTy:
          decl[2].error("Value does not match the specified type")
      gen.declareVar(module, decl[1].ident, node.kind == nkVar, valTy)
      gen.popVar(chunk, module, decl[1])
  of nkBlock: discard gen.genBlock(chunk, module, node, true)
  of nkIf: discard gen.genIf(chunk, module, node, true)
  of nkWhile: gen.genWhile(chunk, module, node)
  of nkBreak: gen.genBreak(chunk, module, node)
  of nkContinue: gen.genContinue(chunk, module, node)
  of nkObject: gen.genObject(chunk, module, node)
  else:
    let ty = gen.genExpr(chunk, module, node)
    if ty != module.ty("void"):
      chunk.emit(opcDiscard)

proc genBlock(node: Node, isStmt: bool): RodType {.codegen.} =
  gen.pushScope()
  for i, s in node.children:
    if isStmt:
      gen.genStmt(chunk, module, s)
    else:
      if i < node.children.len - 1:
        gen.genStmt(chunk, module, s)
      else:
        result = gen.genExpr(chunk, module, s)
  gen.popScope(chunk)
  if isStmt: result = module.ty("void")

proc genScript*(node: Node) {.codegen.} =
  for s in node.children:
    gen.genStmt(chunk, module, s)
  chunk.emit(opcHalt)

proc initCompiler*(): CodeGen =
  result = CodeGen()
