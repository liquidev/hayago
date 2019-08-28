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
  Loop = object
    before: int
    breaks: seq[int]
  Scope = object
    locals: Table[string, Variable]
    types: seq[RodType]
  Compiler* = object
    scopes: seq[Scope]
    loops: seq[Loop]

proc error(node: Node, msg: string) =
  raise (ref RodError)(kind: reCompile, ln: node.ln, col: node.col,
                       msg: node.file & " " & $node.ln & ':' & $node.col & ' ' &
                            msg)

template compilerGuard(body) =
  chunk.ln = node.ln
  chunk.col = node.col
  body

macro compiler(pc) =
  pc[3].insert(1,
    newIdentDefs(ident"module", newNimNode(nnkVarTy).add(ident"Module")))
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

proc pushScope(compiler: var Compiler) =
  compiler.scopes.add(Scope())

proc popScope(compiler: var Compiler, chunk: var Chunk) =
  let v = compiler.scopes[^1].locals.len
  if v > 0:
    chunk.emit(opcNDiscard)
    chunk.emit(v.uint8)
  discard compiler.scopes.pop()

proc declareVar(compiler: var Compiler, module: var Module,
                name: string, mut: bool, ty: RodType) =
  if compiler.scopes.len > 0:
    compiler.scopes[^1].locals.add(name, Variable(
      name: name, isMutable: mut, ty: ty,
      kind: vkLocal,
      stackPos: compiler.scopes[^1].locals.len,
      scope: compiler.scopes.len))
  else:
    module.globals.add(name, Variable(name: name, isMutable: mut, ty: ty,
                                      kind: vkGlobal))

proc getVar(compiler: Compiler, module: Module, node: Node): Variable =
  if compiler.scopes.len > 0:
    for i in countdown(compiler.scopes.len - 1, 0):
      if node.ident in compiler.scopes[i].locals:
        return compiler.scopes[i].locals[node.ident]
  else:
    if module.globals.hasKey(node.ident):
      return module.globals[node.ident]
  node.error("Attempt to reference undeclared variable '" & node.ident & '\'')

proc popVar(compiler: var Compiler, chunk: var Chunk, module: var Module,
            node: Node) =
  var
    variable: Variable
    scopeIndex: int
  if compiler.scopes.len > 0:
    for i in countdown(compiler.scopes.len - 1, 0):
      if node.ident in compiler.scopes[i].locals:
        variable = compiler.scopes[i].locals[node.ident]
        scopeIndex = i
        break
  else:
    if module.globals.hasKey(node.ident):
      variable = module.globals[node.ident]
  if not variable.isMutable and variable.isSet:
    node.error("Attempt to assign to 'let' variable '" & node.ident & '\'')
  else:
    if variable.kind == vkLocal:
      if variable.isSet:
        chunk.emit(opcPopL)
        chunk.emit(variable.stackPos.uint8)
      compiler.scopes[scopeIndex].locals[node.ident].isSet = true
    else:
      chunk.emit(opcPopG)
      chunk.emit(chunk.getConst(node.ident.rod))
      module.globals[node.ident].isSet = true
    return
  node.error("Attempt to assign to undeclared variable '" & node.ident & '\'')

proc pushVar(compiler: Compiler, chunk: var Chunk, variable: Variable) =
  if variable.kind == vkLocal:
    chunk.emit(opcPushL)
    chunk.emit(variable.stackPos.uint8)
  else:
    chunk.emit(opcPushG)
    chunk.emit(chunk.getConst(variable.name.rod))

proc compileExpr(node: Node): RodType {.compiler.}

proc pushConst(node: Node): RodType {.compiler.} =
  case node.kind
  of nkBool:
    if node.boolVal == true: chunk.emit(opcPushTrue)
    else: chunk.emit(opcPushFalse)
    result = module.types["bool"]
  of nkNumber:
    chunk.emit(opcPushN)
    chunk.emit(chunk.getConst(node.numberVal.rod))
    result = module.types["number"]
  else: discard

proc prefix(node: Node): RodType {.compiler.} =
  var
    typeMismatch = false
  let ty = compiler.compileExpr(chunk, module, node[1])
  if ty == module.types["number"]:
    case node[0].ident
    of "+": discard # + is a noop
    of "-": chunk.emit(opcNegN)
    else: typeMismatch = true
    if not typeMismatch: result = ty
  elif ty == module.types["bool"]:
    case node[0].ident
    of "not": chunk.emit(opcInvB)
    else: typeMismatch = true
    if not typeMismatch: result = ty
  else: typeMismatch = true
  if typeMismatch:
    node.error("No overload of '" & node[0].ident & "' available for <" &
                ty.name & ">")

proc infix(node: Node): RodType {.compiler.} =
  if node[0].ident notin ["=", "or", "and"]:
    var typeMismatch = false
    let
      aTy = compiler.compileExpr(chunk, module, node[1])
      bTy = compiler.compileExpr(chunk, module, node[2])
    if aTy == module.types["number"] and bTy == module.types["number"]:
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
        of "+", "-", "*", "/": module.types["number"]
        of "==", "!=", "<", "<=", ">", ">=": module.types["bool"]
        else: module.types["void"]
    elif aTy == module.types["bool"] and bTy == module.types["bool"]:
      case node[0].ident
      of "==": chunk.emit(opcEqB)
      of "!=": chunk.emit(opcEqB); chunk.emit(opcInvB)
      else: typeMismatch = true
      if not typeMismatch: result = aTy
    else: typeMismatch = true
    if typeMismatch:
      node.error("No overload of '" & node[0].ident & "' available for <" &
                 aTy.name & ", " & bTy.name & ">")
  else:
    case node[0].ident
    of "=":
      case node[1].kind
      of nkIdent:
        var variable = compiler.getVar(module, node[1])
        let valTy = compiler.compileExpr(chunk, module, node[2])
        if valTy == variable.ty:
          compiler.popVar(chunk, module, node[1])
        else:
          node.error("Type mismatch: cannot assign value of type <" &
                     valTy.name & "> to a variable of type <" &
                     variable.ty.name & ">")
        result = module.types["void"]
      else: node.error("Cannot assign to '" & ($node.kind)[2..^1] & "'")
    of "or":
      let aTy = compiler.compileExpr(chunk, module, node[1])
      chunk.emit(opcJumpFwdT)
      let hole = chunk.emitHole(2)
      chunk.emit(opcDiscard)
      let bTy = compiler.compileExpr(chunk, module, node[2])
      if aTy != module.types["bool"] or bTy != module.types["bool"]:
        node.error("Operands of 'or' must be booleans")
      chunk.fillHole(hole, uint16(chunk.code.len - hole + 1))
      result = module.types["bool"]
    of "and":
      let aTy = compiler.compileExpr(chunk, module, node[1])
      chunk.emit(opcJumpFwdF)
      let hole = chunk.emitHole(2)
      chunk.emit(opcDiscard)
      let bTy = compiler.compileExpr(chunk, module, node[2])
      if aTy != module.types["bool"] or bTy != module.types["bool"]:
        node.error("Operands of 'and' must be booleans")
      chunk.fillHole(hole, uint16(chunk.code.len - hole + 1))
      result = module.types["bool"]
    else: discard

proc compileBlock(node: Node, isStmt: bool): RodType {.compiler.}

proc compileIf(node: Node, isStmt: bool): RodType {.compiler.} =
  var
    pos = 0
    jumpsToEnd: seq[int]
    ifTy: RodType
    hadElse = false
  while pos < node.children.len:
    if node[pos].kind != nkBlock:
      if compiler.compileExpr(chunk, module, node[pos]) != module.types["bool"]:
        node[pos].error("'if' condition must be a boolean")
      inc(pos)
      chunk.emit(opcJumpFwdF)
      let afterBlock = chunk.emitHole(2)
      chunk.emit(opcDiscard)
      let blockTy = compiler.compileBlock(chunk, module, node[pos], isStmt)
      if not isStmt:
        if ifTy == nil:
          ifTy = blockTy
        else:
          if blockTy != ifTy:
            node[pos].error("Type mismatch: <" & ifTy.name & "> expected, " &
                            "but got <" & blockTy.name & ">")
      if pos < node.children.len - 1:
        chunk.emit(opcJumpFwd)
        jumpsToEnd.add(chunk.emitHole(2))
      inc(pos)
      chunk.fillHole(afterBlock, uint16(chunk.code.len - afterBlock + 1))
    else:
      chunk.emit(opcDiscard)
      let blockTy = compiler.compileBlock(chunk, module, node[pos], isStmt)
      if not isStmt:
        if blockTy != ifTy:
          node[pos].error("Type mismatch: <" & ifTy.name & "> expected, but " &
                          "got <" & blockTy.name & ">")
      hadElse = true
      inc(pos)
  if not hadElse:
    chunk.emit(opcDiscard)
  for hole in jumpsToEnd:
    chunk.fillHole(hole, uint16(chunk.code.len - hole + 1))
  result =
    if isStmt: module.types["void"]
    else: module.types["void"]

proc compileExpr(node: Node): RodType {.compiler.} =
  case node.kind
  of nkBool, nkNumber:
    result = compiler.pushConst(chunk, module, node)
  of nkIdent:
    var variable = compiler.getVar(module, node)
    compiler.pushVar(chunk, variable)
    result = variable.ty
  of nkPrefix:
    result = compiler.prefix(chunk, module, node)
  of nkInfix:
    result = compiler.infix(chunk, module, node)
  of nkIf:
    result = compiler.compileIf(chunk, module, node, false)
  else: node.error("Value does not have a valid type")

proc compileWhile(node: Node) {.compiler.} =
  var
    isWhileTrue = false
    afterLoop: int
  let beforeLoop = chunk.code.len
  compiler.loops.add(Loop(before: beforeLoop))
  if node[0].kind == nkBool:
    if node[0].boolVal == true: isWhileTrue = true
    else: return # while false is a noop
  if not isWhileTrue:
    if compiler.compileExpr(chunk, module, node[0]) != module.types["bool"]:
      node[0].error("'while' condition must be a boolean")
    chunk.emit(opcJumpFwdF)
    afterLoop = chunk.emitHole(2)
    chunk.emit(opcDiscard)
  discard compiler.compileBlock(chunk, module, node[1], true)
  chunk.emit(opcJumpBack)
  chunk.emit(uint16(chunk.code.len - beforeLoop - 1))
  if not isWhileTrue:
    chunk.fillHole(afterLoop, uint16(chunk.code.len - afterLoop + 1))
  for brk in compiler.loops[^1].breaks:
    chunk.fillHole(brk, uint16(chunk.code.len - brk + 1))
  discard compiler.loops.pop()

proc compileBreak(node: Node) {.compiler.} =
  if compiler.loops.len == 0:
    node.error("'break' can only be used in a loop")
  chunk.emit(opcNDiscard)
  chunk.emit(compiler.scopes[^1].locals.len.uint8)
  chunk.emit(opcJumpFwd)
  compiler.loops[^1].breaks.add(chunk.emitHole(2))

proc compileContinue(node: Node) {.compiler.} =
  if compiler.loops.len == 0:
    node.error("'continue' can only be used in a loop")
  chunk.emit(opcNDiscard)
  chunk.emit(compiler.scopes[^1].locals.len.uint8)
  chunk.emit(opcJumpBack)
  chunk.emit(uint16(chunk.code.len - compiler.loops[^1].before))

proc compileObject(node: Node) {.compiler.} =
  discard

proc compileStmt(node: Node) {.compiler.} =
  case node.kind
  of nkLet, nkVar:
    for decl in node.children:
      compiler.declareVar(module, decl[1].ident, node.kind == nkVar,
                          compiler.compileExpr(chunk, module, decl[2]))
      compiler.popVar(chunk, module, decl[1])
  of nkBlock: discard compiler.compileBlock(chunk, module, node, true)
  of nkIf: discard compiler.compileIf(chunk, module, node, true)
  of nkWhile: compiler.compileWhile(chunk, module, node)
  of nkBreak: compiler.compileBreak(chunk, module, node)
  of nkContinue: compiler.compileContinue(chunk, module, node)
  of nkObject: compiler.compileObject(chunk, module, node)
  else:
    let ty = compiler.compileExpr(chunk, module, node)
    if ty != module.types["void"]:
      chunk.emit(opcDiscard)

proc compileBlock(node: Node, isStmt: bool): RodType {.compiler.} =
  compiler.pushScope()
  for i, s in node.children:
    if isStmt:
      compiler.compileStmt(chunk, module, s)
    else:
      if i < node.children.len - 1:
        compiler.compileStmt(chunk, module, s)
      else:
        result = compiler.compileExpr(chunk, module, s)
  compiler.popScope(chunk)
  if isStmt: result = module.types["void"]

proc compileScript*(node: Node) {.compiler.} =
  for s in node.children:
    compiler.compileStmt(chunk, module, s)
  chunk.emit(opcHalt)

proc initCompiler*(): Compiler =
  result = Compiler()
