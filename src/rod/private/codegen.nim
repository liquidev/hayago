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
import value

type
  Loop = ref object
    before: int
    breaks: seq[int]
    bottomScope: int
  CodeGen* = object
    module: Module
    chunk: Chunk
    scopes: seq[Scope]
    loops: seq[Loop]

proc initCodeGen*(module: Module, chunk: Chunk): CodeGen =
  result = CodeGen(module: module, chunk: chunk)

proc error(node: Node, msg: string) =
  ## Raise a compile error on the given node.
  raise (ref RodError)(kind: reCompile, ln: node.ln, col: node.col,
                       msg: fmt"{node.file} {node.ln}:{node.col} {msg}")

template genGuard(body) =
  ## Wraps ``body`` in a "guard" used for code generation. The guard sets the
  ## line information in the target chunk. This is a helper used by {.codegen.}.
  gen.chunk.ln = node.ln
  gen.chunk.col = node.col
  body

macro codegen(theProc: untyped): untyped =
  ## Wrap ``theProc``'s body in a call to ``genGuard``.
  theProc.params.insert(1, newIdentDefs(ident"gen",
                                        newTree(nnkVarTy, ident"CodeGen")))
  if theProc[6].kind != nnkEmpty:
    theProc[6] = newCall("genGuard", theProc[6])
  result = theProc

proc varCount(scope: Scope): int =
  ## Count the number of variables in a scope.
  for _, sym in scope.syms:
    if sym.kind in skVars:
      inc(result)

proc currentScope(gen: CodeGen): Scope =
  ## Returns the current scope.
  result = gen.scopes[^1]

proc pushScope(gen: var CodeGen) =
  ## Push a new scope.
  gen.scopes.add(Scope())

proc popScope(gen: var CodeGen) =
  ## Pop the top scope, discarding its variables.
  gen.chunk.emit(opcDiscard)
  gen.chunk.emit(gen.currentScope.varCount.uint8)
  discard gen.scopes.pop()

proc declareVar(gen: var CodeGen, name: Node, kind: SymKind, ty: Sym) =
  ## Declare a new variable with the given ``name``, of the given ``kind``, with
  ## the given type ``ty``.

  # create the symbol for the variable
  assert kind in skVars, "The kind must be a variable."
  var sym = newSym(kind, name)
  sym.varTy = ty
  sym.varSet = false
  if gen.scopes.len > 0:
    # declare a local
    sym.varLocal = true
    sym.varStackPos = gen.currentScope.varCount
    gen.currentScope.syms.add(name.ident, sym)
  else:
    # declare a global
    sym.varLocal = false
    gen.module.syms.add(name.ident, sym)

proc lookup(gen: CodeGen, name: Node): Sym =
  ## Look up the symbol with the given ``name``.
  if gen.scopes.len > 0:
    # try to find a local symbol
    for i in countdown(gen.scopes.len - 1, 0):
      if name.ident in gen.scopes[i].syms:
        return gen.scopes[i].syms[name.ident]
  if name.ident in gen.module.syms:
    # try to find a global symbol
    return gen.module.syms[name.ident]
  name.error(fmt"'{name.ident}' is not declared in the current scope")

proc popVar(gen: var CodeGen, name: Node) =
  ## Pop the value at the top of the stack to the variable ``name``.

  # first of all, look the variable up
  var
    sym: Sym
    scopeIndex: int
    isLocal = false
  block findVar:
    # try to find a local
    if gen.scopes.len > 0:
      for i in countdown(gen.scopes.len - 1, 0):
        if name.ident in gen.scopes[i].syms and
           gen.scopes[i].syms[name.ident].kind in skVars:
          sym = gen.scopes[i].syms[name.ident]
          scopeIndex = i
          isLocal = true
          break findVar
    # if there's no local by that name, try to find a global
    if name.ident in gen.module.syms and
       gen.module.syms[name.ident].kind in skVars:
      sym = gen.module.syms[name.ident]
      isLocal = false
      break findVar
    # if no variable was found, error out
    name.error(fmt"Attempt to assign to undeclared variable '{name.ident}'")
  if sym.kind == skLet and sym.varSet:
    # if the variable is 'let' and it's already set, error out
    name.error(fmt"Attempt to reassign 'let' variable '{name.ident}'")
  else:
    if isLocal:
      # if it's a local and it's already been set, use popL, otherwise, just
      # leave the variable on the stack
      if sym.varSet:
        gen.chunk.emit(opcPopL)
        gen.chunk.emit(sym.varStackPos.uint8)
    else:
      # if it's a global, always use popG
      gen.chunk.emit(opcPopG)
      gen.chunk.emit(gen.chunk.getString(name.ident))
    # mark the variable as set
    sym.varSet = true

proc pushVar(gen: var CodeGen, sym: Sym) =
  ## Push the variable represented by ``sym`` to the top of the stack.
  assert sym.kind in skVars, "The symbol must represent a variable."
  if sym.varLocal:
    # if the variable is a local, use pushL
    gen.chunk.emit(opcPushL)
    gen.chunk.emit(sym.varStackPos.uint8)
  else:
    # if it's a global, use pushG
    gen.chunk.emit(opcPushG)
    gen.chunk.emit(gen.chunk.getString(sym.name.ident))

proc genExpr(node: Node): Sym {.codegen.}

proc pushConst(node: Node): Sym {.codegen.} =
  ## Generate a push instruction for a constant value.
  case node.kind
  of nkBool:
    # bools - use pushTrue and pushFalse
    if node.boolVal == true: gen.chunk.emit(opcPushTrue)
    else: gen.chunk.emit(opcPushFalse)
    result = gen.module.sym"bool"
  of nkNumber:
    # numbers - use pushN with a number Value
    gen.chunk.emit(opcPushN)
    gen.chunk.emit(initValue(node.numberVal))
    result = gen.module.sym"number"
  else: discard

proc prefix(node: Node): Sym {.codegen.} =
  ## Generate instructions for a prefix operator.
  ## TODO: operator overloading.
  var typeMismatch = false # did a type mismatch occur?
  let ty = gen.genExpr(node[1]) # generate the operand's code
  if ty == gen.module.sym"number":
    # number operators
    case node[0].ident
    of "+": discard # + is a noop
    of "-": gen.chunk.emit(opcNegN)
    else: typeMismatch = true # unknown operator
    result = ty
  elif ty == gen.module.sym"bool":
    # bool operators
    case node[0].ident
    of "not": gen.chunk.emit(opcInvB)
    else: typeMismatch = true # unknown operator
    result = ty
  else: typeMismatch = true
  if typeMismatch:
    node.error(fmt"No overload of '{node[0].ident}' available for <{ty.name}>")

proc infix(node: Node): Sym {.codegen.} =
  ## Generate instructions for an infix operator.
  ## TODO: operator overloading.
  if node[0].ident notin ["=", "or", "and"]:
    # primitive operators
    var typeMismatch = false # did a type mismatch occur?
    let
      aTy = gen.genExpr(node[1]) # generate the left operand's code
      bTy = gen.genExpr(node[2]) # generate the right operand's code
    if aTy == bTy and aTy == gen.module.sym"number":
      # number operators
      case node[0].ident
      # arithmetic
      of "+": gen.chunk.emit(opcAddN)
      of "-": gen.chunk.emit(opcSubN)
      of "*": gen.chunk.emit(opcMultN)
      of "/": gen.chunk.emit(opcDivN)
      # relational
      of "==": gen.chunk.emit(opcEqN)
      of "!=": gen.chunk.emit(opcEqN); gen.chunk.emit(opcInvB)
      of "<": gen.chunk.emit(opcLessN)
      of "<=": gen.chunk.emit(opcLessEqN)
      of ">": gen.chunk.emit(opcGreaterN)
      of ">=": gen.chunk.emit(opcGreaterEqN)
      else: typeMismatch = true # unknown operator
      result =
        case node[0].ident
        # arithmetic operators return numbers
        of "+", "-", "*", "/": gen.module.sym"number"
        # relational operators return bools
        of "==", "!=", "<", "<=", ">", ">=": gen.module.sym"bool"
        else: nil # type mismatch; we don't care
    elif aTy == bTy and aTy == gen.module.sym"bool":
      # bool operators
      case node[0].ident
      # relational
      of "==": gen.chunk.emit(opcEqB)
      of "!=": gen.chunk.emit(opcEqB); gen.chunk.emit(opcInvB)
      else: typeMismatch = true
      # bool operators return bools (duh.)
      result = gen.module.sym"bool"
    else: typeMismatch = true # no operators for given type
    if typeMismatch:
      node.error(fmt"No overload of '{node[0].ident}' available for" &
                 fmt"<{aTy.name}, {bTy.name}>")
  else:
    case node[0].ident
    # assignment is special
    of "=":
      case node[1].kind
      of nkIdent: # to a variable
        let
          sym = gen.lookup(node[1]) # look the variable up
          valTy = gen.genExpr(node[2]) # generate the value
        if valTy == sym.varTy:
          # if the variable's type matches the type of the value, we're ok
          gen.popVar(node[1])
        else:
          node.error(fmt"Type mismatch: cannot assign value of type " &
                     fmt"<{valTy.name}> to a variable of type " &
                     fmt"<{sym.varTy.name}>")
      of nkDot: # to an object field
        if node[1][1].kind != nkIdent:
          # object fields are always identifiers
          node[1][1].error(fmt"{node[1][1]} is not a valid object field")
        let
          typeSym = gen.genExpr(node[1][0]) # generate the receiver's code
          fieldName = node[1][1].ident
          valTy = gen.genExpr(node[2]) # generate the value's code
        if typeSym.tyKind == tkObject and fieldName in typeSym.objFields:
          # assign the field if it's valid, using popF
          let field = typeSym.objFields[fieldName]
          if valTy != field.ty:
            node[2].error(fmt"Type mismatch: field has type <{field.ty.name}>" &
                          fmt" but got <{valTy.name}>")
          gen.chunk.emit(opcPopF)
          gen.chunk.emit(field.id.uint8)
        else:
          node[1].error(fmt"Field '{fieldName}' doesn't exist")
      else: node.error(fmt"Cannot assign to '{($node.kind)[2..^1]}'")
      # assignment doesn't return anything
      result = gen.module.sym"void"
    # ``or`` and ``and`` are special, because they're short-circuiting.
    # that's why they need a little more special care.
    of "or": # ``or``
      let aTy = gen.genExpr(node[1]) # generate the left-hand side
      # if it's ``true``, jump over the rest of the expression
      gen.chunk.emit(opcJumpFwdT)
      let hole = gen.chunk.emitHole(2)
      # otherwise, check the right-hand side
      gen.chunk.emit(opcDiscard)
      gen.chunk.emit(1'u8)
      let bTy = gen.genExpr(node[2]) # generate the right-hand side
      if aTy != gen.module.sym"bool" or bTy != gen.module.sym"bool":
        node.error("Operands of 'or' must be booleans")
      gen.chunk.fillHole(hole, uint16(gen.chunk.code.len - hole + 1))
      result = gen.module.sym"bool"
    of "and": # ``and``
      let aTy = gen.genExpr(node[1]) # generate the left-hand side
      # if it's ``false``, jump over the rest of the expression
      gen.chunk.emit(opcJumpFwdF)
      let hole = gen.chunk.emitHole(2)
      # otherwise, check the right-hand side
      gen.chunk.emit(opcDiscard)
      gen.chunk.emit(1'u8)
      let bTy = gen.genExpr(node[2]) # generate the right-hand side
      if aTy != gen.module.sym"bool" or bTy != gen.module.sym"bool":
        node.error("Operands of 'and' must be booleans")
      gen.chunk.fillHole(hole, uint16(gen.chunk.code.len - hole + 1))
      result = gen.module.sym"bool"
    else: discard

proc objConstr(node: Node, ty: Sym): Sym {.codegen.} =
  ## Generate code for an object constructor.
  result = gen.lookup(node[0]) # find the object type that's being constructed
  if result.tyKind != tkObject:
    node.error(fmt"<{result.name}> is not an object type")
  if node.children.len - 1 < result.objFields.len:
    # TODO: allow the user to not initialize some fields, and set them to their
    # types' default values instead.
    node.error("All fields of an object must be initialized")
  # collect the initialized fields into a seq with their values and the fields
  # themselves
  var fields: seq[tuple[node: Node, field: ObjectField]]
  fields.setLen(result.objFields.len)
  for f in node.children[1..^1]:
    let name = f[0].ident
    if name notin result.objFields:
      node.error(fmt"<{result.name}> does not have a field '{name}'")
    let field = result.objFields[name]
    fields[field.id] = (node: f[1], field: field)
  # iterate the fields and values in reverse, and push them onto the stack.
  # we iterate in reverse because that's how stacks work.
  for i in countdown(fields.len - 1, 0):
    let
      (value, field) = fields[i]
      ty = gen.genExpr(value)
    if ty != field.ty:
      node.error(fmt"Type mismatch: field has type <{field.ty.name}>, " &
                 fmt"but got <{ty.name}>")
  # construct the object
  gen.chunk.emit(opcConstrObj)
  gen.chunk.emit(uint8(fields.len))

proc procCall(node: Node, procSym: Sym): Sym {.codegen.} =
  discard

proc call(node: Node): Sym {.codegen.} =
  ## Generates code for a nkCall (proc call or object constructor).
  let sym = gen.lookup(node[0])
  if sym.kind == skType:
    # object construction
    result = gen.objConstr(node, sym)

proc genGetField(node: Node): Sym {.codegen.} =
  ## Generate code for field access.
  if node[1].kind != nkIdent:
    node[1].error("{node[1]} is not a valid object field")
  let
    typeSym = gen.genExpr(node[0]) # generate the left-hand side
    fieldName = node[1].ident # get the field's name
  if typeSym.tyKind == tkObject and fieldName in typeSym.objFields:
    # if it's an existent field, pushF it onto the stack.
    let field = typeSym.objFields[fieldName]
    result = field.ty
    gen.chunk.emit(opcPushF)
    gen.chunk.emit(field.id.uint8)
  else:
    node.error("<{typeSym.name}> does not have a field '{fieldName}'")

proc genBlock(node: Node, isStmt: bool): Sym {.codegen.}

proc genIf(node: Node, isStmt: bool): Sym {.codegen.} =
  ## Generate code for an if expression/statement.
  var
    pos = 0 # the current position in the node
    jumpsToEnd: seq[int]
    ifTy: Sym # the type of the if expression, ``void`` for an if statement
    hadElse = false # did the if have an else branch?
  while pos < node.children.len:
    # iterate through the if statement's nodes
    if node[pos].kind != nkBlock:
      # if it's not a block, it must be an ``if`` or ``elif`` branch
      if gen.genExpr(node[pos]) != gen.module.sym"bool":
        node[pos].error("'if' condition must be a <bool>")
      inc(pos)
      # if the condition is ``false``, jump over to the next branch
      gen.chunk.emit(opcJumpFwdF)
      let afterBlock = gen.chunk.emitHole(2)
      # otherwise, discard the condition and execute the body
      gen.chunk.emit(opcDiscard)
      gen.chunk.emit(1'u8)
      let blockTy = gen.genBlock(node[pos], isStmt)
      if not isStmt:
        # if we have an if expression, check if the all branches' types are
        # the same
        if ifTy == nil:
          ifTy = blockTy
        else:
          if blockTy != ifTy:
            node[pos].error(fmt"Type mismatch: <{ifTy.name}> expected, but " &
                            fmt"got <{blockTy.name}>")
      if pos < node.children.len - 1:
        # if this isn't the last branch, add a jump to the end of the expression
        gen.chunk.emit(opcJumpFwd)
        jumpsToEnd.add(gen.chunk.emitHole(2))
      inc(pos)
      gen.chunk.fillHole(afterBlock,
                         uint16(gen.chunk.code.len - afterBlock + 1))
    else:
      # if it's a block, we're in an ``else`` branch
      # first, we discard the condition
      gen.chunk.emit(opcDiscard)
      gen.chunk.emit(1'u8)
      # and we execute the body
      let blockTy = gen.genBlock(node[pos], isStmt)
      if not isStmt:
        # we need to check the branch's type if we're compiling an if expression
        if blockTy != ifTy:
          node[pos].error(fmt"Type mismatch: <{ifTy.name}> expected, but got " &
                          fmt"<{blockTy.name}>")
      hadElse = true
      inc(pos)
  if not hadElse:
    # if we didn't have an else branch, we'll need to discard the condition from
    # the last ``if``/``elif`` branch
    gen.chunk.emit(opcDiscard)
    gen.chunk.emit(1'u8)
  # after that, we fill all the jumps to the end
  for hole in jumpsToEnd:
    gen.chunk.fillHole(hole, uint16(gen.chunk.code.len - hole + 1))
  result =
    # statements are ``void``
    if isStmt: gen.module.sym"void"
    # expressions have a type, so return the if's type.
    else: ifTy

proc genExpr(node: Node): Sym {.codegen.} =
  ## Generates code for an expression.
  case node.kind
  of nkBool, nkNumber: # constants
    result = gen.pushConst(node)
  of nkIdent: # variables
    var varSym = gen.lookup(node)
    gen.pushVar(varSym)
    result = varSym.varTy
  of nkPrefix: # prefix operators
    result = gen.prefix(node)
  of nkInfix: # infix operators
    result = gen.infix(node)
  of nkDot: # field access
    result = gen.genGetField(node)
  of nkCall: # calls and object construction
    result = gen.call(node)
  of nkIf: # if expressions
    result = gen.genIf(node, isStmt = false)
  else: node.error("Value does not have a valid type")

proc genWhile(node: Node) {.codegen.} =
  ## Generates code for a while loop.
  var
    isWhileTrue = false # an optimization for while true loops
    afterLoop: int # a hole pointer to the end of the loop
  let beforeLoop = gen.chunk.code.len
  # begin a new loop
  gen.loops.add(Loop(before: beforeLoop,
                     bottomScope: gen.scopes.len))
  if node[0].kind == nkBool: # if the condition is a bool literal
    if node[0].boolVal == true: isWhileTrue = true # while true is optimized
    else: return # while false is a noop
  if not isWhileTrue:
    # if it's not a while true loop, execute the condition
    if gen.genExpr(node[0]) != gen.module.sym"bool":
      node[0].error("'while' condition must be a <bool>")
    # if it's false, jump over the loop's body
    gen.chunk.emit(opcJumpFwdF)
    afterLoop = gen.chunk.emitHole(2)
    # otherwise, discard the condition, and execute the body
    gen.chunk.emit(opcDiscard)
    gen.chunk.emit(1'u8)
  # generate the body. we don't care about its type, because while loops are not
  # expressions
  discard gen.genBlock(node[1], isStmt = true)
  # after the body's done, jump back to reevaluate the condition
  gen.chunk.emit(opcJumpBack)
  gen.chunk.emit(uint16(gen.chunk.code.len - beforeLoop - 1))
  if not isWhileTrue:
    # if it wasn't a while true, we need to fill in the hole after the loop
    gen.chunk.fillHole(afterLoop, uint16(gen.chunk.code.len - afterLoop + 1))
  for brk in gen.loops[^1].breaks:
    # we also need to fill any breaks
    gen.chunk.fillHole(brk, uint16(gen.chunk.code.len - brk + 1))
  # we're now done with the loop.
  discard gen.loops.pop()

proc genBreak(node: Node) {.codegen.} =
  ## Generate code for a ``break`` statement.
  if gen.loops.len < 1:
    node.error("'break' can only be used in a loop")
  let loop = gen.loops[^1]
  # discard the loop's variables
  var varCount = 0
  for scope in gen.scopes[loop.bottomScope..^1]:
    inc(varCount, scope.varCount)
  gen.chunk.emit(opcDiscard)
  gen.chunk.emit(varCount.uint8)
  # jump past the loop
  gen.chunk.emit(opcJumpFwd)
  loop.breaks.add(gen.chunk.emitHole(2))

proc genContinue(node: Node) {.codegen.} =
  ## Generate code for a ``continue`` statement.
  if gen.loops.len < 1:
    node.error("'continue' can only be used in a loop")
  let loop = gen.loops[^1]
  # discard the loop's variables
  var varCount = 0
  for scope in gen.scopes[loop.bottomScope..^1]:
    inc(varCount, scope.varCount)
  gen.chunk.emit(opcDiscard)
  gen.chunk.emit(varCount.uint8)
  # jump back to the condition
  gen.chunk.emit(opcJumpBack)
  gen.chunk.emit(uint16(gen.chunk.code.len - loop.before))

proc genObject(node: Node) {.codegen.} =
  ## Process an object declaration, and add the new type into the current
  ## module or scope.

  # create a new type for the object
  var ty = newType(tkObject, node[0])
  for fields in node.children[1..^1]:
    # get the fields' type
    let fieldsTy = gen.lookup(fields.children[^1])
    # create all the fields with the given type
    for name in fields.children[0..^2]:
      ty.objFields.add(name.ident,
                       (id: ty.objFields.len, name: name, ty: fieldsTy))
  if gen.scopes.len > 0:
    # local type
    gen.currentScope.syms.add(ty.name.ident, ty)
  else:
    # global type
    gen.module.syms.add(ty.name.ident, ty)

proc genStmt(node: Node) {.codegen.} =
  ## Generate code for a statement.
  case node.kind
  of nkLet, nkVar: # variable declarations
    for decl in node.children:
      let valTy = gen.genExpr(decl[2]) # generate the value
      if decl[3].kind != nkEmpty:
        # if an explicit type was specified, check if the value's type matches
        let expectedTy = gen.lookup(decl[3])
        if valTy != expectedTy:
          decl[2].error("Value does not match the specified type")
      # declare the variable
      gen.declareVar(decl[1],
                     if node.kind == nkVar: skVar
                     else: skLet,
                     valTy)
      # and pop the value into it
      gen.popVar(decl[1])
  of nkBlock: discard gen.genBlock(node, true) # block statement
  of nkIf: discard gen.genIf(node, true) # if statement
  of nkWhile: gen.genWhile(node) # while loop
  of nkBreak: gen.genBreak(node) # break statement
  of nkContinue: gen.genContinue(node) # continue statement
  of nkObject: gen.genObject(node) # object declaration
  else: # expression statement
    let ty = gen.genExpr(node)
    if ty != gen.module.sym"void":
      # if the expression's type if non-void, discard the result.
      # TODO: discard statement for clarity
      gen.chunk.emit(opcDiscard)
      gen.chunk.emit(1'u8)

proc genBlock(node: Node, isStmt: bool): Sym {.codegen.} =
  ## Generate a block of code.

  # every block creates a new scope
  gen.pushScope()
  for i, s in node.children:
    if isStmt:
      # if it's a statement block, generate its children normally
      gen.genStmt(s)
    else:
      # otherwise, treat the last statement as an expression (and the value of
      # the block)
      if i < node.children.len - 1:
        gen.genStmt(s)
      else:
        result = gen.genExpr(s)
  # pop the block's scope
  gen.popScope()
  # if it was a statement, the block's type is void
  if isStmt: result = gen.module.sym"void"

proc genScript*(node: Node) {.codegen.} =
  ## Generates the code for a full script.
  for s in node.children:
    gen.genStmt(s)
  gen.chunk.emit(opcHalt)

