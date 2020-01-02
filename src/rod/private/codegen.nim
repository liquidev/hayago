#--
# the rod scripting language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

import macros
import strformat
import tables

import chunk
import common
import parser
import value

#--
# Compile-time info
#--

type
  Scope = ref object of RootObj ## A local scope.
    syms: Table[string, Sym]
  Module* = ref object of Scope ## \
      ## A module representing the global scope of a single source file.
    name: string ## The name of the module.
  SymKind = enum ## The kind of a symbol.
    skVar
    skLet
    skType
    skProc
    skChoice ## An overloaded symbol, stores many symbols of the same name.
  TypeKind = enum ## The kind of a type.
    tkVoid = "void"
    tkBool = "bool"
    tkNumber = "number"
    tkString = "string"
    tkObject = "object"
  Sym = ref object ## A symbol.
    name: Node ## The name of the symbol.
    impl: Node ## \
      ## The implementation of the symbol. May be ``nil`` if the
      ## symbol is generated.
    case kind: SymKind
    of skVar, skLet:
      varTy: Sym ## The type of the variable.
      varSet: bool ## Is the variable set?
      varLocal: bool ## Is the variable local?
      varStackPos: int ## The position of a local variable on the stack.
    of skType:
      case tyKind: TypeKind
      of tkVoid..tkString: discard
      of tkObject:
        objectId: TypeId
        objectFields: Table[string, ObjectField]
    of skProc:
      procId: uint16 ## The unique number of the proc.
      procParams: seq[ProcParam] ## The proc's parameters.
      procReturnTy: Sym ## The return type of the proc.
    of skChoice:
      choices: seq[Sym] ## The choices.
  ObjectField = tuple
    id: int
    name: Node
    ty: Sym
  ProcParam* = tuple ## A single param of a proc.
    name: Node
    ty: Sym

const
  skVars = {skVar, skLet}
  tkPrimitives = {tkVoid..tkString}

proc `$`(sym: Sym): string =
  ## Stringify a symbol.
  case sym.kind
  of skVar:
    result = "var of type " & sym.varTy.name.ident
  of skLet:
    result = "let of type " & sym.varTy.name.ident
  of skType:
    result = "type = "
    case sym.tyKind
    of tkPrimitives: result.add($sym.tyKind)
    of tkObject:
      result.add("object {")
      for name, field in sym.objectFields:
        result.add(" " & name & ": " & $field.ty.name & ";")
      result.add(" }")
  of skProc:
    result = "proc " & $sym.procId & " ("
    for i, param in sym.procParams:
      result.add(param.name.ident & ": " & param.ty.name.ident)
      if i != sym.procParams.len - 1:
        result.add(", ")
    result.add(")")
  of skChoice:
    result = "choice between " & $sym.choices.len & " {"
    for choice in sym.choices:
      result.add(" " & $choice & ";")
    result.add(" }")

proc newSym(kind: SymKind, name: Node, impl: Node = nil): Sym =
  ## Create a new symbol from a Node.
  result = Sym(name: name, impl: impl, kind: kind)

proc genSym(kind: SymKind, name: string): Sym =
  ## Generate a new symbol from a string name.
  result = Sym(name: newIdent(name), kind: kind)

proc newType(kind: TypeKind, name: Node): Sym =
  ## Create a new type symbol from a Node.
  result = Sym(name: name, kind: skType, tyKind: kind)

proc genType(kind: TypeKind, name: string): Sym =
  ## Generate a new type symbol from a string name.
  result = Sym(name: newIdent(name), kind: skType,
               tyKind: kind)

proc canAdd(choice, sym: Sym): bool =
  ## Tests if ``sym`` can be added into ``choice``. Refer to ``add``
  ## documentation below for details.
  assert choice.kind == skChoice
  assert sym.kind != skChoice, "an skChoice is not recursive"
  case sym.kind
  of skVars:
    for other in choice.choices:
      if other.kind in skVars: return false
    result = true
  of skType:
    for other in choice.choices:
      if other.kind == skType: return false
    result = true
  of skProc:
    result = true
    for other in choice.choices:
      if other.kind == skProc:
        if sym.procParams.len != other.procParams.len: continue
        result = false
        for i, (_, ty) in sym.procParams:
          if ty != other.procParams[i].ty: return true
  of skChoice: discard

proc add(scope: Scope, sym: Sym): bool =
  ## Add a symbol to the given scope. If a symbol under the given name already
  ## exists, it's added into an skChoice. The rules for overloading are:
  ## - there may only be one skVar or skLet under a given skChoice,
  ## - there may only be one skType under a given skChoice,
  ## - there may be any number of skProcs with unique parameter lists under a
  ##   single skChoice.
  ## If any one of these checks fails, the proc will return ``false``.
  let name = sym.name.ident
  if name notin scope.syms:
    scope.syms[name] = sym
    result = true
  else:
    let other = scope.syms[name]
    if other.kind != skChoice:
      var choice = newSym(skChoice, other.name)
      choice.choices.add(other)
      scope.syms[name] = choice
    if scope.syms[name].canAdd(sym):
      scope.syms[name].choices.add(sym)
      result = true
    else:
      result = false

proc `$`*(scope: Scope): string =
  ## Stringifies a scope.
  for name, sym in scope.syms:
    result.add("\n  ")
    result.add(name)
    result.add(": ")
    result.add($sym)

proc `$`*(module: Module): string =
  ## Stringifies a module.
  result = "module " & module.name & ":" & $module.Scope

proc sym(module: Module, name: string): Sym =
  ## Lookup the symbol ``name`` from a module.
  result = module.syms[name]

proc load*(module: Module, other: Module) =
  ## Import the module ``other`` into ``module``.
  for _, sym in other.syms:
    discard module.add(sym)

proc error(node: Node, msg: string) =
  ## Raise a compile error on the given node.
  raise (ref RodError)(kind: reCompile,
                       file: node.file,
                       ln: node.ln, col: node.col,
                       msg: fmt"{node.file}({node.ln}, {node.col}): {msg}")

proc addProc*(script: Script, module: Module, name, impl: Node,
              params: openarray[ProcParam], returnTy: Sym,
              kind: ProcKind, addToScript = true): Proc =
  ## Add a procedure to the given module, belonging to the given script.
  let
    id = script.procs.len.uint16
    strName =
      if name.kind == nkEmpty: ":anonymous"
      else: name.ident
  result = Proc(name: strName, kind: kind,
                paramCount: params.len,
                hasResult: returnTy.tyKind != tkVoid)
  if addToScript: script.procs.add(result)
  let sym = newSym(skProc, name, impl)
  sym.procId = id
  sym.procParams = @params
  sym.procReturnTy = returnTy
  if not module.add(sym):
    name.error("An overload with the given parameter types already exists")

proc addProc*(script: Script, module: Module, name: string,
              params: openarray[(string, string)], returnTy: string,
              impl: ForeignProc = nil): Proc {.discardable.} =
  ## Add a foreign procedure to the given module, belonging to the given script.
  var nodeParams: seq[ProcParam]
  for param in params:
    nodeParams.add((newIdent(param[0]), module.sym(param[1])))
  result = script.addProc(module, newIdent(name), impl = nil,
                          nodeParams, module.sym(returnTy), pkForeign,
                          addToScript = impl != nil)
  result.foreign = impl

proc newModule*(name: string): Module =
  ## Initialize a new module.
  result = Module(name: name)

proc initSystemTypes*(module: Module) =
  ## Add primitive types into the module.
  ## This should only ever be called when creating the ``system`` module.
  for kind in tkPrimitives:
    let name = $kind
    discard module.add(genType(kind, name))

proc initSystemOps*(script: Script, module: Module) =
  ## Add builtin operations into the module.
  ## This should only ever be called when creating the ``system`` module.

  # unary operators
  script.addProc(module, "not", {"x": "bool"}, "bool")
  script.addProc(module, "+", {"x": "number"}, "number")
  script.addProc(module, "-", {"x": "number"}, "number")

  # binary operators
  script.addProc(module, "+", {"a": "number", "b": "number"}, "number")
  script.addProc(module, "-", {"a": "number", "b": "number"}, "number")
  script.addProc(module, "*", {"a": "number", "b": "number"}, "number")
  script.addProc(module, "/", {"a": "number", "b": "number"}, "number")
  script.addProc(module, "==", {"a": "number", "b": "number"}, "bool")
  script.addProc(module, "!=", {"a": "number", "b": "number"}, "bool")
  script.addProc(module, "<", {"a": "number", "b": "number"}, "bool")
  script.addProc(module, "<=", {"a": "number", "b": "number"}, "bool")
  script.addProc(module, ">", {"a": "number", "b": "number"}, "bool")
  script.addProc(module, ">=", {"a": "number", "b": "number"}, "bool")
  script.addProc(module, "==", {"a": "bool", "b": "bool"}, "bool")
  script.addProc(module, "!=", {"a": "bool", "b": "bool"}, "bool")

#--
# Code generation
#--

type
  Loop = ref object ## Loop metadata.
    before: int ## Offset before the loop. Used by ``continue``.
    breaks: seq[int] ## ``break`` holes.
    bottomScope: int ## The bottom scope of the loop, used to discard variables
                     ## upon ``break`` or ``continue``.
  CodeGen* = object ## A code generator for a module or proc.
    script: Script ## The script all procs go into.
    module: Module ## The global scope.
    chunk: Chunk ## The chunk of code we're generating.
    scopes: seq[Scope] ## Local scopes.
    loops: seq[Loop]
    case isToplevel: bool ## Does this generator handle a module or a proc?
    of false: # handles a proc
      returns: seq[int] ## ``return`` holes.
      returnTy: Sym ## The proc's return type.
    else: # handles a module
      discard

proc initCodeGen*(script: Script, module: Module, chunk: Chunk,
                  isToplevel = true): CodeGen =
  result = CodeGen(script: script, module: module, chunk: chunk,
                   isToplevel: isToplevel)

template genGuard(body) =
  ## Wraps ``body`` in a "guard" used for code generation. The guard sets the
  ## line information in the target chunk. This is a helper used by {.codegen.}.
  when declared(node):
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

proc varCount(gen: CodeGen): int =
  ## Count the number of variables in all of the codegen's scopes.
  for scope in gen.scopes:
    result += scope.varCount

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

proc declareVar(gen: var CodeGen, name: Node, kind: SymKind, ty: Sym,
                isMagic = false): Sym {.discardable.} =
  ## Declare a new variable with the given ``name``, of the given ``kind``, with
  ## the given type ``ty``.
  ## If ``isMagic == true``, this will disable some error checks related to
  ## magic variables (eg. shadowing ``result``).

  # check if the variable's name is not ``result`` when in a non-void proc
  if not isMagic and not gen.isToplevel and gen.returnTy.tyKind != tkVoid:
    if name.ident == "result":
      name.error("Variable shadows proc's implicit 'result' variable")
  # create the symbol for the variable
  assert kind in skVars, "The kind must be a variable"
  result = newSym(kind, name)
  result.varTy = ty
  result.varSet = false
  if gen.scopes.len > 0:
    # declare a local
    result.varLocal = true
    result.varStackPos = gen.varCount
    if not gen.currentScope.add(result):
      name.error(fmt"'{name}' is already declared in this scope")
  else:
    # declare a global
    result.varLocal = false
    if not gen.module.add(result):
      name.error(fmt"'{name}' is already declared")

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
  name.error(fmt"'{name}' is not declared in the current scope")

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
    name.error(fmt"Variable '{name}' doesn't exist")
  if sym.kind == skLet and sym.varSet:
    # if the variable is 'let' and it's already set, error out
    name.error(fmt"'{name}' cannot be reassigned")
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
  assert sym.kind in skVars, "The symbol must represent a variable"
  if sym.varLocal:
    # if the variable is a local, use pushL
    gen.chunk.emit(opcPushL)
    gen.chunk.emit(sym.varStackPos.uint8)
  else:
    # if it's a global, use pushG
    gen.chunk.emit(opcPushG)
    gen.chunk.emit(gen.chunk.getString(sym.name.ident))

proc pushDefault(gen: var CodeGen, ty: Sym) =
  ## Push the default value for the type ``ty`` onto the stack.
  assert ty.kind == skType, "Only types have default values"
  case ty.tyKind
  of tkVoid: assert false, "void is not a value"
  of tkBool:
    gen.chunk.emit(opcPushFalse)
  of tkNumber:
    gen.chunk.emit(opcPushN)
    gen.chunk.emit(0.0)
  of tkString:
    gen.chunk.emit(opcPushS)
    gen.chunk.emit(gen.chunk.getString(""))
  of tkObject:
    gen.chunk.emit(opcPushNil)
    gen.chunk.emit(uint16(tyFirstObject + ty.objectId))

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
    gen.chunk.emit(node.numberVal)
    result = gen.module.sym"number"
  of nkString:
    # strings - use pushS with a string ID
    gen.chunk.emit(opcPushS)
    gen.chunk.emit(gen.chunk.getString(node.stringVal))
    result = gen.module.sym"string"
  else: discard

proc findProcOverload(procSym: Sym, params: seq[Sym],
                      nameNode: Node = nil): Sym {.codegen.} =
  ## Finds the correct proc overload for ``procSym``, given the parameter types.
  if procSym.kind == skProc:
    result = procSym
    for i, (_, ty) in procSym.procParams:
      if params[i] != ty:
        result = nil
        break
  elif procSym.kind == skChoice:
    for choice in procSym.choices:
      if choice.kind == skProc:
        if choice.procParams.len != params.len: continue
        block checkParams:
          for i, (_, ty) in choice.procParams:
            if params[i] != ty: break checkParams
          result = choice
  if nameNode != nil and result == nil:
    var errorMsg = "Type mismatch, got: <"
    for i, param in params:
      errorMsg.add($param.name)
      if i != params.len - 1:
        errorMsg.add(", ")
    errorMsg.add(">, but expected one of:")
    let overloads =
      if procSym.kind == skChoice: procSym.choices
      else: @[procSym]
    for overload in overloads:
      errorMsg.add("\n  proc " & $overload.name & "(")
      for i, (name, ty) in overload.procParams:
        errorMsg.add($name & ": " & $ty.name)
        if i != overload.procParams.len - 1:
          errorMsg.add(", ")
      errorMsg.add(")")
      if overload.procReturnTy.tyKind != tkVoid:
        errorMsg.add(" -> " & $overload.procReturnTy.name)
    nameNode.error(errorMsg)

proc callProc(procSym: Sym, argTypes: seq[Sym],
              nameNode: Node = nil): Sym {.codegen.} =
  ## Generate code that calls a procedure. ``nameNode`` is used for error
  ## reporting.
  if procSym.kind in {skProc, skChoice}:
    # find the overload
    let theProc = gen.findProcOverload(procSym, argTypes, nameNode)
    # call the proc
    gen.chunk.emit(opcCallD)
    gen.chunk.emit(theProc.procId)
    result = theProc.procReturnTy
  elif procSym.kind in skVars:
    discard # call through reference in variable
  else:
    if nameNode != nil: nameNode.error(fmt"'{procSym.name}' is not a proc")

proc prefix(node: Node): Sym {.codegen.} =
  ## Generate instructions for a prefix operator.
  ## TODO: operator overloading.
  var noBuiltin = false # is no builtin operator available?
  let ty = gen.genExpr(node[1]) # generate the operand's code
  if ty == gen.module.sym"number":
    # number operators
    case node[0].ident
    of "+": discard # + is a noop
    of "-": gen.chunk.emit(opcNegN)
    else: noBuiltin = true # non-builtin operator
    result = ty
  elif ty == gen.module.sym"bool":
    # bool operators
    case node[0].ident
    of "not": gen.chunk.emit(opcInvB)
    else: noBuiltin = true # non-builtin operator
    result = ty
  else: noBuiltin = true
  if noBuiltin:
    let procSym = gen.lookup(node[0])
    result = gen.callProc(procSym, argTypes = @[ty], node[0])

proc infix(node: Node): Sym {.codegen.} =
  ## Generate instructions for an infix operator.
  ## TODO: operator overloading.
  if node[0].ident notin ["=", "or", "and"]:
    # primitive operators
    var noBuiltin = false # is there no built-in operator available?
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
      of "<=": gen.chunk.emit(opcGreaterN); gen.chunk.emit(opcInvB)
      of ">": gen.chunk.emit(opcGreaterN)
      of ">=": gen.chunk.emit(opcLessN); gen.chunk.emit(opcInvB)
      else: noBuiltin = true # unknown operator
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
      else: noBuiltin = true
      # bool operators return bools (duh.)
      result = gen.module.sym"bool"
    else: noBuiltin = true # no optimized operators for given type
    if noBuiltin:
      let procSym = gen.lookup(node[0])
      result = gen.callProc(procSym, argTypes = @[aTy, bTy], node[0])
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
        if typeSym.tyKind == tkObject and fieldName in typeSym.objectFields:
          # assign the field if it's valid, using popF
          let field = typeSym.objectFields[fieldName]
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
  if node.children.len - 1 != result.objectFields.len:
    # TODO: allow the user to not initialize some fields, and set them to their
    # types' default values instead.
    node.error("All fields of an object must be initialized")
  # collect the initialized fields into a seq with their values and the fields
  # themselves
  var fields: seq[tuple[node: Node, field: ObjectField]]
  fields.setLen(result.objectFields.len)
  for f in node.children[1..^1]:
    if f.kind != nkColon:
      f.error("Field initializer must be a colon expression 'a: b'")
    let name = f[0].ident
    if name notin result.objectFields:
      f[0].error(fmt"<{result.name}> does not have a field '{name}'")
    let field = result.objectFields[name]
    fields[field.id] = (node: f[1], field: field)
  # iterate the fields and values, and push them onto the stack.
  for (value, field) in fields:
    let ty = gen.genExpr(value)
    if ty != field.ty:
      node.error(fmt"Type mismatch: field has type <{field.ty.name}>, " &
                 fmt"but got <{ty.name}>")
  # construct the object
  gen.chunk.emit(opcConstrObj)
  gen.chunk.emit(uint16(tyFirstObject + ty.objectId))
  gen.chunk.emit(uint8(fields.len))

proc procCall(node: Node, procSym: Sym): Sym {.codegen.} =
  ## Generate code for a procedure call.
  var argTypes: seq[Sym]
  for arg in node.children[1..^1]:
    argTypes.add(gen.genExpr(arg))
  result = gen.callProc(procSym, argTypes, node[0])

proc call(node: Node): Sym {.codegen.} =
  ## Generates code for a nkCall (proc call or object constructor).
  let sym = gen.lookup(node[0])
  if sym.kind == skType:
    # object construction
    result = gen.objConstr(node, sym)
  else:
    # procedure call
    result = gen.procCall(node, sym)

proc genGetField(node: Node): Sym {.codegen.} =
  ## Generate code for field access.
  if node[1].kind != nkIdent:
    node[1].error("{node[1]} is not a valid object field")
  let
    typeSym = gen.genExpr(node[0]) # generate the left-hand side
    fieldName = node[1].ident # get the field's name
  if typeSym.tyKind == tkObject and fieldName in typeSym.objectFields:
    # if it's an existent field, pushF it onto the stack.
    let field = typeSym.objectFields[fieldName]
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

proc genProc(node: Node) {.codegen.} =
  ## Process and compile a procedure.

  # get some basic metadata
  let
    name = node[0]
    formalParams = node[2]
    body = node[3]
  # collect the parameters into a seq[ProcParam]
  var params: seq[ProcParam]
  for defs in formalParams.children[1..^1]:
    if defs[^1].kind != nkEmpty:
      defs[^1].error("Default proc parameters are not yet supported") # TODO
    let ty = gen.lookup(defs[^2])
    for name in defs.children[0..^3]:
      params.add((name, ty))
  # get the return type
  # empty return type == void
  let
    returnTy =
      if formalParams[0].kind != nkEmpty: gen.lookup(formalParams[0])
      else: gen.module.sym"void"
  # add a new proc to the module and script, create a chunk for it, and generate
  # its code
  var
    theProc = gen.script.addProc(gen.module, name, impl = node,
                                 params, returnTy, kind = pkNative)
    chunk = newChunk()
    procGen = initCodeGen(gen.script, gen.module, chunk, isToplevel = false)
  procGen.returnTy = returnTy
  # add the proc's parameters as locals
  # TODO: upvalues
  procGen.pushScope()
  for param in params:
    let param = procGen.declareVar(param.name, skLet, param.ty)
    param.varSet = true # do not let arguments be overwritten
  # declare ``result`` if applicable
  if returnTy.tyKind != tkVoid:
    procGen.declareVar(newIdent("result"), skVar, returnTy, isMagic = true)
    procGen.pushDefault(returnTy)
    procGen.popVar(newIdent("result"))
  # compile the proc's body
  discard procGen.genBlock(body, isStmt = true)
  # finally, return ``result`` if applicable
  if returnTy.tyKind != tkVoid:
    let resultSym = procGen.lookup(newIdent("result"))
    procGen.chunk.emit(opcPushL)
    procGen.chunk.emit(resultSym.varStackPos.uint8)
    procGen.chunk.emit(opcReturnVal)
  else:
    procGen.chunk.emit(opcReturnVoid)
  # we're done with generating the chunk
  theProc.chunk = chunk

proc genExpr(node: Node): Sym {.codegen.} =
  ## Generates code for an expression.
  case node.kind
  of nkBool, nkNumber, nkString: # constants
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

proc genReturn(node: Node) {.codegen.} =
  ## Generate code for a ``return`` statement.
  if gen.isToplevel:
    node.error("'return' can only be used inside a proc")
  if node[0].kind == nkEmpty:
    let resultSym = gen.lookup(newIdent("result"))
    gen.chunk.emit(opcPushL)
    gen.chunk.emit(resultSym.varStackPos.uint16)
  else:
    let valTy = gen.genExpr(node[0])
    if valTy != gen.returnTy:
      node.error(fmt"Type mismatch: got <{valTy}>, " &
                 fmt"but proc returns <{gen.returnTy}>")
  if gen.returnTy.tyKind != tkVoid:
    gen.chunk.emit(opcReturnVal)
  else:
    gen.chunk.emit(opcReturnVoid)

proc genObject(node: Node) {.codegen.} =
  ## Process an object declaration, and add the new type into the current
  ## module or scope.

  # create a new type for the object
  var ty = newType(tkObject, node[0])
  ty.impl = node
  ty.objectId = gen.script.typeCount
  inc(gen.script.typeCount)
  for fields in node.children[1..^1]:
    # get the fields' type
    let fieldsTy = gen.lookup(fields[^2])
    # create all the fields with the given type
    for name in fields.children[0..^3]:
      ty.objectFields.add(name.ident,
                          (id: ty.objectFields.len, name: name, ty: fieldsTy))
  if gen.scopes.len > 0:
    # local type
    if not gen.currentScope.add(ty):
      node[0].error(fmt"'{node[0]}' is already declared in this scope")
  else:
    # global type
    if not gen.module.add(ty):
      node[0].error(fmt"'{node[0]}' is already declared")

proc genStmt(node: Node) {.codegen.} =
  ## Generate code for a statement.
  case node.kind
  of nkLet, nkVar: # variable declarations
    for decl in node.children:
      if decl[^1].kind == nkEmpty:
        # TODO: if the type was specified, set the value to the default value of
        # the given type
        decl[^1].error("Variable must have a value")
      var valTy: Sym
      for name in decl.children[0..^3]:
        # generate the value
        valTy = gen.genExpr(decl[^1])
        # declare the variable
        gen.declareVar(name,
                       if node.kind == nkVar: skVar
                       else: skLet,
                       valTy)
        # and pop the value into it
        gen.popVar(name)
      if decl[^2].kind != nkEmpty:
        # if an explicit type was specified, check if the value's type matches
        let expectedTy = gen.lookup(decl[^2])
        if valTy != expectedTy:
          decl[^1].error("Value does not match the specified type")
  of nkBlock: discard gen.genBlock(node, true) # block statement
  of nkIf: discard gen.genIf(node, true) # if statement
  of nkWhile: gen.genWhile(node) # while loop
  of nkBreak: gen.genBreak(node) # break statement
  of nkContinue: gen.genContinue(node) # continue statement
  of nkReturn: gen.genReturn(node) # return statement
  of nkProc: gen.genProc(node) # procedure declaration
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

