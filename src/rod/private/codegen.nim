#--
# the rod scripting language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

import hashes
import macros
import strutils
import tables

import chunk
import common
import parser
import value

{.push experimental: "codeReordering".}

#--
# Error messages
#--

const
  ErrorFmt = "$1($2, $3): $4"
  ErrProcRedefinition =
    "An overload with the given parameter types already exists"
  ErrShadowResult = "Variable shadows implicit 'result' variable"
  ErrLocalRedeclaration = "'$1' is already declared in this scope"
  ErrGlobalRedeclaration = "'$1' is already declared"
  ErrUndefinedReference = "'$1' is not declared in the current scope"
  ErrLetReassignment = "'$1' cannot be reassigned"
  ErrTypeMismatch = "Type mismatch: got <$1>, but expected <$2>"
  ErrTypeMismatchChoice = "Type mismatch: got <$1>, but expected one of:$2"
  ErrNotAProc = "'$1' is not a proc"
  ErrInvalidField = "'$1' is not a valid field"
  ErrNonExistentField = "Field '$1' does not exist"
  ErrInvalidAssignment = "Cannot assign to '$1'"
  ErrTypeIsNotAnObject = "'$1' is not an object type"
  ErrObjectFieldsMustBeInitialized = "All object fields must be initialized"
  ErrFieldInitMustBeAColonExpr =
    "Field initializer must be a colon expression: 'a: b'"
  ErrNoSuchField = "<$1> does not have the field '$2'"
  ErrValueIsVoid = "Value does not have a valid type"
  ErrOnlyUsableInABlock =
    "'$1' can only be used inside a loop or a block statement"
  ErrOnlyUsableInALoop = "'$1' can only be used inside a loop"
  ErrOnlyUsableInAProc = "'$1' can only be used inside a proc"
  ErrOnlyUsableInAnIterator = "'$1' can only be used inside an iterator"
  ErrVarMustHaveValue = "Variable must have a value"
  ErrIterMustHaveYieldType = "Iterator must have a non-void yield type"
  ErrSymKindMismatch = "$1 expected, but got $2"
  ErrIdentDefsValueNotAllowed = "Value not allowed here"
  ErrExprIsNotASym = "Expression '$1' does not name a valid symbol"
  ErrSymIsNotConcrete = "'$1' is not a concrete $2"
  ErrSymIsNotAGenericX = "'$1' is not a generic $2"
  ErrSymIsNotGeneric = "'$1' is not generic"
  ErrGenericParamAmtMismatch = "Got $1 generic type(s), but expected $2"
  ErrInGenericInst = "Error in generic instantiation:\n$1"

#--
# Compile-time info
#--

type
  Context = distinct int ## A scope context.
  Scope = ref object of RootObj ## A local scope.
    syms: Table[string, Sym]
    context: Context ## The scope's context. This is used for scope hygiene.
  Module* = ref object of Scope ## \
      ## A module representing the global scope of a single source file.
    name: string ## The name of the module.
  SymKind = enum ## The kind of a symbol.
    skVar = "var"
    skLet = "let"
    skType = "type"
    skProc = "proc"
    skIterator = "iterator"
    skChoice = "(...)" ## An overloaded symbol, stores many symbols with \
                       ## the same name.
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
      tyParents: seq[Sym] ## Parent types.
      tyParams: seq[GenericParam] ## Generic params of a type. Empty if \
                                  ## the type does not have generic params.
      tyInstCache: Table[seq[Sym], Sym] ## Cache of generic instantiations.
    of skProc:
      procId: uint16 ## The unique number of the proc.
      procParams: seq[ProcParam] ## The proc's parameters.
      procReturnTy: Sym ## The return type of the proc.
    of skIterator:
      iterParams: seq[ProcParam] ## The iterator's parameters.
      iterYieldTy: Sym ## The yield type of the iterator.
    of skChoice:
      choices: seq[Sym] ## The choices.
  ObjectField = tuple
    id: int
    name: Node
    ty: Sym
  GenericParam* = tuple ## A single generic param.
    name: Node
    constraint: Sym
  ProcParam* = tuple ## A single param of a proc.
    name: Node
    ty: Sym

const
  skVars = {skVar, skLet}
  skCallable = {skProc, skIterator}
  tkPrimitives = {tkVoid..tkString}

proc `==`(a, b: Context): bool {.borrow.}

proc `$`(params: seq[ProcParam]): string =
  ## Stringify a seq of proc parameters.
  result = "("
  for i, param in params:
    result.add($param.name & ": " & $param.ty.name)
    if i != params.len - 1:
      result.add(", ")
  result.add(")")

proc `$`(sym: Sym): string =
  ## Stringify a symbol.
  case sym.kind
  of skVar:
    result = "var of type " & sym.varTy.name.ident
  of skLet:
    result = "let of type " & sym.varTy.name.ident
  of skType:
    result = "type"
    if sym.tyParams.len != 0:
      result.add("[")
      for i, (name, constraint) in sym.tyParams:
        result.add($name)
        if constraint != nil:
          result.add(": " & $constraint.name)
        if i != sym.tyParams.len - 1:
          result.add(", ")
      result.add("]")
    result.add(" = ")
    case sym.tyKind
    of tkPrimitives: result.add($sym.tyKind)
    of tkObject:
      result.add("object {")
      for name, field in sym.objectFields:
        result.add(" " & name & ": " & $field.ty.name & ";")
      result.add(" }")
  of skProc:
    result = "proc " & $sym.name & "{" & $sym.procId & "}" & $sym.procParams
  of skIterator:
    result = "iterator " & $sym.name & $sym.iterParams
  of skChoice:
    result = "choice between " & $sym.choices.len & " {"
    for choice in sym.choices:
      result.add(" " & $choice & ",")
    result.add(" }")

proc hash(sym: Sym): Hash =
  ## Get a unique hash for a symbol.
  result = 0
  result = result !& hash(sym.name)
  if sym.impl != nil: result = result !& hash(sym.impl)
  result = result !& hash(sym.kind)
  case sym.kind
  of skVars:
    result = result !& hash(sym.varTy)
    result = result !& hash(sym.varLocal)
  of skType:
    result = result !& hash(sym.tyKind)
    if sym.tyKind == tkObject:
      result = result !& hash(sym.objectId)
    result = result !& hash(sym.tyParams)
  of skProc:
    result = result !& hash(sym.procId)
    result = result !& hash(sym.procParams)
    result = result !& hash(sym.procReturnTy)
  of skIterator:
    result = result !& hash(sym.iterParams)
    result = result !& hash(sym.iterYieldTy)
  of skChoice:
    for s in sym.choices:
      result = result !& hash(s)
  result = !$result

proc isConcrete(sym: Sym): bool =
  ## Checks if the symbol is a concrete symbol (denodes *one* specific type
  ## without any generic parameters).
  result =
    if sym.kind == skType: sym.tyParams.len == 0
    else: true

proc newSym(kind: SymKind, name: Node, impl: Node = nil): Sym =
  ## Create a new symbol from a Node.
  result = Sym(name: name, impl: impl, kind: kind)

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
  of skCallable:
    result = true
    for other in choice.choices:
      if other.kind in skCallable:
        if sym.procParams.len != other.procParams.len: continue
        result = false
        for i, (_, ty) in sym.procParams:
          if ty != other.procParams[i].ty: return true
  of skChoice: discard

proc add(scope: Scope, sym: Sym, name = ""): bool =
  ## Add a symbol to the given scope. If a symbol under the given name already
  ## exists, it's added into an skChoice. The rules for overloading are:
  ## - there may only be one skVar or skLet under a given skChoice,
  ## - there may only be one skType under a given skChoice,
  ## - there may be any number of skProcs with unique parameter lists under a
  ##   single skChoice.
  ## If any one of these checks fails, the proc will return ``false``.
  ## If ``name`` is not empty, it will be used for the lookup name of the given
  ## symbol instead of ``sym.name.ident``.
  let name =
    if name.len == 0: sym.name.ident
    else: name
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
                       msg: ErrorFmt % [node.file, $node.ln, $node.col, msg])

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
    name.error(ErrProcRedefinition)

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
  ContextAllocator = ref object ## A context allocator. It's shared between \
                                ## codegen instances.
    occupied: seq[Context]

  FlowBlockKind = enum
    fbLoopOuter  # outer loop flow block, used by ``break``
    fbLoopIter   # iteration loop flow block, used by ``continue``
  FlowBlock = ref object
    kind: FlowBlockKind
    breaks: seq[int]
    bottomScope: int
    context: Context
  GenKind = enum
    gkToplevel
    gkProc
    gkIterator
  CodeGen* = object ## A code generator for a module or proc.
    script: Script ## The script all procs go into.
    module: Module ## The global scope.
    chunk: Chunk ## The chunk of code we're generating.
    scopes: seq[Scope] ## Local scopes.
    flowBlocks: seq[FlowBlock] ## Flow control blocks.
    ctxAllocator: ContextAllocator ## The context allocator.
    context: Context ## The codegen's scope context. This is used to achieve \
                     ## scope hygiene with iterators.
    case kind: GenKind ## Does this generator handle a module or a proc?
    of gkToplevel: discard
    of gkProc:
      procReturnTy: Sym ## The proc's return type.
    of gkIterator:
      iter: Sym ## The symbol representing the iterator.
      iterForBody: Node ## The for loop's  body.
      iterForVar: Node ## The for loop variable's name.
      iterForCtx: Context ## The for loop's context.

proc allocCtx*(allocator: ContextAllocator): Context =
  while result in allocator.occupied:
    result = Context(result.int + 1)
  allocator.occupied.add(result)

proc freeCtx*(allocator: ContextAllocator, ctx: Context) =
  let index = allocator.occupied.find(ctx)
  assert index != -1, "freeCtx called on a context more than one time"
  allocator.occupied.del(index)

proc initCodeGen*(script: Script, module: Module, chunk: Chunk,
                  kind = gkToplevel,
                  ctxAllocator: ContextAllocator = nil): CodeGen =
  result = CodeGen(script: script, module: module, chunk: chunk,
                   kind: kind)
  if ctxAllocator == nil:
    result.ctxAllocator = ContextAllocator()
    result.context = result.ctxAllocator.allocCtx()

proc clone(gen: CodeGen, kind: GenKind): CodeGen =
  ## Clone a code generator, using a different kind for the new one.
  result = CodeGen(script: gen.script, module: gen.module, chunk: gen.chunk,
                   scopes: gen.scopes, flowBlocks: gen.flowBlocks,
                   ctxAllocator: gen.ctxAllocator, context: gen.context,
                   kind: kind)

template genGuard(body) =
  ## Wraps ``body`` in a "guard" used for code generation. The guard sets the
  ## line information in the target chunk. This is a helper used by {.codegen.}.
  when declared(node):
    let
      oldFile = gen.chunk.file
      oldLn = gen.chunk.ln
      oldCol = gen.chunk.col
    gen.chunk.file = node.file
    gen.chunk.ln = node.ln
    gen.chunk.col = node.col
  body
  when declared(node):
    gen.chunk.file = oldFile
    gen.chunk.ln = oldLn
    gen.chunk.col = oldCol

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

proc varCount(gen: CodeGen, bottom = 0): int =
  ## Count the number of variables in all of the codegen's scopes.
  for scope in gen.scopes[bottom..^1]:
    result += scope.varCount

proc currentScope(gen: CodeGen): Scope =
  ## Returns the current scope.
  result = gen.scopes[^1]

proc pushScope(gen: var CodeGen) =
  ## Push a new scope.
  gen.scopes.add(Scope(context: gen.context))

proc popScope(gen: var CodeGen) =
  ## Pop the top scope, discarding its variables.
  if gen.currentScope.varCount > 0:
    gen.chunk.emit(opcDiscard)
    gen.chunk.emit(gen.currentScope.varCount.uint8)
  discard gen.scopes.pop()

proc pushFlowBlock(gen: var CodeGen, kind: FlowBlockKind,
                   context = Context(-1)) =
  ## Push a new flow block. This creates a new scope for the flow block.
  let fblock = FlowBlock(kind: kind,
                         bottomScope: gen.scopes.len)
  if context == Context(-1):
    fblock.context = gen.context
  else:
    fblock.context = context
  gen.flowBlocks.add(fblock)
  gen.pushScope()

proc breakFlowBlock(gen: var CodeGen, fblock: FlowBlock) =
  ## Break a code block. This discards the flow block's scope's variables *and*
  ## generates a jump past the block.
  ## This does not remove the flow block from the stack, it only jumps past it
  ## and discards any already declared variables.
  gen.chunk.emit(opcDiscard)
  gen.chunk.emit(gen.varCount(fblock.bottomScope).uint8)
  gen.chunk.emit(opcJumpFwd)
  fblock.breaks.add(gen.chunk.emitHole(2))

proc popFlowBlock(gen: var CodeGen) =
  ## Pop the topmost flow block, popping its scope and filling in any breaks.
  gen.popScope()
  for brk in gen.flowBlocks[^1].breaks:
    gen.chunk.patchHole(brk)
  discard gen.flowBlocks.pop()

proc findFlowBlock(gen: var CodeGen, kinds: set[FlowBlockKind]): FlowBlock =
  ## Find the topmost flow block, with the given kind, and defined in the same
  ## context as ``gen``'s current context.
  ## Returns ``nil`` if a matching flow block can't be found.
  for i in countdown(gen.flowBlocks.len - 1, 0):
    let fblock = gen.flowBlocks[i]
    if fblock.context == gen.context and fblock.kind in kinds:
      return fblock

proc declareVar(gen: var CodeGen, name: Node, kind: SymKind, ty: Sym,
                isMagic = false): Sym {.discardable.} =
  ## Declare a new variable with the given ``name``, of the given ``kind``, with
  ## the given type ``ty``.
  ## If ``isMagic == true``, this will disable some error checks related to
  ## magic variables (eg. shadowing ``result``).

  # check if the variable's name is not ``result`` when in a non-void proc
  if not isMagic and gen.kind == gkProc and gen.procReturnTy.tyKind != tkVoid:
    if name.ident == "result":
      name.error(ErrShadowResult)
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
      name.error(ErrLocalRedeclaration % $name)
  else:
    # declare a global
    result.varLocal = false
    if not gen.module.add(result):
      name.error(ErrGlobalRedeclaration % $name)

proc lookup(gen: var CodeGen, expr: Node): Sym

proc createObject(node: Node, isInstantiation = false): Sym {.codegen.} =
  ## Create an object symbol from a Node.

  # create a new type for the object
  result = newType(tkObject, node[0])
  result.impl = node
  result.objectId = gen.script.typeCount
  inc(gen.script.typeCount)
  # collect its generic params
  if not isInstantiation and node[1].kind != nkEmpty:
    for defs in node[1]:
      if defs[^1].kind != nkEmpty:
        defs.error(ErrIdentDefsValueNotAllowed)
      let constraint =
        if defs[^2].kind != nkEmpty: gen.lookup(defs[^2])
        else: nil
      for name in defs[0..^3]:
        result.tyParams.add (name, constraint)
  # if the object is not generic, create a concrete object right away
  else:
    for fields in node[2]:
      # get the fields' type
      let fieldsTy = gen.lookup(fields[^2])
      # create all the fields with the given type
      for name in fields[0..^3]:
        result.objectFields.add(name.ident,
                                (id: result.objectFields.len,
                                 name: name,
                                 ty: fieldsTy))

proc instantiate(gen: var CodeGen, sym: Sym, params: seq[Sym],
                 errorExpr: Node = nil): Sym =
  ## Instantiate a generic symbol, given the generic parameters.
  ## Errors will be reported at ``errorExpr``'s position.
  case sym.kind
  of skType:
    if errorExpr != nil and sym.tyParams.len == 0:
      errorExpr.error(ErrSymIsNotAGenericX % [$errorExpr, "type"])
    # generic instantiations are cached, so A[T] == A[T].
    # also, speed. but semantics are more important.
    if params in sym.tyInstCache:
      result = sym.tyInstCache[params]
    else:
      # if our desired instantiation is not in the type's cache, we'll have to
      # create it
      if params.len != sym.tyParams.len:
        errorExpr.error(ErrGenericParamAmtMismatch %
                          [$params.len, $sym.tyParams.len])
      if sym.tyKind == tkObject:
        gen.pushScope()
        for i, ty in params:
          # TODO: inheritance, concepts, and type constraints
          let (name, constraint {.used.}) = sym.tyParams[i]
          discard gen.currentScope.add(ty, $name)
        try:
          result = gen.createObject(sym.impl, isInstantiation = true)
          sym.tyInstCache[params] = result
        except RodError as err:
          errorExpr.error(ErrInGenericInst % [err.msg])
        gen.popScope()
      else:
        # instantiating a builtin does not need much extra work.
        # builtins only need generics for extra type safety, so it's fine to
        # simply copy the existing sym.
        result = new(Sym)
        result[] = sym[]
      # now we need to add the parent sym and make the new sym concrete.
      result.tyParents.add(sym)
      result.tyParams.setLen(0)
      result.tyInstCache.clear()
    assert result.isConcrete
  else:
    errorExpr.error(ErrSymIsNotGeneric)

proc lookup(gen: var CodeGen, expr: Node): Sym =
  ## Look up the symbol for the given expression ``expr``.
  if expr.kind notin {nkIdent, nkIndex}:
    expr.error(ErrExprIsNotASym % $expr)
  let name =
    if expr.kind == nkIndex: expr[0]
    else: expr
  if gen.scopes.len > 0:
    # try to find a local symbol
    for i in countdown(gen.scopes.len - 1, 0):
      if gen.scopes[i].context == gen.context and
         name.ident in gen.scopes[i].syms:
        result = gen.scopes[i].syms[name.ident]
        break
  elif name.ident in gen.module.syms:
    # try to find a global symbol
    result = gen.module.syms[name.ident]
  if result == nil:
    expr.error(ErrUndefinedReference % $name)
  # instantiate generic symbols
  if expr.kind == nkIndex:
    var genericParams: seq[Sym]
    for param in expr[1..^1]:
      genericParams.add(gen.lookup(param))
    result = gen.instantiate(result, genericParams, errorExpr = expr)
  if not result.isConcrete:
    expr.error(ErrSymIsNotConcrete % [$expr, "type"])

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
    name.error(ErrUndefinedReference % $name)
  if sym.kind == skLet and sym.varSet:
    # if the variable is 'let' and it's already set, error out
    name.error(ErrLetReassignment % $name)
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

proc params(sym: Sym): seq[ProcParam] =
  ## Get the proc/iterator's params.
  assert sym.kind in skCallable
  case sym.kind
  of skProc: result = sym.procParams
  of skIterator: result = sym.iterParams
  else: discard

proc findOverload(sym: Sym, params: seq[Sym],
                  errorNode: Node = nil): Sym {.codegen.} =
  ## Finds the correct overload for ``sym``, given the parameter types.
  if sym.kind in skCallable:
    result = sym
    if sym.params.len != params.len: result = nil
    else:
      for i, (_, ty) in sym.params:
        if params[i] != ty:
          result = nil
          break
  elif sym.kind == skChoice:
    for choice in sym.choices:
      if choice.kind in skCallable:
        if choice.params.len != params.len: continue
        block checkParams:
          for i, (_, ty) in choice.procParams:
            if params[i] != ty: break checkParams
          result = choice
  if errorNode != nil and result == nil:
    var paramList = ""
    for i, param in params:
      paramList.add($param.name)
      if i != params.len - 1:
        paramList.add(", ")
    var overloadList = ""
    let overloads =
      if sym.kind == skChoice: sym.choices
      else: @[sym]
    for overload in overloads:
      if overload.kind in skCallable:
        overloadList.add("\n  " & $overload.kind & ' ' & $overload.name & "(")
        for i, (name, ty) in overload.params:
          overloadList.add($name & ": " & $ty.name)
          if i != overload.procParams.len - 1:
            overloadList.add(", ")
        overloadList.add(")")
        if overload.procReturnTy.tyKind != tkVoid:
          overloadList.add(" -> " & $overload.procReturnTy.name)
    errorNode.error(ErrTypeMismatchChoice % [paramList, overloadList])

proc splitCall(ast: Node): tuple[callee: Sym, args: seq[Node]] {.codegen.} =
  ## Splits any call node (prefix, infix, call, dot access, dot call) into a
  ## callee (the thing being called) and parameters. The callee is resolved to a
  ## symbol.
  var
    callee: Node
    args: seq[Node]
  assert ast.kind in {nkPrefix, nkInfix, nkCall}
  case ast.kind
  of nkPrefix:
    callee = ast[0]
    args = @[ast[1]]
  of nkInfix:
    callee = ast[0]
    args = ast[1..2]
  of nkCall:
    callee = ast[0]
    args = ast[1..^1]
  else: discard
  result = (gen.lookup(callee), args)

proc callProc(procSym: Sym, argTypes: seq[Sym],
              errorNode: Node = nil): Sym {.codegen.} =
  ## Generate code that calls a procedure. ``errorNode`` is used for error
  ## reporting.
  if procSym.kind in {skProc, skChoice}:
    # find the overload
    let theProc = gen.findOverload(procSym, argTypes, errorNode)
    if theProc.kind != skProc:
      errorNode.error(ErrSymKindMismatch % [$skProc, $theProc.kind])
    # call the proc
    gen.chunk.emit(opcCallD)
    gen.chunk.emit(theProc.procId)
    result = theProc.procReturnTy
  elif procSym.kind in skVars:
    discard # TODO: call through reference in variable
  else:
    if errorNode != nil: errorNode.error(ErrNotAProc % $procSym.name)

proc prefix(node: Node): Sym {.codegen.} =
  ## Generate instructions for a prefix operator.
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
    result = gen.callProc(procSym, argTypes = @[ty], node)

proc infix(node: Node): Sym {.codegen.} =
  ## Generate instructions for an infix operator.
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
      result = gen.callProc(procSym, argTypes = @[aTy, bTy], node)
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
          node.error(ErrTypeMismatch % [$valTy.name, $sym.varTy.name])
      of nkDot: # to an object field
        if node[1][1].kind != nkIdent:
          # object fields are always identifiers
          node[1][1].error(ErrInvalidField % $node[1][1])
        let
          typeSym = gen.genExpr(node[1][0]) # generate the receiver's code
          fieldName = node[1][1].ident
          valTy = gen.genExpr(node[2]) # generate the value's code
        if typeSym.tyKind == tkObject and fieldName in typeSym.objectFields:
          # assign the field if it's valid, using popF
          let field = typeSym.objectFields[fieldName]
          if valTy != field.ty:
            node[2].error(ErrTypeMismatch % [$field.ty.name, $valTy.name])
          gen.chunk.emit(opcPopF)
          gen.chunk.emit(field.id.uint8)
        else:
          node[1].error(ErrNonExistentField % fieldName)
      else: node.error(ErrInvalidAssignment % $node)
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
      if aTy.tyKind != tkBool: node[1].error(ErrTypeMismatch % [$aTy, "bool"])
      if bTy.tyKind != tkBool: node[2].error(ErrTypeMismatch % [$bTy, "bool"])
      gen.chunk.patchHole(hole)
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
      if aTy.tyKind != tkBool: node[1].error(ErrTypeMismatch % [$aTy, "bool"])
      if bTy.tyKind != tkBool: node[2].error(ErrTypeMismatch % [$bTy, "bool"])
      gen.chunk.patchHole(hole)
      result = gen.module.sym"bool"
    else: discard

proc objConstr(node: Node, ty: Sym): Sym {.codegen.} =
  ## Generate code for an object constructor.
  result = gen.lookup(node[0]) # find the object type that's being constructed
  if result.tyKind != tkObject:
    node.error(ErrTypeIsNotAnObject % $result.name)
  if node.len - 1 != result.objectFields.len:
    # TODO: allow the user to not initialize some fields, and set them to their
    # types' default values instead.
    node.error(ErrObjectFieldsMustBeInitialized)
  # collect the initialized fields into a seq with their values and the fields
  # themselves
  var fields: seq[tuple[node: Node, field: ObjectField]]
  fields.setLen(result.objectFields.len)
  for f in node[1..^1]:
    if f.kind != nkColon:
      f.error(ErrFieldInitMustBeAColonExpr)
    let name = f[0].ident
    if name notin result.objectFields:
      f[0].error(ErrNoSuchField % [$result.name, name])
    let field = result.objectFields[name]
    fields[field.id] = (node: f[1], field: field)
  # iterate the fields and values, and push them onto the stack.
  for (value, field) in fields:
    let ty = gen.genExpr(value)
    if ty != field.ty:
      node.error(ErrTypeMismatch % [$field.ty.name, $ty.name])
  # construct the object
  gen.chunk.emit(opcConstrObj)
  gen.chunk.emit(uint16(tyFirstObject + ty.objectId))
  gen.chunk.emit(uint8(fields.len))

proc procCall(node: Node, procSym: Sym): Sym {.codegen.} =
  ## Generate code for a procedure call.
  var argTypes: seq[Sym]
  for arg in node[1..^1]:
    argTypes.add(gen.genExpr(arg))
  result = gen.callProc(procSym, argTypes, node)

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
    node[1].error(ErrInvalidField % $node[1])
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
    node.error(ErrNoSuchField % [$typeSym.name, fieldName])

proc genBlock(node: Node, isStmt: bool): Sym {.codegen.}

proc genIf(node: Node, isStmt: bool): Sym {.codegen.} =
  ## Generate code for an if expression/statement.

  # get some properties about the statement
  let
    hasElse = node.len mod 2 == 1
    branches =
      # separate the else branch from the rest of branches and conditions
      if hasElse: node[0..^2]
      else: node.children
  # then, we compile all the branches
  var jumpsToEnd: seq[int]
  for i in countup(0, branches.len - 1, 2):
    # if there was a previous branch, discard its condition
    if i != 0:
      gen.chunk.emit(opcDiscard)
      gen.chunk.emit(1'u8)
    let
      # first, we compile the condition and check its type
      cond = branches[i]
      condTy = gen.genExpr(cond)
    if condTy.tyKind != tkBool:
      cond.error(ErrTypeMismatch % [$condTy.name, "bool"])
    # if the condition is false, jump past the branch
    gen.chunk.emit(opcJumpFwdF)
    let afterBranch = gen.chunk.emitHole(2)
    # otherwise, discard the condition's value and execute the body
    gen.chunk.emit(opcDiscard)
    gen.chunk.emit(1'u8)
    let
      branch = branches[i + 1]
      branchTy = gen.genBlock(branch, isStmt)
    # if the ``if`` is an expression, check its type
    if not isStmt:
      if result == nil: result = branchTy
      else:
        if branchTy != result:
          branch.error(ErrTypeMismatch % [$branchTy.name, $result.name])
    # after the block is done, jump to the end of the whole statement
    gen.chunk.emit(opcJumpFwd)
    jumpsToEnd.add(gen.chunk.emitHole(2))
    # we also need to fill the previously created jump after the branch
    gen.chunk.patchHole(afterBranch)
    # after the branch, there's another branch or the end of the if statement
  # discard the last branch's condition
  gen.chunk.emit(opcDiscard)
  gen.chunk.emit(1'u8)
  # if we have an else branch, we need to compile it, too
  if hasElse:
    # compile the branch
    let
      elseBranch = node[^1]
      elseTy = gen.genBlock(elseBranch, isStmt)
    # check its type
    if not isStmt and elseTy != result:
      elseBranch.error(ErrTypeMismatch % [$elseTy.name, $result.name])
  # finally, fill the gaps
  for jmp in jumpsToEnd:
    gen.chunk.patchHole(jmp)
  # if it's a statement, its type is void
  if isStmt:
    result = gen.module.sym"void"

proc collectParams(formalParams: Node): seq[ProcParam] {.codegen.} =
  ## Helper used to collect parameters from an nkFormalParams to a
  ## seq[ProcParam].
  for defs in formalParams[1..^1]:
    let ty = gen.lookup(defs[^2])
    for name in defs[0..^3]:
      result.add((name, ty))

proc genProc(node: Node) {.codegen.} =
  ## Process and compile a procedure.

  # get some basic metadata
  let
    name = node[0]
    formalParams = node[2]
    body = node[3]
    params = gen.collectParams(formalParams)
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
    procGen = initCodeGen(gen.script, gen.module, chunk, gkProc)
  chunk.file = gen.chunk.file
  procGen.procReturnTy = returnTy
  # add the proc's parameters as locals
  # TODO: closures and upvalues
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
  else: node.error(ErrValueIsVoid)

proc genWhile(node: Node) {.codegen.} =
  ## Generates code for a while loop.
  var
    isWhileTrue = false # an optimization for while true loops
    afterLoop: int # a hole pointer to the end of the loop
  let beforeLoop = gen.chunk.code.len
  # begin a new loop by pushing the outer flow control block
  gen.pushFlowBlock(fbLoopOuter)
  if node[0].kind == nkBool: # if the condition is a bool literal
    if node[0].boolVal == true: isWhileTrue = true # while true is optimized
    else: return # while false is a noop
  if not isWhileTrue:
    # if it's not a while true loop, execute the condition
    let condTy = gen.genExpr(node[0])
    if condTy.tyKind != tkBool:
      node[0].error(ErrTypeMismatch % [$condTy.name, "bool"])
    # if it's false, jump over the loop's body
    gen.chunk.emit(opcJumpFwdF)
    afterLoop = gen.chunk.emitHole(2)
    # otherwise, discard the condition, and execute the body
    gen.chunk.emit(opcDiscard)
    gen.chunk.emit(1'u8)
  # generate the body. we don't care about its type, because while loops are not
  # expressions. this also creates the `iteration` flow block used by
  # ``continue`` statements
  # XXX: creating a flow block here creates a scope without any unique
  # variables, then genBlock creates another scope. optimize this
  gen.pushFlowBlock(fbLoopIter)
  discard gen.genBlock(node[1], isStmt = true)
  gen.popFlowBlock()
  # after the body's done, jump back to reevaluate the condition
  gen.chunk.emit(opcJumpBack)
  gen.chunk.emit(uint16(gen.chunk.code.len - beforeLoop - 1))
  if not isWhileTrue:
    # if it wasn't a while true, we need to fill in the hole after the loop
    gen.chunk.patchHole(afterLoop)
    # ...and pop the condition off the stack after the loop is done
    gen.chunk.emit(opcDiscard)
    gen.chunk.emit(1'u8)
  # finish the loop by popping its outer flow block.
  gen.popFlowBlock()

proc genFor(node: Node) {.codegen.} =
  ## Generate code for a ``for`` loop.
  ##
  ## Actually, a for loop isn't really a `loop`. It can be, if the iterator
  ## actually does use a loop, but it doesn't have to.
  ## All a ``for`` loop does is it walks the body of the iterator and replaces
  ## any ``yield``s with the for loop's body.

  # this is some really fragile stuff, I wouldn't be surprised if it contains
  # like, a million bugs

  # as of now, only one loop variable is supported
  # this will be changed when tuples are introduced
  let
    loopVarName = node[0]
    (iterSym, iterParams) = gen.splitCall(node[1])
    body = node[2]
  # create a new code generator for the iterator with a separated context
  var iterGen = gen.clone(gkIterator)
  iterGen.iterForBody = body
  iterGen.iterForVar = loopVarName
  iterGen.iterForCtx = gen.context
  iterGen.context = gen.ctxAllocator.allocCtx()
  # generate the arguments passed to the iterator
  # the context is switched only *after* the loop's outer flow block is pushed,
  # so that the loop can be ``break`` properly
  iterGen.pushFlowBlock(fbLoopOuter, gen.context)
  var argTypes: seq[Sym]
  for arg in iterParams:
    argTypes.add(iterGen.genExpr(arg))
  # resolve the iterator
  let theIter = gen.findOverload(iterSym, argTypes, node[1])
  if theIter.kind != skIterator:
    node[1].error(ErrSymKindMismatch % [$skIterator, $theIter.kind])
  iterGen.iter = theIter
  # declare all the variables passed as the iterator's arguments
  for (name, ty) in theIter.iterParams:
    var arg = iterGen.declareVar(name, skLet, ty)
    arg.varSet = true
  # iterate
  discard iterGen.genBlock(theIter.impl[3], isStmt = true)
  # clean up the argument scope and free the scope context
  iterGen.popFlowBlock()
  gen.ctxAllocator.freeCtx(iterGen.context)

proc genBreak(node: Node) {.codegen.} =
  ## Generate code for a ``break`` statement.

  # break from the current loop's outer flow block
  let fblock = gen.findFlowBlock({fbLoopOuter})
  if fblock == nil:
    node.error(ErrOnlyUsableInABlock % "break")
  gen.breakFlowBlock(fblock)

proc genContinue(node: Node) {.codegen.} =
  ## Generate code for a ``continue`` statement.

  # break from the current loop's iteration flow block
  let fblock = gen.findFlowBlock({fbLoopIter})
  if fblock == nil:
    node.error(ErrOnlyUsableInALoop % "continue")
  gen.breakFlowBlock(fblock)

proc genReturn(node: Node) {.codegen.} =
  ## Generate code for a ``return`` statement.
  if gen.kind != gkProc:
    node.error(ErrOnlyUsableInAProc % "return")
  if node[0].kind == nkEmpty:
    if gen.procReturnTy.tyKind != tkVoid:
      let resultSym = gen.lookup(newIdent("result"))
      gen.chunk.emit(opcPushL)
      gen.chunk.emit(resultSym.varStackPos.uint16)
  else:
    let valTy = gen.genExpr(node[0])
    if valTy != gen.procReturnTy:
      node[0].error(ErrTypeMismatch % [$valTy.name, $gen.procReturnTy.name])
  if gen.procReturnTy.tyKind != tkVoid:
    gen.chunk.emit(opcReturnVal)
  else:
    gen.chunk.emit(opcReturnVoid)

proc genYield(node: Node) {.codegen.} =
  ## Generate code for a ``yield`` statement.

  # yield can only be used inside of an iterator, but never in a for loop's body
  # using yield in a for loop's body would trigger an infinite recursion, so we
  # prevent that
  if gen.kind != gkIterator or gen.context == gen.iterForCtx:
    node.error(ErrOnlyUsableInAnIterator % "yield")
  # generate the iterator value
  let valTy = gen.genExpr(node[0])
  if valTy != gen.iter.iterYieldTy:
    node[0].error(ErrTypeMismatch % [$valTy.name, $gen.iter.iterYieldTy.name])
  # switch context to the for loop
  let myCtx = gen.context
  gen.context = gen.iterForCtx
  # create a new iter flow block with the for loop variable
  gen.pushFlowBlock(fbLoopIter)
  var loopVar = gen.declareVar(gen.iterForVar, skLet, gen.iter.iterYieldTy)
  loopVar.varSet = true
  # run the for loop's body
  discard gen.genBlock(gen.iterForBody, isStmt = true)
  # go back to the iterator's context
  gen.popFlowBlock()
  gen.context = myCtx

proc genObject(node: Node): Sym {.codegen.} =
  ## Process an object declaration, and add the new type into the current
  ## module or scope.

  let ty = gen.createObject(node)
  if gen.scopes.len > 0:
    # local type
    if not gen.currentScope.add(ty):
      node[0].error(ErrLocalRedeclaration % [$node[0]])
  else:
    # global type
    if not gen.module.add(ty):
      node[0].error(ErrGlobalRedeclaration % [$node[0]])

proc genIterator(node: Node) {.codegen.} =
  ## Process an iterator declaration, and add it into the current module or
  ## scope.

  # create a new symbol for the iterator
  var iterSym = newSym(skIterator, name = node[0], impl = node)
  # get the metadata
  let
    formalParams = node[2]
    params = gen.collectParams(formalParams)
  # check if the yield type is present
  if formalParams[0].kind == nkEmpty:
    node.error(ErrIterMustHaveYieldType)
  let yieldTy = gen.lookup(formalParams[0])
  if yieldTy.tyKind == tkVoid:
    node.error(ErrIterMustHaveYieldType)
  # fill in the iterator
  iterSym.iterParams = params
  iterSym.iterYieldTy = yieldTy
  if gen.scopes.len > 0:
    # local iterator
    if not gen.currentScope.add(iterSym):
      node[0].error(ErrLocalRedeclaration % [$node[0]])
  else:
    # global iterator
    if not gen.module.add(iterSym):
      node[0].error(ErrGlobalRedeclaration % [$node[0]])

proc genStmt(node: Node) {.codegen.} =
  ## Generate code for a statement.
  case node.kind
  of nkLet, nkVar: # variable declarations
    for decl in node:
      if decl[^1].kind == nkEmpty:
        # TODO: if the type was specified, set the value to the default value of
        # the given type
        decl[^1].error(ErrVarMustHaveValue)
      var valTy: Sym
      for name in decl[0..^3]:
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
          decl[^1].error(ErrTypeMismatch % [$valTy.name, $expectedTy.name])
  of nkBlock: discard gen.genBlock(node, true) # block statement
  of nkIf: discard gen.genIf(node, true) # if statement
  of nkWhile: gen.genWhile(node) # while loop
  of nkFor: gen.genFor(node) # for loop
  of nkBreak: gen.genBreak(node) # break statement
  of nkContinue: gen.genContinue(node) # continue statement
  of nkReturn: gen.genReturn(node) # return statement
  of nkYield: gen.genYield(node) # yield statement
  of nkProc: gen.genProc(node) # procedure declaration
  of nkIterator: gen.genIterator(node) # iterator declaration
  of nkObject: discard gen.genObject(node) # object declaration
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
  for i, s in node:
    if isStmt:
      # if it's a statement block, generate its children normally
      gen.genStmt(s)
    else:
      # otherwise, treat the last statement as an expression (and the value of
      # the block)
      if i < node.len - 1:
        gen.genStmt(s)
      else:
        result = gen.genExpr(s)
  # pop the block's scope
  gen.popScope()
  # if it was a statement, the block's type is void
  if isStmt: result = gen.module.sym"void"

proc genScript*(node: Node) {.codegen.} =
  ## Generates the code for a full script.
  for s in node:
    gen.genStmt(s)
  gen.chunk.emit(opcHalt)

{.pop experimental: "codeReordering".}
