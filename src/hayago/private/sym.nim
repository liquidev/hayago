#--
# the hayago scripting language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

import std/hashes
import std/options
import std/sequtils
import std/strutils
import std/tables

import ast
import parser
import value

type
  Context* = distinct uint16
    ## A scope context.

  Scope* = ref object of RootObj
    ## A local scope.
    syms*: Table[string, Sym]
    context*: Context  ## the scope's context. this is used for scope hygiene
  Module* = ref object of Scope
    ## A module representing the global scope of a single source file.
    name*: string  ## the name of the module

  SymKind* = enum
    ## The kind of a symbol.
    skVar = "var"
    skLet = "let"
    skType = "type"
    skProc = "proc"
    skIterator = "iterator"
    skGenericParam = "generic param"
    skChoice = "(...)"  ## an overloaded symbol, stores many symbols with \
                        ## the same name

  TypeKind* = enum
    ## The kind of a type.
    # meta-types
    tkVoid = "void"      # matches no types
    tkAny = "any"        # matches any and all types

    # concrete types
    tkBool = "bool"
    tkInt = "int"        # int64
    tkFloat = "float"    # float64
    tkString = "string"  # ref string

    # user-defined types
    tkObject = "object"

  Sym* = ref object
    ## A symbol. This represents an ident that can be looked up.
    name*: Node  ## the name of the symbol
    impl*: Node  ## the implementation of the symbol. may be ``nil`` if the \
                ## symbol is generated
    case kind*: SymKind
    of skVar, skLet:
      varTy*: Sym        ## the type of the variable
      varSet*: bool      ## is the variable set?
      varLocal*: bool    ## is the variable local?
      varStackPos*: int  ## the position of this local variable on the stack
    of skType:
      case tyKind*: TypeKind
      of tkVoid..tkString: discard
      of tkObject:
        objectId*: TypeId
        objectFields*: OrderedTable[string, ObjectField]
    of skProc:
      procId*: uint16              ## the unique number of the proc
      procParams*: seq[ProcParam]  ## the proc's parameters
      procReturnTy*: Sym           ## the return type of the proc
    of skIterator:
      iterParams*: seq[ProcParam]  ## the iterator's parameters
      iterYieldTy*: Sym            ## the yield type of the iterator
    of skGenericParam:
      constraint*: Sym  ## the generic type constraint
    of skChoice:
      choices*: seq[Sym]
    genericParams*: Option[seq[Sym]]    # some if the sym is generic
    genericInstCache*: Table[seq[Sym], Sym]
    genericBase*: Option[Sym]           # contains the base generic type if the
                                        # sym is an instantiation
    genericInstArgs*: Option[seq[Sym]]  # some if the sym is an instantiation

  ObjectField* = tuple
    id: int     # every object field has an id that's used for lookups on
                # runtime. this id is simply a seq index so field lookups are
                # fast
    name: Node  # the name of the field
    ty: Sym     # the type of the field

  ProcParam* = tuple
    ## A single param of a proc.
    name: Node
    ty: Sym
    # TODO: default param values
  HayaCompileError* = object of ValueError
    file*: string
    ln*, col*: int

const
  skVars* = {skVar, skLet}
  skCallable* = {skProc, skIterator}
  skDecl* = {skType} + skCallable + skVars
  skTyped* = {skType, skGenericParam}

  tkPrimitives* = {tkVoid..tkString}
  tkMeta* = {tkVoid, tkAny}

proc `==`*(a, b: Context): bool {.borrow.}

proc clone*(sym: Sym): Sym =
  ## Clones a symbol, returning a newly allocated instance with the same fields.
  new(result)
  result[] = sym[]

proc params*(sym: Sym): seq[ProcParam] =
  ## Get the proc/iterator's params.
  assert sym.kind in skCallable
  case sym.kind
  of skProc: result = sym.procParams
  of skIterator: result = sym.iterParams
  else: discard

proc returnTy*(sym: Sym): Sym =
  ## Get the proc/iterator's return or yield type.
  assert sym.kind in skCallable
  case sym.kind
  of skProc: result = sym.procReturnTy
  of skIterator: result = sym.iterYieldTy
  else: discard

proc `$`*(sym: Sym): string

proc `$`*(params: seq[ProcParam]): string =
  ## Stringify a seq of proc parameters.
  result = "("
  for i, param in params:
    result.add($param.name & ": " & $param.ty)
    if i != params.len - 1:
      result.add(", ")
  result.add(")")

proc `$`*(sym: Sym): string =
  ## Stringify a symbol in a user-friendly way.
  case sym.kind
  of skVar, skLet:
    # we don't have a runtime value for the variable, so we just show the name
    # and the type
    result =
      if sym.kind == skVar: "var "
      else: "let "
    result.add(sym.name.render)
    result.add(": ")
    result.add($sym.varTy)
  of skType:
    result = sym.name.render
    if sym.genericInstArgs.isSome:
      result.add('[' & sym.genericInstArgs.get.join(", ") & ']')
  of skGenericParam:
    result = sym.name.render
    if sym.constraint != nil:
      result.add(": " & $sym.constraint)
  of skCallable:
    result =
      if sym.kind == skProc: "proc "
      else: "iterator "
    result.add(sym.name.render)
    if sym.genericParams.isSome or sym.genericInstArgs.isSome:
      let genericParams =
        sym.genericParams.get(otherwise = sym.genericInstArgs.get)
      result.add('[' & genericParams.join(", ") & ']')
    result.add($sym.params)
    if sym.returnTy.tyKind != tkVoid:
      result.add(" -> ")
      result.add($sym.returnTy)
  of skChoice:
    result = sym.choices.join("\n").indent(2)

proc `$$`*(sym: Sym): string =
  ## Stringify a symbol verbosely for debugging purposes.
  case sym.kind
  of skVar:
    result = "var of type " & sym.varTy.name.ident
  of skLet:
    result = "let of type " & sym.varTy.name.ident
  of skType:
    result = "type"
    if sym.genericParams.isSome:
      result.add('[')
      result.add(sym.genericParams.get.join(", "))
      result.add(']')
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
  of skGenericParam:
    result = $sym.name
    if sym.constraint != nil:
      result.add(": " & $sym.constraint)
  of skChoice:
    result = "choice between " & $sym.choices.len & " {"
    for choice in sym.choices:
      result.add(" " & $choice & ",")
    result.add(" }")

proc isGeneric*(sym: Sym): bool =
  ## Returns whether the symbol is generic or not.
  ## A symbol is generic if it has generic params or *is* a generic param.
  (sym.genericParams.isSome or sym.kind == skGenericParam) and
  sym.genericBase.isNone

proc isInstantiation*(sym: Sym): bool =
  ## Returns whether the symbol is an instantiation.
  result = sym.genericBase.isSome

proc hash*(sym: Sym): Hash =
  ## Hashes a sym (for use in Tables).

  # we don't do any special hashing, just hash it by instance to make sure that
  # even if there are two symbols named the same, they'll stay different when
  # used in table lookups

  # side note: this efficient hashing of integers that Nim claims to provide
  # is just converting the int to a Hash (which is a distinct int) :)
  result = hash(cast[int](sym))

proc newSym*(kind: SymKind, name: Node, impl: Node = nil): Sym =
  ## Create a new symbol from a Node.
  result = Sym(name: name, impl: impl, kind: kind)

proc newType*(kind: TypeKind, name: Node, impl: Node = nil): Sym =
  ## Create a new type symbol from a Node.
  result = Sym(name: name, impl: impl, kind: skType, tyKind: kind)

proc genType*(kind: TypeKind, name: string): Sym =
  ## Generate a new type symbol from a string name.
  result = Sym(name: newIdent(name), kind: skType,
               tyKind: kind)

proc sameType*(a, b: Sym): bool =
  ## Returns ``true`` if ``a`` and ``b`` are are compatible types.
  assert a.kind in skTyped and b.kind in skTyped,
    "type comparison can't be done on non-type symbols"

  # make them mutable for the next segment
  var (a, b) = (a, b)

  # handle generic params as a special case, because they're a different kind of
  # a symbol
  if a.kind == skGenericParam: a = a.constraint
  if b.kind == skGenericParam: b = b.constraint

  # ``any`` is a special case: it matches literally any type
  if a.tyKind == tkAny or b.tyKind == tkAny:
    return true

  # as for other types: if they're not generic, we simply check if the symbols
  # are equal
  if not a.isInstantiation and not b.isInstantiation:
    result = a == b

  # otherwise, we check the base type, and the generic params for equivalence
  else:
    # an generic type referenced somewhere in code cannot possibly pass
    # ``isGeneric``, because all symbols are instantiated on lookup
    assert not a.isGeneric and not b.isGeneric,
      "type somehow is generic even though it was instantiated"

    # both types have to be instantiations to be equivalent, but this limitation
    # may be lifted at some point
    if not a.isInstantiation and b.isInstantiation or
       a.isInstantiation and not b.isInstantiation:
      return false

    # likewise, both types have to have the same amount of generic arguments
    if a.genericInstArgs.get.len != b.genericInstArgs.get.len:
      return false

    # in the end, we check if the base types are equivalent and the parameters
    # are equivalent
    result = a.genericBase.get.sameType(b.genericBase.get)
    for i, arg in a.genericInstArgs.get:
      result = result and arg.sameType(b.genericInstArgs.get[i])
      if result == false: break

proc sameParams*(sym: Sym, params: seq[Sym]): bool =
  ## Returns ``true`` if both ``a`` and ``b`` are called with the same
  ## parameters.
  assert sym.kind in skCallable, "symbol must be callable"
  result = sym.params.len == params.len
  if result == false: return
  for i, param in sym.params:
    if not param.ty.sameType(params[i]):
      return false

proc sameParams*(a, b: Sym): bool =
  ## Overload of ``sameParams`` for two symbols.
  assert b.kind in skCallable, "symbol must be callable"
  result = a.sameParams(b.params.mapIt(it.ty))

proc canAdd*(choice, sym: Sym): bool =
  ## Tests if ``sym`` can be added into ``choice``. Refer to ``add``
  ## documentation below for details.
  assert choice.kind == skChoice
  if sym.kind notin skDecl: return false
  case sym.kind
  of skVars:
    result = choice.choices.allIt(it.kind notin skVars)
  of skType:
    result = choice.choices.allIt(it.kind != skType)
  of skCallable:
    result = choice.choices.allIt(not sym.sameParams(it))
  of skGenericParam, skChoice: discard

proc add*(scope: Scope, sym: Sym, lookupName: Node = nil): bool =
  ## Add a symbol to the given scope. If a symbol under the given name already
  ## exists, it's added into an skChoice. The rules for overloading are:
  ## - there may only be one skVar or skLet under a given skChoice,
  ## - there may only be one skType under a given skChoice,
  ## - there may be any number of skProcs with unique parameters under a
  ##   single skChoice.
  ## If any one of these checks fails, the proc will return ``false``.
  ## These checks will probably made more strict in the future.

  # this proc can override the name of the ident. this is used for generic
  # instantiations to make type aliases under the names of the generic
  # parameters
  let name =
    if lookupName == nil: sym.name.ident
    else: lookupName.ident

  # if the symbol hasn't been overloaded, just add it to the symbol table
  if name notin scope.syms:
    scope.syms[name] = sym
    result = true

  # otherwise, add it to a shared 'choice' symbol if an overload with the same
  # name doesn't already exist
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
  ## This is only really useful for debugging.
  for name, sym in scope.syms:
    result.add("\n  ")
    result.add(name)
    result.add(": ")
    result.add($$sym)

proc `$`*(module: Module): string =
  ## Stringifies a module.
  ## This is only really useful for debugging.
  result = "module " & module.name & ":" & $module.Scope

proc sym*(module: Module, name: string): Sym =
  ## Get the symbol ``name`` from a module.
  result = module.syms[name]

proc load*(module: Module, other: Module) =
  ## Import the module ``other`` into ``module``.
  for _, sym in other.syms:
    discard module.add(sym)

proc newModule*(name: string): Module =
  ## Initialize a new module.
  result = Module(name: name)

