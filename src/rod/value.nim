#~~
# the rod programming language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#~~

import sets
import tables

import scanner
import variant

type
  RodFnSignature* = tuple
    name: string
    arity: int
  RodBaseFn* = ref object of RootObj
    sig*: RodFnSignature

  RodClass* = ref object
    name*: string
    methods*: TableRef[string, RodBaseFn]
    dynamic*: bool
    fields*: HashSet[string]
  RodObj* = ref object
    class*: RodClass
    fields*: TableRef[string, RodValue]
    userdata*: Variant

  RodValueKind* = enum
    rvkNull
    rvkBool
    rvkNum
    rvkStr
    rvkClass
    rvkObj
    rvkFn
  RodValue* = object
    case kind*: RodValueKind
    of rvkNull: discard
    of rvkBool:  boolVal*: bool
    of rvkNum:   numVal*: float
    of rvkStr:   strVal*: string
    of rvkClass: classVal*: RodClass
    of rvkObj:   objVal*: RodObj
    of rvkFn:    fnVal*: RodBaseFn

  RuntimeError* = object of Exception
    at*: TextPos
  TypeError* = object of RuntimeError

#~~
# Values and their attributes
#~~

proc `==`*(a, b: RodValue): bool =
  if a.kind == b.kind:
    case a.kind
    of rvkNull:  return true
    of rvkBool:  return a.boolVal == b.boolVal
    of rvkNum:   return a.numVal == b.numVal
    of rvkStr:   return a.strVal == b.strVal
    of rvkClass: return a.classVal == b.classVal
    of rvkObj:   return a.objVal == b.objVal
    of rvkFn:    return a.fnVal == b.fnVal

proc `$`*(val: RodValue): string =
  result =
    case val.kind
    of rvkNull: "null"
    of rvkBool: $val.boolVal
    of rvkNum: $val.numVal
    of rvkStr: val.strVal
    of rvkClass: "<class " & val.classVal.name & ">"
    of rvkObj: "<object of " & val.objVal.class.name & ">"
    of rvkFn: "<fn " & val.fnVal.sig.name & "(" & $val.fnVal.sig.arity & ")>"

proc `$+`*(val: RodValue): string =
  case val.kind
  of rvkStr: result.addQuoted(val.strVal)
  else: result = $val

proc className*(val: RodValue): string =
  result =
    case val.kind
    of rvkNull:  "Null"
    of rvkBool:  "Bool"
    of rvkNum:   "Num"
    of rvkStr:   "Str"
    of rvkFn:    "Fn"
    of rvkClass: "Class"
    of rvkObj:   val.objVal.class.name

let RodNull* = RodValue(kind: rvkNull)

#~~
# Convenience converters
#~~

converter asBool*(val: RodValue): bool =
  result =
    case val.kind
    of rvkNull: false
    of rvkBool: val.boolVal
    else:       true

template convert(name, resultT: untyped,
                 dest: RodValueKind, field: untyped,
                 errName: string): untyped {.dirty.} =
  proc name*(val: RodValue): resultT =
    if val.kind == dest:
      result = val.field
    else:
      raise newException(TypeError, val.className & " is not " & errName)

convert num, float, rvkNum, numVal, "a number"
convert str, string, rvkStr, strVal, "a string"
convert class, RodClass, rvkClass, classVal, "a class"
convert obj, RodObj, rvkObj, objVal, "an object"
convert fn, RodBaseFn, rvkFn, fnVal, "a function"

converter asRodVal*(val: bool): RodValue =
  RodValue(kind: rvkBool, boolVal: val)

converter asRodVal*(val: float): RodValue =
  RodValue(kind: rvkNum, numVal: val)

converter asRodVal*(val: string): RodValue =
  RodValue(kind: rvkStr, strVal: val)

converter asRodVal*(val: RodClass): RodValue =
  RodValue(kind: rvkClass, classVal: val)

converter asRodVal*(val: RodObj): RodValue =
  RodValue(kind: rvkObj, objVal: val)

converter asRodVal*(val: RodBaseFn): RodValue =
  RodValue(kind: rvkFn, fnVal: val)

#~~
# Classes and objects
#~~

proc newClass*(name: string, dynamic = false): RodClass =
  result = RodClass(
    name: name,
    methods: newTable[string, RodBaseFn](),
    dynamic: dynamic, fields: initSet[string]()
  )

proc addFields*(class: var RodClass, names: varargs[string]) =
  for f in names:
    class.fields.incl(f)

proc `[]`*(class: RodClass, methodName: string): RodBaseFn =
  result = class.methods[methodName]

proc `[]=`*(class: var RodClass, methodName: string, impl: RodBaseFn) =
  class.methods[methodName] = impl

proc newObject*[T](class: RodClass, userdata: T): RodObj =
  result = RodObj(
    class: class,
    fields: newTable[string, RodValue](),
    userdata: newVariant(userdata)
  )

proc newObject*(class: RodClass): RodObj =
  result = newObject(class, newVariant(nil))

proc `[]`*(obj: RodObj, field: string): RodValue =
  result =
    if field in obj.fields:
      obj.fields[field]
    else:
      RodNull

proc `[]=`*(obj: var RodObj, field: string, val: RodValue) =
  obj.fields[field] = val

proc get*(obj: RodObj, T: typedesc): T =
  result = obj.userdata.get(T)

proc make*[T](obj: var RodObj, data: T) =
  obj.userdata = newVariant(data)
