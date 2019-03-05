#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

import sets
import tables

import variant

type
  RodFnSignature* = tuple
    name: string
    arity: int
  RodClass* = ref object
    name*: string
    prototype*: TableRef[RodFnSignature, RodObj]
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
    rvkObj
  RodValue* = object
    case kind*: RodValueKind
    of rvkNull: discard
    of rvkBool: boolVal*: bool
    of rvkNum:  numVal*: float
    of rvkStr:  strVal*: string
    of rvkObj:  objVal*: RodObj

converter asBool*(val: RodValue): bool =
  case val.kind
  of rvkNull: return false
  of rvkBool: return val.boolVal
  else: raise newException(ValueError, "The value " & $val & "is not " &
    "implicitly convertible to a boolean")

proc `==`*(a, b: RodValue): bool =
  if a.kind == b.kind:
    case a.kind
    of rvkNull: return true
    of rvkBool: return a.boolVal == b.boolVal
    of rvkNum:  return a.numVal == b.numVal
    of rvkStr:  return a.strVal == b.strVal
    of rvkObj:  return a.objVal == b.objVal

proc `!=`*(a, b: RodValue): bool = not (a == b)

proc `$`*(val: RodValue): string =
  result.add("<")
  result.add(
    case val.kind
    of rvkNull: "null"
    of rvkBool: $val.boolVal
    of rvkNum: $val.numVal
    of rvkStr: '"' & val.strVal & '"'
    of rvkObj: "object " & val.objVal.class.name
  )
  result.add(">")

let RodNull* = RodValue(kind: rvkNull)

converter asRodVal*(val: bool): RodValue =
  RodValue(kind: rvkBool, boolVal: val)

converter asRodVal*(val: float): RodValue =
  RodValue(kind: rvkNum, numVal: val)

converter asRodVal*(val: string): RodValue =
  RodValue(kind: rvkStr, strVal: val)
