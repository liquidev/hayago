#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import strutils
import tables

type
  RodTypeKind* = enum
    tkSimple
    tkObject
  RodType* = ref object
    name*: string
    params*: seq[string]
    parents*: seq[RodType]
    case kind*: RodTypeKind
    of tkSimple: discard
    of tkObject:
      objFields*: Table[string, Field]
  Field* = tuple
    id: int
    name: string
    ty: RodType

proc `$`*(ty: RodType): string =
  result = ty.name & '[' & ty.params.join(", ") & "] = "
  case ty.kind
  of tkSimple: result.add("simple")
  of tkObject:
    result.add("object { ")
    for n, f in ty.objFields:
      result.add(n & ": " & f.ty.name & "; ")
    result.add("}")
