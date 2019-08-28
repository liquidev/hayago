#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import tables

type
  RodTypeKind* = enum
    tkSimple
    tkObject
  RodType* = ref object
    name*: string
    case kind*: RodTypeKind
    of tkSimple: discard
    of tkObject:
      fields*: Table[string, Field]
  Field* = tuple
    name: string
    ty: RodType
