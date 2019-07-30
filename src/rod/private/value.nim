#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

import strutils

type
  RodValueKind* = enum
    rvNumber
    rvString
  RodValue* = object
    case kind: RodValueKind
    of rvNumber: numberVal: float
    of rvString: stringVal: string

proc kind*(value: RodValue): RodValueKind = value.kind

proc `==`*(a, b: RodValue): bool =
  if a.kind != b.kind: return false
  result =
    case a.kind
    of rvNumber: a.numberVal == b.numberVal
    of rvString: a.stringVal == b.stringVal

proc `$`*(value: RodValue): string =
  result =
    case value.kind
    of rvNumber: $value.numberVal
    of rvString: escape(value.stringVal)

proc rod*[T: SomeNumber](value: T): RodValue =
  result = RodValue(kind: rvNumber, numberVal: float(value))

proc rod*(value: string): RodValue =
  result = RodValue(kind: rvString, stringVal: value)

proc num*(value: RodValue): float =
  result =
    if value.kind == rvNumber: value.numberVal
    else: 0

proc str*(value: RodValue): string =
  result =
    if value.kind == rvString: value.stringVal
    else: ""
