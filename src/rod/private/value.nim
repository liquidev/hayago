#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

type
  RodValueKind* = enum
    rvNumber
  RodValue* = object
    case kind: RodValueKind
    of rvNumber: numberVal: float

proc kind*(value: RodValue): RodValueKind = value.kind

proc `==`*(a, b: RodValue): bool =
  if a.kind != b.kind: return false
  result =
    case a.kind
    of rvNumber: a.numberVal == b.numberVal

proc `$`*(value: RodValue): string =
  result =
    case value.kind
    of rvNumber: $value.numberVal

proc rod*[T: SomeNumber](value: T): RodValue =
  result = RodValue(kind: rvNumber, numberVal: float(value))

proc num*(value: RodValue): float =
  result =
    if value.kind == rvNumber: value.numberVal
    else: 0
