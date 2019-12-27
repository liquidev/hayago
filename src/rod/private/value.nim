#--
# the rod scripting language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

const
  ValueSize* = max([
    sizeof(bool),
    sizeof(float),
    sizeof(string)
  ])

type
  Value* {.union.} = object ## A value. \
    ## Values are union types. They don't need to be annotated with an extra
    ## type field, because all types are resolved and checked statically. This
    ## improves execution time greatly, because runtime checks involving value
    ## types are not required.
    rawBytes*: array[ValueSize, uint8]
    boolVal*: bool
    numberVal*: float
    stringVal*: string

proc boolStr*(value: Value): string =
  result = $value.boolVal

proc numberStr*(value: Value): string =
  result = $value.numberVal

proc initValue*(value: bool): Value =
  result = Value(boolVal: value)

proc initValue*(value: float): Value =
  result = Value(numberVal: value)

