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

const
  tyBool* = 0
  tyNumber* = 1
  tyString* = 2
  tyObject* = {16..high(uint16)}

type
  RawValue* {.union.} = object ## A raw, untagged value.
    bytes*: array[ValueSize, uint8]
    boolVal*: bool
    numberVal*: float
    stringVal*: string
  Value* = object ## A rod value.
    typeId*: uint16 ## The type ID, used for dynamic dispatch.
    into*: RawValue
  Stack* = seq[Value] ## A runtime stack of values, used in the VM.
  StackView* = ptr UncheckedArray[Value] ## An unsafe view into a Stack.
  ForeignProc* = proc (args: StackView): Value ## A foreign proc.

proc boolStr*(value: Value): string =
  result = $value.into.boolVal

proc numberStr*(value: Value): string =
  result = $value.into.numberVal

proc initValue*(value: bool): Value =
  result = Value(typeId: tyBool)
  result.into.boolVal = value

proc initValue*(value: float): Value =
  result = Value(typeId: tyNumber)
  result.into.numberVal = value

proc initValue*(value: string): Value =
  result = Value(typeId: tyString)
  result.into.stringVal = value

