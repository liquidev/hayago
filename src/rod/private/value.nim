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
  tyNil* = 0
  tyBool* = 1
  tyNumber* = 2
  tyString* = 3
  tyFirstObject* = 4

type
  TypeId* = range[0..32766] # Max amount of case object branches
  RawValue* {.union.} = object ## A raw, untagged value.
    bytes*: array[ValueSize, uint8]
    boolVal*: bool
    numberVal*: float
    stringVal*: string
  Value* = object ## A rod value.
    case typeId*: TypeId ## The type ID, used for dynamic dispatch.
    of tyBool: boolVal*: bool
    of tyNumber: numberVal*: float
    of tyString: stringVal*: string
    else:
      isNil*: bool # keeping this as a field simplifies dynamic dispatch
      objectFields*: seq[Value]
  Stack* = seq[Value] ## A runtime stack of values, used in the VM.
  StackView* = ptr UncheckedArray[Value] ## An unsafe view into a Stack.
  ForeignProc* = proc (args: StackView): Value ## A foreign proc.

proc initValue*(value: bool): Value =
  result = Value(typeId: tyBool, boolVal: value)

proc initValue*(value: float): Value =
  result = Value(typeId: tyNumber, numberVal: value)

proc initValue*(value: string): Value =
  result = Value(typeId: tyString, stringVal: value)

