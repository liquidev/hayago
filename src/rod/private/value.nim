#--
# the rod scripting language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

import strutils

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
  Object* = ref object ## A rod object.
    fields*: seq[Value]
  Value* = object ## A rod value.
    case typeId*: TypeId ## The type ID, used for dynamic dispatch.
    of tyBool: boolVal*: bool
    of tyNumber: numberVal*: float
    of tyString: stringVal*: string
    else: objectVal*: Object
  Stack* = seq[Value] ## A runtime stack of values, used in the VM.
  StackView* = ptr UncheckedArray[Value] ## An unsafe view into a Stack.
  ForeignProc* = proc (args: StackView): Value ## A foreign proc.

proc `$`*(value: Value): string =
  result =
    case value.typeId
    of tyNil: "nil"
    of tyBool: $value.boolVal
    of tyNumber: $value.numberVal
    of tyString: escape($value.stringVal)
    else: "<object>"

proc initValue*(value: bool): Value =
  result = Value(typeId: tyBool, boolVal: value)

proc initValue*(value: float): Value =
  result = Value(typeId: tyNumber, numberVal: value)

proc initValue*(value: string): Value =
  result = Value(typeId: tyString, stringVal: value)

const
  nilObject* = -1

proc initObject*(id: TypeId, fieldCount: int): Value =
  result = Value(typeId: id)
  if fieldCount == nilObject:
    result.objectVal = nil
  else:
    result.objectVal = Object(fields: newSeq[Value](fieldCount))

