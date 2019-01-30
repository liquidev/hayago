#~~
# rod values
# copyright (C) iLiquid, 2018
#~~

import tables

type
  RodBaseVM* = ref object of RootObj
    ## A base rod VM. This is used heavily throughout rod, and allows \
    ## for extra modularity.
    ## Side note: Totally didn't make this because Nim doesn't support cyclic \
    ## imports yet.
    foreignFns*: seq[RodForeignFn]
    foreignFnSignatures*: TableRef[string, int]
  RodCompatVM* = concept vm, var vvm
    vm is RodBaseVM; vvm is RodBaseVM
    vm.registerForeignFn(string, RodForeignFn)
  RodForeignFn* = proc (vm: var RodCompatVM)
    ## A foreign function, used for low-level binding.
    ## If you want something simpler, see the ``autorod`` module.

  RodValueKind* = enum
    rvNull ## a null value
    rvBool ## true or false
    rvNum  ## a Nim ``float``
    rvStr  ## a Nim ``string``
  RodValue* = object
    ## The base value type in rod. Stores its kind and Nim value, necessary \
    ## for dynamic typing.
    case kind*: RodValueKind
    of rvNull: discard
    of rvBool: boolVal*: bool
    of rvNum:  numVal*: float
    of rvStr:  strVal*: string

proc nullVal*(): RodValue =
  ## Creates a rod null value.
  RodValue(kind: rvNull)

proc boolVal*(val: bool): RodValue =
  ## Creates a rod boolean value.
  RodValue(kind: rvBool, boolVal: val)

proc numVal*(val: float): RodValue =
  ## Creates a rod number value.
  RodValue(kind: rvNum, numVal: val)

proc strVal*(val: string): RodValue =
  ## Creates a rod string value.
  RodValue(kind: rvStr, strVal: val)
