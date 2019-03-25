#~~
# the rod programming language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#~~

## This module contains binding/FFI utilities for rod.
## This is a pretty high-level API. It allows you to bind procs *really* \
## easily, using a few macros.
## Following rod's general design, this module doesn't have any global \
## state.

import macros
import tables

import value
import vm

dumpTree:
  proc someFn*(a, b: int): int =
    result = a * 2 + b / a

macro bindFn*(p: untyped): NimNode =
  if p.kind == nnkProcDef:
    discard
  else:
    error("bindFn is for procs only", p)
