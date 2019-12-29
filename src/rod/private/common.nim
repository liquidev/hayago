#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

type
  RodErrorKind* = enum
    reSyntax
    reCompile
  RodError* = object of CatchableError
    case kind*: RodErrorKind
    of reSyntax, reCompile:
      file*: string
      ln*, col*: int

template OrdTable*(K, V: typedesc): typedesc =
  array[low(K)..high(K), V]
