#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

type
  RodOpcode* = enum
    roPushConst  = "push_const"
    roPushGlobal = "push_global"
    roPushLocal  = "push_local"
    roDiscard    = "discard"
    roPopGlobal  = "pop_global"
    roPopLocal   = "pop_local"
    roCall       = "call"
