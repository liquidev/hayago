#~~
# the rod programming language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#~~

type
  RodOpcode* = enum
    #~ variables and values
    roNewGlobal   = "new_global"
    roPushConst   = "push_const"
    roPushGlobal  = "push_global"
    roPushLocal   = "push_local"
    roDiscard     = "discard"
    roPopGlobal   = "pop_global"
    roPopLocal    = "pop_local"
    roStoreGlobal = "store_global"
    roStoreLocal  = "store_local"
    #~ functions and methods
    roPushMethod  = "push_method"
    roCallFn      = "call_fn"
    roCallMethod  = "call_method"
    #~ flow control
    roJump        = "jump"
    roJumpCond    = "jump_cond"
    roReturn      = "return"
