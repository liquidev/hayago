#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

type
  RodOpcode* = enum
    #~ variables and values
    roPushConst  = "push_const"
    roPushGlobal = "push_global"
    roPushLocal  = "push_local"
    roDiscard    = "discard"
    roPopGlobal  = "pop_global"
    roPopLocal   = "pop_local"
    #~ functions and methods
    # The difference between ``push_method`` and ``method`` is that
    # ``push_method`` doesn't pop the receiver off the stack,
    # while ``method`` does.
    roPushMethod = "push_method"
    roMethod     = "method"
    roCallFn     = "call_fn"
    roCallMethod = "call_method"
