#~~
# the rod programming language
# copyright (C) iLiquid, 2018
# licensed under the MIT license
#~~

type
  RodOpcode* = enum
    roPushConst = "push_const"
    roCall = "call"
