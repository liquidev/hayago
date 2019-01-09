#~~
# rod parser
# copyright (C) iLiquid, 2019
#~~

import tables

import lexer, stream

type
  RodParseRule = proc (input: Stream[Token]): RodNode

  RodNode* = object of RootObj
  RodInvalidNode* = object of RodNode
    token: Token

  RodExpr* = object of RootObj


var rules = initTable[string, RodParseRule]()

template rule(name: string, body: untyped): untyped {.dirty.} =
  rules[name] = proc (input: Stream[Token]): RodNode =
    body

rule "stmt":
  discard

template execAll() {.dirty.} =
  discard

proc parse*(tokens: seq[Token]): RodNode =
  var istr = Stream[Token](input: tokens)
  while not istr.finished:
    execAll()
