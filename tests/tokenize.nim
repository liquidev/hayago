import strutils
import unittest

import rod/lexer

include res/sources

suite "tokens":
  # TODO: check if the results are valid
  test "functions":
    let result = tokenize(SrcFn)
  test "all tokens":
    let result = tokenize(SrcAllTokens)
  test "vec2":
    let result = tokenize(SrcVec2)
