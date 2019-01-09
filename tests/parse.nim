import unittest

import rod/[lexer, parser]

include res/sources

suite "parsing":
  test "basic AST":
    echo parse(tokenize(SrcCall))
