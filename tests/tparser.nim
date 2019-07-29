import unittest

import rod/private/scanner
import rod/private/parser

template parse(parser: untyped, input: string) =
  var scanner = initScanner(input, "testcase.rod")
  echo parser(scanner)

suite "parser":
  test "expressions":
    parse(parseExpr, """2 * 2 + 2""")
