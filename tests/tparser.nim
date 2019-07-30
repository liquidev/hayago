import unittest

import rod/private/scanner
import rod/private/parser

template parse(parser: untyped, input: string) =
  var scanner = initScanner(input, "testcase.rod")
  echo parser(scanner)

suite "parser":
  test "expressions":
    parse(parseExpr, r"2 * 2 + 2")
  test "statements":
    parse(parseStmt, r"var x = 2, y = 3, z = 4")
    parse(parseStmt, r"let x = 3, y = 4, z = 5")
