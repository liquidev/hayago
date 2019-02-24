import unittest

import ../src/rod/[scanner, parser]

suite "parser":
  test "parseLiteral()":
    var scan = newScanner("123")
    echo parseLiteral(scan)
