import unittest

import ../src/rod/[scanner, parser]

suite "parser":
  test "parseLiteral()":
    var scan = newScanner("""
      /* Multiline
         /* nested */
         comment */

      "Hello, World!"
    """)
    echo parseLiteral(scan)
  test "parsePrefix()":
    var scan = newScanner("-10")
    echo parsePrefix(scan)
  test "parseInfix()":
    #var scan = newScanner("3 + 4 * 2 / (1 − 5) ^ 2 ^ 3")
    var scan = newScanner("3 + 4 * 2 / (1 - 5) ^ 2 ^ 3")
    echo parseInfix(scan)
