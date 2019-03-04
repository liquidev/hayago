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
  test "parseVar()":
    var scan = newScanner("""abc""")
    echo parseVar(scan)
  test "parseCall()":
    var scan = newScanner("fun(42)");
    echo parsePrefix(scan)
  test "parsePrefix()":
    var scan = newScanner("-10")
    echo parsePrefix(scan)
  test "parseInfix()":
    #var scan = newScanner("3 + 4 * 2 / (1 − 5) ^ 2 ^ 3")
    var scan = newScanner("3 + 4 * 2 / (1 - 5) ^ 2 ^ 3")
    echo `$`(parseExpr(scan, 0), false)
  test "parseLet()":
    var scan = newScanner("""
      let x = 2, y = 3;""")
    echo parseLet(scan)
  test "parseBlock()":
    var scan = newScanner("""
      {
        let x = 2 * 4;
        x / 3;
      }
    """)
    echo parseBlock(scan)
  test "parseScript()":
    var scan = newScanner("""
      let x = 10;
      let y = x + 2;
      print(x);
      curry(x)(y);
    """)
    echo parseScript(scan)
