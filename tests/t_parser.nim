import times
import unittest

import ../src/rod/[scanner, parser]
import utils

suite "parser":
  test "parseLiteral()":
    measureTime("parsing") do:
      var scan = newScanner("""
        /* Multiline
          /* nested */
          comment */

        "Hello, World!"
      """)
      echo parseLiteral(scan)
  test "parseVar()":
    measureTime("parsing") do:
      var scan = newScanner("""abc""")
      echo parseVar(scan)
  test "parseCall()":
    measureTime("parsing") do:
      var scan = newScanner("fun(42)");
      echo parsePrefix(scan)
  test "parsePrefix()":
    measureTime("parsing") do:
      var scan = newScanner("-10")
      echo parsePrefix(scan)
  test "parseInfix()":
    measureTime("parsing") do:
      #var scan = newScanner("3 + 4 * 2 / (1 − 5) ^ 2 ^ 3")
      var scan = newScanner("3 + 4 * 2 / (1 - 5) ^ 2 ^ 3")
      echo `$`(parseExpr(scan, 0), false)
  test "parseLet()":
    measureTime("parsing") do:
      var scan = newScanner("""
        let x = 2, y = 3;""")
      echo parseLet(scan)
  test "parseBlock()":
    measureTime("parsing") do:
      var scan = newScanner("""
        {
          let x = 2 * 4;
          x / 3;
        }
      """)
      echo parseBlock(scan)
  test "parseScript()":
    measureTime("parsing") do:
      var scan = newScanner("""
        let x = 10;
        let y = x + 2;
        print(x);
        curry(x)(y);
      """)
      echo parseScript(scan)
