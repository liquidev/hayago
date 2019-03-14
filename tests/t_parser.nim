import times
import unittest

import ../src/rod/[scanner, parser]
import utils

template testParse(parseFn: untyped, input: string): untyped =
  measureTime("parsing") do:
    var scan = newScanner(input)
    echo parseFn(scan)

suite "parser":
  test "parseLiteral":
    testParse(parseLiteral, """
      /* Multiline
        /* nested */
        comment */

      "Hello, World!"
    """)
  test "parseVar()":
    testParse(parseVar, """
      abc
    """)
  test "parseCall":
    testParse(parsePrefix, "fun(24)")
  test "parsePrefix":
    testParse(parsePrefix, "-10")
  test "parseIf":
    testParse(parsePrefix, """
      if x == 2 { 1 }
      else if x == 3 { 2 }
      else if x >= 6 { 3 }
      else { 4 }
    """)
    testParse(parsePrefix, """
      if x == y && y < x {
        x + y * 2
      }
    """)
    testParse(parseScript, """
      if x {}
      if y {}
      if z {}
    """)
  test "parseDo":
    testParse(parseScript, """
      let x = do {
        let a = f();
        a + 3
      };
    """)
  test "parseInfix":
    testParse(parseAssign, "3 + 4 * 2 / (1 - 5) ^ 2 ^ 3")
    testParse(parseAssign, "x == y && y > x")
  test "parseLet":
    testParse(parseLet, """
      let x = 2, y = 3;
    """)
  test "parseLoop":
    testParse(parseLoop, """
      loop {
        print("Hello");
      } while x < 10;
    """)
  test "parseWhile":
    testParse(parseWhile, """
      while x < 30 {
        println(x);
        x = x + 1;
      }
    """)
  test "parseFor":
    testParse(parseFor, """
      for i in 0..10 {
        println(i);
      }
    """)
  test "parseScript":
    testParse(parseScript, """
      let x = 10;
      let y = x + 2;
      print(x);
      curry(x)(y);
    """)
