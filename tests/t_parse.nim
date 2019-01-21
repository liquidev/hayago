import unittest
from strutils import `%`

import rod/[lexer, parser]

include res/sources

template tryParse(input: string, parser: string): untyped =
  var tok = tokenize(input)
  parse(tok, parser).toLispStr(true)

template debugParse(input: string, parser: string): untyped =
  var tok = tokenize(input)
  parse(tok, parser).toLispStr(false)

proc testRule(rule: string,
              cases: openArray[tuple[input, assertion: string]]) =
  for test in cases:
    try:
      assert tryParse(test.input, rule) == test.assertion
    except AssertionError:
      echo "rule $1: case \"$2\" failed" % [rule, test.input]
      raise

suite "parsing":
  test "literals":
    testRule("literal", [
      ("null", "(rnNull)"),
      ("true", "(rnBool true)"),
      ("false", "(rnBool false)"),
      ("123.42", "(rnNum 123.42)"),
      ("\"Hello, World!\"", "(rnStr \"Hello, World!\")")
    ])
  test "identifiers":
    testRule("ident", [
      ("x", "(rnIdent x)"),
      ("xyz", "(rnIdent xyz)"),
      ("rust_case", "(rnIdent rust_case)"),
      ("nimCase", "(rnIdent nimCase)"),
      ("PascalCase", "(rnIdent PascalCase)"),
      ("CONSTANT_CASE", "(rnIdent CONSTANT_CASE)")
    ])
  test "calls":
    # testRule("call", [
    #   ("empty()", "(rnCall (rnVariable (rnIdent empty)) (rnCallArgs (rnEmpty)))"),
    #   ("oneArg(!true)", "(rnCall (rnVariable (rnIdent oneArg)) (rnCallArgs (rnPrefix (rnOperator rtExcl) (rnBool true))))"),
    #   ("moreArgs(1, true, x)", "(rnCall (rnVariable (rnIdent moreArgs)) (rnCallArgs (rnNum 1.0) (rnBool true) (rnIdent x)))"),
    #   ("nested(call())", "(rnCall (rnVariable (rnIdent nested)) (rnCallArgs (rnCall (rnVariable (rnIdent call)) (rnCallArgs (rnEmpty)))))"),
    #   ("prefix(!operatorCall())", "(rnCall (rnVariable (rnIdent prefix)) (rnCallArgs (rnPrefix (rnOperator rtExcl) (rnCall (rnVariable (rnIdent operatorCall)) (rnCallArgs (rnEmpty))))))")
    # ])
    echo debugParse("Object::new()", "call")
  test "prefix ops":
    testRule("prefixOp", [
      ("-42", "(rnPrefix (rnOperator rtMinus) (rnNum 42.0))"),
      ("~!!chained", "(rnPrefix (rnOperator rtTilde) (rnPrefix (rnOperator rtExcl) (rnPrefix (rnOperator rtExcl) (rnVariable (rnIdent chained)))))")
    ])
  test "infix ops":
    # testRule("infixOp", [
    #   ("2 + 3 * 4", "(rnInfix (rnOperator rtPlus) (rnNum 2.0) (rnInfix (rnOperator rtStar) (rnNum 3.0) (rnNum 4.0)))"),
    #   ("2 * 3 + 4", "(rnInfix (rnOperator rtPlus) (rnInfix (rnOperator rtStar) (rnNum 2.0) (rnNum 3.0)) (rnNum 4.0))"),
    #   ("2 + 3 == 5", "(rnInfix (rnOperator rtEq) (rnInfix (rnOperator rtPlus) (rnNum 2.0) (rnNum 3.0)) (rnNum 5.0))"),
    #   #("2 * (2 + x) / 6")
    # ])
    echo debugParse("2 * (2 + x) / 6", "infixOp")
  test "statements and blocks":
    echo debugParse("println(\"Hello, World!\");", "stmt")
    echo debugParse("""
      println(1);
      println(2);
    """, "script")
    echo debugParse("""
      {
        println(1)
        println("Hello")
      }
    """, "block")
  test "if expressions":
    echo debugParse("""
      if true {
        println("success!");
      } else if false {
        println("what?");
      } else {
        println("this will never execute, for sure.");
      }
    """, "if")
  test "loops":
    echo debugParse("""
      loop {
        println("you're trapped forever!");
      }
    """, "loop")
    echo debugParse("""
      while true {
        println("you're trapped forever, again!");
      }
    """, "while")
    echo debugParse("""
      for i in 1..10 {
        println(i);
      }
    """, "for")
  test "assignment":
    echo debugParse("x = 2", "assign")
    echo debugParse("(x, y, z) = player.get_pos()", "assign")
    echo debugParse("(p2.x, p2.y) = p1.tuple", "assign")
  test "variable declaration":
    echo debugParse("let x = 2", "let")
    echo debugParse("let mut x = 2", "let")
    echo debugParse("let x, y, z", "let")
    echo debugParse("let (x, y, z) = player.get_pos()", "let")
  test "scripts":
    echo debugParse("""
      let mut x = 0;
      while x < 20 {
        x = x + 1;
      }
    """, "script")
