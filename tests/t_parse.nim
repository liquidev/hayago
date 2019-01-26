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
  test "constructors":
    echo debugParse("[1, true, 2, \"hello\", \"world\"]", "constructor")
    echo debugParse("""
      [
        "nested",
        ["vectors"]
      ]
    """, "constructor")
    echo debugParse("""
      {
        "myValue": 1,
        syntactic: ["sugar"],
        4: "anything can be a",
        ["key"]: true,
        {even: "another"}: "table"
      }
    """, "constructor")
  test "identifiers":
    testRule("ident", [
      ("x", "(rnIdent x)"),
      ("xyz", "(rnIdent xyz)"),
      ("rust_case", "(rnIdent rust_case)"),
      ("nimCase", "(rnIdent nimCase)"),
      ("PascalCase", "(rnIdent PascalCase)"),
      ("CONSTANT_CASE", "(rnIdent CONSTANT_CASE)")
    ])
  test "types":
    echo debugParse("str", "type")
    echo debugParse("Vec<num>", "type")
    echo debugParse("Table<num, str>", "type")
    echo debugParse("Map<Tile>", "type")
  test "calls":
    echo debugParse("Object::new()", "call")
    echo debugParse("Table::new<str, str>()", "call")
  test "prefix ops":
    testRule("prefixOp", [
      ("-42", "(rnPrefix (rnOperator rtMinus) (rnNum 42.0))"),
      ("~!!chained", "(rnPrefix (rnOperator rtTilde) (rnPrefix (rnOperator rtExcl) (rnPrefix (rnOperator rtExcl) (rnVariable (rnIdent chained)))))")
    ])
  test "infix ops":
    echo debugParse("2 * (2 + x) / 6", "infixOp")
  test "index operator":
    echo debugParse("x[2]", "index")
    echo debugParse("map[0, 0]", "index")
    echo debugParse("map[1, 2][\"id\"]", "index")
  test "statements and blocks":
    echo debugParse("println(\"Hello, World!\");", "stmt")
    echo debugParse("""
      println(1);
      println(2);
    """, "script")
    echo debugParse("""
      {
        println(1);
        println("Hello");
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
  test "closures":
    echo debugParse("|x| x + 2", "closure")
    echo debugParse("""
      |ctx| {
        ctx.clear();
      }
    """, "closure")
    echo debugParse("|| { println(\"callback\"); }", "closure")
  test "variable declaration":
    echo debugParse("let x = 2", "let")
    echo debugParse("let mut x = 2", "let")
    echo debugParse("let x, y, z", "let")
    echo debugParse("let (x, y, z) = player.get_pos()", "let")
  test "function declaration":
    echo debugParse("fn main(args: str) { println(args[0]); }", "fn")
    echo debugParse("fn main(args: str) -> Result { Result(OK) }", "fn")
  test "class declaration":
    echo debugParse("""
      class Greeter {
        fn say_hello() {
          println("Hello!");
        }
      }
    """, "class")
    echo debugParse("""
      class InstTest {
        fn inst_fn(self) {
          println(self);
        }
        fn []= (self, index, value) {
          self._set_impl(index, value);
        }
      }
    """, "class")
    echo debugParse("""
      class Vec2 {
        let
          x: num,
          y: num;

        pub fn new(x: num, y: num) -> Vec2 {
          Vec2 { x, y }
        }

        pub fn + (self, other: Vec2) -> Vec2 {
          Vec2::new(self.x + other.x, self.y + other.y)
        }

        pub fn - (self, other: Vec2) -> Vec2 {
          Vec2::new(self.x - other.x, self.y - other.y)
        }

        pub fn * (self, other: Vec2) -> Vec2 {
          Vec2::new(self.x * other.x, self.y * other.y)
        }

        pub fn / (self, other: Vec2) -> Vec2 {
          Vec2::new(self.x / other.x, self.y / other.y)
        }
      }
    """, "class")
    echo debugParse("""
    class Mapping<K, V> {
      let mut
        table: Table<K, V>;

      pub fn new() -> Mapping {
        Mapping { table: {} }
      }

      pub fn [] (self, key: K) -> V {

      }
    }
    """, "class")
    # echo debugParse("""
    #   class Test {
    #     let mut
    #       x: num,
    #       y: num;

    #     fn new() {
    #       Test { x: 0, y: 0 }
    #     }

    #     fn increment(self, x: num, y: num) {
    #       self.x = self.x + x;
    #       self.y = self.y + y;
    #     }

    #     fn concat(self) -> num {
    #       self.x + self.y
    #     }
    #   }
    # """, "class")
