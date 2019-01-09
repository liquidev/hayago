#~~
# source code samples
#~~

const SrcCall = "println();"

const SrcFn = """
fn main() {}
fn withArguments(x, y) {}
fn withStatements(x, y) {
  let z = x + y;
  println("{} {} {}", x, y, z);
}
"""

const SrcAllTokens = """
// a comment, should be ignored
/* another comment, also should be ignored */

// brackets
() [] {}

// operators
+ - * / %             // math
. , :: : ;            // punctuation
== != < > <= >=       // comparison
! || && | & << <<< >> // bitwise/boolean
=                     // assignment

// types
null true false       // keyword types
123 42 3.14 1 2.3 6.2 // numbers
"hello, world"        // strings

// keywords
if for while return continue break // flow control
let fn                             // declarations
class mixin enum self              // OOP
use pub                            // modularity

// identifiers
hello_world helloWorld HelloWorld
testing Testing
rust_case nimCase ClassCase CONSTANT_CASE

"""

const SrcVec2 = """
use strutil::{format};

pub fn test_fn(arg1, arg2) {
  let my_num = 32 + arg1 - arg2;
  return my_num;
}

class Vec2 {
  let x, y;

  pub fn new(x, y) {
    Vec2 { x: x, y: y }
  }

  pub fn op+(self, other) {
    Vec2::new(self.x + other.x, self.y + other.y)
  }

  pub fn op-(self, other) {
    Vec2::new(self.x - other.x, self.y + other.y)
  }

  pub fn to_str(self) {
    "Vec2({}, {})".format(self.x, self.y)
  }
}

mixin Vec2 class Vec2MoreOps {
  pub fn op*(self, other) {
    Vec2::new(self.x * other.x, self.y * other.y)
  }

  pub fn op/(self, other) {
    Vec2::new(self.x / other.x, self.y / other.y)
  }
}

let my_vector = Vec2::new(0, 0);
let another = my_vector + Vec2::new(10, 20);

"""
