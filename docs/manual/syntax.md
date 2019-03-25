# Syntax

rod's syntax is highly inspired by Rust – it uses similar, short, easy-to-type
keywords, but it also features advanced object orientation.

## Comments

rod features single-line, and multi-line comments:

```rust
// A single-line comment is preceded with `//`
/*
 * A multi-line comment starts with '/*' and ends with '*/'
 * Notice how the above use of /* */ doesn't break anything: that's because
 * multi-line comments can be nested.
 */
```

## Keywords

rod has the following keywords:

```
break continue do else for if in loop return while
fn let
pub use
null true false
is
```

Keep in mind that those keywords are *not quite* reserved, but when you do use
them as identifiers, do so with care. Keywords are banned as identifiers where:
 - A statement starts with a keyword.
 - (more strict rules may be added in the future)

For instance, this will compile and run properly:

```rust
fn return(x) {
  x
}

let useless = return(2);
```

But this will not:

```rust
let return = 10;
return = 2; // error: expression expected after 'return'
```

## Literals

The core syntax is comprised of literals. Literals are non-terminal AST nodes.

### Null

Unlike Rust, rod features a `null`. It's simply written as `null`.
The presence of `null` means that there is no usable value in a variable
or field, however, options are preferred over `null` to signify a value
intentionally left uninitialized.

### Booleans

rod has a standard, boolean type. Its value is either `true` or `false`.
In logic comparison and branching, `true` is always "truthy", and `false` is
always falsey.

`null` the only other falsey value. All other values are truthy.

### Numbers

rod supports standard integer, floating point, and hex literals:
```rust
5 // integer literal
3.14159 // floating point literal
0x30de // hex literal
```

### Strings

In rod, strings are simply enclosed in `""`:
```rust
"Hello, World!"
```
As of now, string literals don't have any special features, like escape
sequences, but that is subject to change.

### Identifiers

rod identifiers are sequences of ASCII characters from the following set:
```nim
{ 'a'..'z', 'A'..'Z', '0'..'9', '_' }
```

Although rod doesn't strictly enforce them, the following naming conventions
apply and should be used in idiomatic code:
 - use `snake_case` for variable and function names,
 - use `PascalCase` for types (structs, classes and traits),
 - use `lowercase` for module names, to make them OS-independent.
Other naming conventions are allowed, but discouraged—especially in the case of
public code (libraries, examples, open-source apps).
