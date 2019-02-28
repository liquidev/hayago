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

## Literals

The core syntax is comprised of literals. They're the smallest AST node,
and so, are leaf (non-terminal) nodes.

### Null

Unlike Rust, rod features a `null`. It's simply written as `null`.
The presence of `null` means that there is no usable value in a variable
or field, however, options are preferred over `null` to signify a value
intentionally left uninitialized.

### Booleans

rod has a standard, distinct boolean type. Its value is either `true` or
`false`. Nothing too fancy.

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
