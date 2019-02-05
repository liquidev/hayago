# Syntax

rod's syntax is minimal, yet intuitive, giving the programmer a simple, yet easy to follow set of rules.

## Comments

Comments are ignored during lexing. They divide into single-line, and multi-line:

```
// a single-line comment
// is terminated by a newline (\n)

/*
  a multi-line comment is terminated with a star and a slash
  /*
    unlike C (and many other languages), multi-line comments can be nested
   */
 */
```

## Literals

A literal is the smallest part of rod's syntax. They go directly into bytecode without any extra processing.

Here are the available literals:

```
// null
null

// booleans
true false

// numbers
42 123
3.14159

// strings
"Hello, World!"
"Hello,
World!" // strings can span over multiple lines
"Hello,\nWorld!" // escape sequences are supported
r"C:\Windows" // raw string literals don't process escape sequences
```

### String escape sequences

*Note: Escape sequences are case-sensitive.*

| Sequence | Description |
| --- | --- |
| `\a` | Terminal bell |
| `\b` | Backspace |
| `\f` | Form feed |
| `\n` | Line feed |
| `\r` | Carriage return |
| `\t` | Horizontal tabulator |
| `\v` | Vertical tabulator |
| `\e` | Escape |
| `\xHH` | Byte. `HH` are two hex digits |
| `\uHHHH` | 16-bit Unicode character. `HHHH` are four hex digits |
| `\UHHHHHHHH` | 32-bit Unicode character. `HHHHHHHH` are eight hex digits |
| `\[c]` | Inserts `[c]` into the string. `[c]` can be any character (which isn't a valid escape code), but this is most useful with `"` and `\`. |
