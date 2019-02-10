# Operators

rod has plenty of operators to offer, including some special ones for user-defined operations.

## Prefix operators

Prefix operators are parsed inside-out, so the ones closer to an atom have a higher precedence than the ones farther:
```
~-x
```
```
(rnInfixOp
  (rnOperator ~)
  (rnInfixOp
    (rnOperator -)
    (rnVariable
      (rnIdent x))))
```

The available prefix operators include:
 - `+`
 - `-` negation
 - `!` boolean not
 - `~` bitwise not

Prefix operators always have precedence over infix operators.

## Infix operators

The precedence of operators is inspired [Nim's operator precedence](https://nim-lang.org/docs/manual.html#syntax-precedence).
Here's a list of infix operators and their precedences:

| Level | Operators |
| --- | --- |
| 10 | `*` `/` `%` `<<` `>>` |
|  9 | `+` `-` `|` |
|  8 | `&` |
|  7 | `..` `...` |
|  6 | `<` `>` `<=` `>=` `==` `!=` `<=>` |
|  5 | `is` `in` |
|  4 | `&&` |
|  3 | `||` |
|  2 | `=` `:=` |
|  1 | reserved for future use |

### Level 10: Multiplicative

```
assert(2 * 2 == 4);
assert(1 / 2 == 0.5);
assert(3 % 2 == 1);
assert(0b0001 << 2 == 0b0100);
assert(0b0100 >> 2 == 0b0001);
```

### Level 9 and 8: Additive

```
assert(2 + 2 == 4);
assert(4 - 1 == 3);
assert(0b0101 | 0b1010 == 0b1111);
assert(0b0110 & 0b1100 == 0b0100);
```

### Level 7: Ranges

```
// prints numbers from 0 to 10
for i in 0..10 {
  println(i);
}

// prints numbers from 0 to 9
for i in 0..9 {
  println(i);
}
```

### Level 6: Comparison

```
assert(1 < 2);
assert(4 > 3);
assert(2 <= 2.4);
assert(3 >= 3);
assert(3 == 3);
assert(3 != 5);
assert((3 <=> 3) == 0);
assert((3 <=> 4) < 0);
assert((3 <=> 2) > 0);
```

### Level 5

```
assert(2 is num);
assert("hello" is str);
assert("y" in ["x", "y", "z"]);
```

### Level 4 and 3

```
assert((true && false) == false)
assert((true || false) == true)
```

### Level 2

```
let mut x = 2;
assert((x = 3) == 3);
assert(x == 3);
```

The bind (`:=`) operator is a special one, because it doesn't have
an implementation in most built-in classes. The `Box<T>` class uses it
for value setting, though:

```
let mut box = Box(2);
box := 3;
assert(box.value == 3);
```
