#~~
# simple stream implementation
# copyright (C) iLiquid, 2018
#~~

type
  Stream*[T] = object
    input*: seq[T]
    pos*: int

proc finished*(str: Stream): bool =
  str.pos == str.input.len

proc peek*[T](str: Stream[T], amt: int = 1, offset: int = 0): seq[T] =
  if str.pos + amt + offset - 1 < str.input.len:
    result = str.input[str.pos + offset..<str.pos + amt + offset]

proc consume*[T](str: var Stream[T], amt: int = 1): seq[T] =
  result = str.peek(amt)
  str.pos += amt
