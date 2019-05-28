#--
# the rod scripting language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#--

## This module is rod's parser, implemented in NPeg.
## Using this library allows for very easy changes in the syntax, unlike using \
## a manual recursive descent parser which can get really messy over time.
## This parser is also the official syntax definition of rod, so build your \
## implemenations around it.

import strutils

import npeg

type
  RodNodeKind* = enum
    rnkNum, rnkStr, rnkIdent
    rnkPrefix, rnkInfix
  RodNode* = ref object
    case kind*: RodNodeKind
    of rnkNum:   numVal*: float
    of rnkStr:   strVal*: string
    of rnkIdent: ident*: string
    else:        children*: seq[RodNode]

proc `$`*(node: RodNode): string =
  result =
    case node.kind
    of rnkNum:   $node.numVal
    of rnkStr:   '"' & node.strVal & '"'
    of rnkIdent: node.ident
    else:        '(' & ($node.kind)[3..^1] & ' ' & node.children.join(" ")

proc makeNum*(num: float): RodNode =
  result = RodNode(kind: rnkNum, numVal: num)

proc makeStr*(str: string): RodNode =
  result = RodNode(kind: rnkStr, strVal: str)

proc parseRod*(script: string): RodNode =
  var stack: seq[RodNode]

  template push(node: RodNode) =
    stack.add(node)
  template pop(): RodNode =
    stack.pop()

  template infix() =
    let
      b = pop()
      a = pop()
    push()

  let rodParser = peg "Script":
    #--
    # Errors
    #--

    ErrBinDigitsExpected <- E"One or more binary digits expected"
    ErrOctDigitsExpected <- E"One or more octal digits expected"
    ErrHexDigitsExpected <- E"One or more hexadecimal digits expected"

    #--
    # Punctuation
    #--

    # Tabs are prohibited, and it's intentional.
    # Plain whitespace looks consistent across setups, regardless of the tab
    # width set there.
    wsr <- {' ', '\l', '\c'}
    ws <- ?wsr

    # Dang Windows. Prefer the Unix way
    lf <- '\l' | '\c' * '\l'

    #--
    # Numbers
    #--

    BinDigit <- {'0', '1'}
    OctDigit <- {'0', '7'}

    BinInt <- i"0b" * (>(+BinDigit) | ErrBinDigitsExpected):
      push(parseBinInt($1).float.makeNum)
    OctInt <- i"0o" * (>(+OctDigit) | ErrOctDigitsExpected):
      push(parseOctInt($1).float.makeNum)
    HexInt <- i"0x" * (>(+Xdigit) | ErrHexDigitsExpected):
      push(parseHexInt($1).float.makeNum)
    Float <- >(+Digit * ?('.' * +Digit) * ?({'e', 'E'} * +Digit)):
      push(parseFloat($1).makeNum)
    Num <- HexInt | BinInt | OctInt | Float

    #--
    # Strings
    #--

    Utf8Cont <- { 128..191 }
    Utf8Char <- { 0..127 } |
                { 194..223 } * Utf8Cont[1] |
                { 224..239 } * Utf8Cont[2] |
                { 240..244 } * Utf8Cont[3]

    StrChar <- Utf8Char
    Str <- '"' * >(*(StrChar - '"')) * '"':
      push ($1).makeStr

    #--
    # Values
    #--

    Lit <- Num | Str

    Atom <- Lit # temporary, change to Infix7

    #--
    # Operations
    #--

    PrefixOp <- '+' | '-' | "not"
    InfixOp1 <- '=' | ":=" | "::="
    InfixOp2 <- "or"
    InfixOp3 <- "and"
    InfixOp4 <- "==" | "!=" | "<=" | "<" | ">=" | ">" | "in" | "of"
    InfixOp5 <- ".." | "..<"
    InfixOp6 <- '+' | '-'
    InfixOp7 <- '*' | '/' | "mod"

    Prefix <- >PrefixOp * Atom | Atom

    Infix7 <- Infix6 * ws * InfixOp7 * ws * Infix6 | Infix6
    Infix6 <- Infix5 * ws * InfixOp6 * ws * Infix5 | Infix5
    Infix5 <- Infix4 * ws * InfixOp5 * ws * Infix4 | Infix4
    Infix4 <- Infix3 * ws * InfixOp4 * ws * Infix3 | Infix3
    Infix3 <- Infix2 * ws * InfixOp3 * ws * Infix2 | Infix2
    Infix2 <- Infix1 * ws * InfixOp2 * ws * Infix1 | Infix1
    Infix1 <- Prefix * ws * InfixOp1 * ws * Prefix | Prefix

    Expr <- Infix7

    #--
    # Top level
    #--

    Script <- Lit

  discard rodParser.match(script)
  result = stack[0]
