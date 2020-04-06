#--
# the rod scripting language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

import std/hashes
import std/strutils

type
  NodeKind* = enum
    # building blocks
    nkEmpty          # empty node
    nkScript         # full script
    nkBlock          # a block - {...}
    nkIdentDefs      # identifier definitions - a, b: s = x
    nkFormalParams   # formal params - (a: s, ...) -> t
    nkGenericParams  # generic params - [T, U: X]
    nkRecFields      # record fields - { a, b: t; c: u }
    # literals
    nkBool           # bool literal
    nkNumber         # number literal
    nkString         # string literal
    nkIdent          # identifier
    # expressions
    nkPrefix         # prefix operator - op expr
    nkInfix          # infix operator - left op right
    nkDot            # dot expression - left.right
    nkColon          # colon expression - left: right
    nkIndex          # index expression - left[a, ...]
    nkCall           # call - left(a, ...)
    nkIf             # if expression - if expr {...} elif expr {...} else {...}
    # types
    nkProcTy         # procedure type - proc (...) -> t
    # statements
    nkVar            # var declaration - var a = x
    nkLet            # let declaration - let a = x
    nkWhile          # while loop - while cond {...}
    nkFor            # for loop - for x in y {...}
    nkBreak          # break statement - break
    nkContinue       # continue statement - continue
    nkReturn         # return statement - return x
    nkYield          # yield statement - yield x
    # declarations
    nkObject         # object declaration - object o[T, ...] {...}
    nkProc           # procedure declaration - proc p(a: s, ...) -> t {...}
    nkIterator       # iterator declaration - iterator i(a: s, ...) -> t {...}
  Node* = ref object          ## An AST node.
    ln*, col*: int            ## Line information used for compile errors
    file*: string
    case kind*: NodeKind      ## The kind of the node
    of nkEmpty: ## Empty node
      discard
    of nkBool: ## Bool literal
      boolVal*: bool
    of nkNumber: ## Number literal
      numberVal*: float
    of nkString: ## String literal
      stringVal*: string
    of nkIdent: ## Identifier (may get merged with nkString)
      ident*: string
    else: ## Any other branch nodes
      children*: seq[Node]

const LeafNodes = {
  nkEmpty, nkBool, nkNumber, nkString, nkIdent
}

proc len*(node: Node): int =
  result = node.children.len

proc `[]`*(node: Node, index: int | BackwardsIndex): Node =
  result = node.children[index]

proc `[]`*(node: Node, slice: HSlice): seq[Node] =
  result = node.children[slice]

proc `[]=`*(node: Node, index: int | BackwardsIndex, child: Node) =
  node.children[index] = child

iterator items*(node: Node): Node =
  for child in node.children:
    yield child

iterator pairs*(node: Node): tuple[i: int, n: Node] =
  for i, child in node.children:
    yield (i, child)

proc add*(node, child: Node): Node {.discardable.} =
  node.children.add(child)
  result = node

proc add*(node: Node, children: openarray[Node]): Node {.discardable.} =
  node.children.add(children)
  result = node

proc hash*(node: Node): Hash =
  var h = Hash(0)
  h = h !& hash(node.kind)
  case node.kind
  of nkEmpty: discard
  of nkBool: h = h !& hash(node.boolVal)
  of nkNumber: h = h !& hash(node.numberVal)
  of nkString: h = h !& hash(node.stringVal)
  of nkIdent: h = h !& hash(node.ident)
  else: h = h !& hash(node.children)
  result = h

proc `$`*(node: Node): string =
  ## Stringify a node. This only supports leaf nodes, for trees,
  ## use ``treeRepr``.
  assert node.kind in LeafNodes, "only leaf nodes can be `$`'ed"
  case node.kind
  of nkEmpty: result = ""
  of nkBool: result = $node.boolVal
  of nkNumber: result = $node.numberVal
  of nkString: result = node.stringVal.escape
  of nkIdent: result = node.ident
  else: discard

proc treeRepr*(node: Node): string =
  ## Stringify a node into a tree representation.
  case node.kind
  of nkEmpty: result = "Empty"
  of nkBool: result = "Bool " & $node.boolVal
  of nkNumber: result = "Number " & $node.numberVal
  of nkString: result = "String " & escape(node.stringVal)
  of nkIdent: result = "Ident " & node.ident
  else:
    result = ($node.kind)[2..^1]
    var children = ""
    for i, child in node.children:
      children.add('\n' & child.treeRepr)
    result.add(children.indent(2))

proc render*(node: Node): string =
  ## Renders the node's AST representation into a string. Note that this is
  ## imperfect and can't fully reproduce the actual user input (this, for
  ## instance, omits parentheses, as they are ignored by the parser).

  proc join(nodes: seq[Node], delimiter: string): string =
    for i, node in nodes:
      result.add(node.render)
      if i != nodes.len - 1:
        result.add(delimiter)

  case node.kind
  of nkEmpty: result = ""
  of nkScript: result = node.children.join("\n")
  of nkBlock:
    result =
      if node.len == 0: "{}"
      else: "{\n" & node.children.join("\n").indent(2) & "\n}"
  of nkIdentDefs:
    result = node[0..^3].join(", ")
    if node[^2].kind != nkEmpty: result.add(": " & node[^2].render)
    if node[^1].kind != nkEmpty: result.add(" = " & node[^1].render)
  of nkFormalParams:
    result = '(' & node[1..^1].join(", ") & ')'
    if node[0].kind != nkEmpty: result.add(" -> " & node[0].render)
  of nkGenericParams: result = '[' & node.children.join(", ") & ']'
  of nkRecFields: result = node.children.join("\n")
  of nkBool, nkNumber, nkString, nkIdent: result = $node
  of nkPrefix: result = node[0].render & node[1].render
  of nkInfix:
    result = node[1].render & ' ' & node[0].render & ' ' & node[2].render
  of nkDot: result = node[0].render & '.' & node[1].render
  of nkColon: result = node[0].render & ": " & node[1].render
  of nkIndex: result = node[0].render & '[' & node[1].render & ']'
  of nkCall: result = node[0].render & '(' & node[1..^1].join(", ") & ')'
  of nkIf:
    result = "if " & node[0].render & ' ' & node[1].render
    let
      hasElse = node.children.len mod 2 == 1
      elifBranches =
        if hasElse: node[2..^2]
        else: node[2..^1]
    for i in countup(0, elifBranches.len - 1, 2):
      result.add(" elif " & elifBranches[i].render & ' ' &
                 elifBranches[i + 1].render)
    if hasElse:
      result.add(" else " & node[^1].render)
  of nkProcTy:
    result = "proc " & node[0].render
  of nkVar, nkLet:
    result =
      (if node.kind == nkVar: "var " else: "let ") & node[0].render
  of nkWhile: result = "while " & node[0].render & ' ' & node[1].render
  of nkFor:
    result = "for " & node[0].render & " in " & node[1].render &
             ' ' & node[2].render
  of nkBreak: result = "break"
  of nkContinue: result = "continue"
  of nkReturn, nkYield:
    result =
      if node.kind == nkReturn: "return"
      else: "yield"
    if node[0].kind != nkEmpty: result.add(' ' & node[0].render)
  of nkObject:
    result = "object " & node[0].render & node[1].render & " {\n" &
             node[2].render.indent(2) & "\n}\n"
  of nkProc, nkIterator:
    result = (if node.kind == nkProc: "proc " else: "iterator ") &
             node[0].render & node[1].render & node[2].render & ' ' &
             node[3].render
    if node[0].kind != nkEmpty:
      result.add('\n')

proc newNode*(kind: NodeKind): Node =
  ## Construct a new node.
  result = Node(kind: kind)

proc newEmpty*(): Node =
  ## Construct a new empty node.
  result = newNode(nkEmpty)

proc newBlock*(children: varargs[Node]): Node =
  ## Construct a new block.
  result = newNode(nkBlock)

proc newTree*(kind: NodeKind, children: varargs[Node]): Node =
  ## Construct a new branch node with the given kind.
  assert kind notin LeafNodes, "kind must denote a branch node"
  result = newNode(kind)
  result.add(children)

proc newBoolLit*(val: bool): Node =
  ## Construct a new bool literal.
  result = newNode(nkBool)
  result.boolVal = val

proc newNumberLit*(val: float): Node =
  ## Construct a new number literal.
  result = newNode(nkNumber)
  result.numberVal = val

proc newStringLit*(val: string): Node =
  ## Construct a new string literal.
  result = newNode(nkString)
  result.stringVal = val

proc newIdent*(ident: string): Node =
  ## Construct a new ident node.
  result = newNode(nkIdent)
  result.ident = ident

proc newIdentDefs*(names: openarray[Node], ty: Node,
                   value = newEmpty()): Node =
  ## Construct a new nkIdentDefs node.
  result = newTree(nkIdentDefs, names)
  result.add([ty, value])
