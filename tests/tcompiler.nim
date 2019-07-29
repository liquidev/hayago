import unittest

import rod/private/scanner
import rod/private/parser
import rod/private/chunk
import rod/private/compiler
import rod/private/disassembler

template valCompile(parser, compiler: untyped, input: string) =
  var scanner = initScanner(input, "testcase.rod")
  let ast = parser(scanner)
  var
    chunk = initChunk()
    cp = initCompiler()
    dest = cp.alloc()
  discard cp.compiler(chunk, ast, dest)
  echo chunk.disassemble()

suite "compiler":
  test "basic prefix":
    valCompile(parseExpr, compileValue, "2 * 2 + 2")
