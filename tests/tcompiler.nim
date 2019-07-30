import times
import unittest

import rod/private/scanner
import rod/private/parser
import rod/private/chunk
import rod/private/compiler
import rod/private/disassembler

template benchmark(name, body) =
  let t0 = epochTime()
  body
  echo name, " took ", (epochTime() - t0) * 1000, "ms"

template compile(parser, compiler: untyped, input: string) =
  benchmark("compilation"):
    var scanner = initScanner(input, "testcase.rod")
    let ast = parser(scanner)
    var
      chunk = initChunk()
      cp = initCompiler()
    cp.compiler(chunk, ast)
  echo chunk.disassemble()

suite "compiler":
  test "variables":
    compile(parseStmt, compileStmt, "var x = 2 + 2")
