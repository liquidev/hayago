#--
# the rod programming language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

## The VM is the hot core of rod, used for code execution.
## This VM is implemented *in the C way*, and **that's intentional**.
## It intentionally uses very unsafe and low-level practices to provide a
## reasonable execution speed.

import strutils
import tables

import chunk
import value

type
  Vm* = ref object
    globals*: Table[string, Value]
  CallFrame = tuple
    chunk: Chunk
    pc: ptr UncheckedArray[uint8]
    stackBottom: int

proc `{}`[T](x: seq[T], i: int): ptr UncheckedArray[T] =
  ## Return an unsafe view into a seq.
  result = cast[ptr UncheckedArray[T]](x[i].unsafeAddr)

proc `{}`[T](x: seq[T], i: BackwardsIndex): ptr UncheckedArray[T] =
  result = x{x.len - i.int}

proc read[T](code: ptr UncheckedArray[uint8], offset: int): T =
  ## Read a value of type ``T`` at ``offset`` in ``code``.
  result = cast[ptr T](code[offset].unsafeAddr)[]

proc push(stack: var Stack, val: Value) =
  ## Pushes ``val`` onto the stack.
  stack.add(val)
  when defined(rodVmWriteStackOps):
    echo "push ", stack

proc pop(stack: var Stack): Value =
  ## Pops a value off the stack.
  result = stack[^1]
  stack.setLen(stack.len - 1)
  when defined(rodVmWriteStackOps):
    echo "pop  ", stack

proc peek(stack: Stack): Value =
  ## Peek the value at the top of the stack.
  result = stack[^1]

when defined(c) or defined(cpp) or defined(objc):
  {.pragma: vmvar, codegenDecl: "register $# $#".}

  template inc[T](point: ptr UncheckedArray[T], offset = 1) =
    {.emit: [point, "+=", offset, ";"].}

  template dec[T](point: ptr UncheckedArray[T], offset = 1) =
    {.emit: [point, "-=", offset, ";"].}
else:
  {.pragma: vmvar.}

  template inc[T](point: ptr UncheckedArray[T], offset = 1) =
    point = cast[ptr UncheckedArray[T]](cast[int](point) + offset)

  template dec[T](point: ptr UncheckedArray[T], offset = 1) =
    point = cast[ptr UncheckedArray[T]](cast[int](point) - offset)

proc interpret*(vm: Vm, script: Script, startChunk: Chunk): Value =
  ## Interpret a chunk of code.

  # VM state
  var
    stack: Stack
    callStack: seq[CallFrame]
    chunk {.vmvar.} = startChunk
    pc {.vmvar.} = chunk.code{0}
    stackBottom = 0

  # interpret loop
  while true:
    {.computedGoto.}

    let opcode = pc[0].Opcode
    when defined(rodVmWritePcFlow):
      let relPc = cast[int](pc) - cast[int](chunk.code{0})
      echo "pc: ", toHex(relPc.BiggestInt, 8), " - ", opcode

    inc(pc)

    template unary(expr) =
      let a {.inject.} = stack.pop()
      stack.push(initValue(expr))
    template binary(expr) =
      let
        b {.inject.} = stack.pop()
        a {.inject.} = stack.pop()
      stack.push(initValue(expr))

    template storeFrame() =
      callStack.add((chunk: chunk,
                     pc: pc,
                     stackBottom: stackBottom))
    template restoreFrame() =
      # discard locals from current frame
      stack.setLen(stackBottom)
      # restore the frame
      let frame = callStack.pop()
      chunk = frame.chunk
      pc = frame.pc
      stackBottom = frame.stackBottom

    template doCall(theProc: Proc) =
      storeFrame()
      stackBottom = stack.len - theProc.paramCount
      case theProc.kind
      of pkNative:
        chunk = theProc.chunk
        pc = chunk.code{0}
        # the frame is restored by the return(Void|Val) opcode in the proc
      of pkForeign:
        let callResult = theProc.foreign(stack{^theProc.paramCount})
        restoreFrame()
        if theProc.hasResult:
          stack.push(callResult)

    case opcode
    of opcNoop: discard

    #--
    # Stack
    #--

    # literals
    of opcPushTrue:
      stack.push(initValue(true))
    of opcPushFalse:
      stack.push(initValue(false))
    of opcPushNil:
      let id = pc.read[:uint16](0).TypeId
      stack.push(initObject(id, nilObject))
      inc(pc, sizeof(uint16))
    of opcPushN: # push number
      let num = pc.read[:float](0)
      stack.push(initValue(num))
      inc(pc, sizeof(float))
    of opcPushS: # push string
      let id = pc.read[:uint16](0)
      stack.push(initValue(chunk.strings[id]))
      inc(pc, sizeof(uint16))

    # variables
    of opcPushG: # push global
      let
        id = pc.read[:uint16](0)
        name = chunk.strings[id]
      stack.push(vm.globals[name])
      inc(pc, sizeof(uint16))
    of opcPopG: # pop to global
      let
        id = pc.read[:uint16](0)
        name = chunk.strings[id]
      vm.globals[name] = stack.pop()
      inc(pc, sizeof(uint16))
    of opcPushL: # push local
      stack.push(stack[stackBottom + pc[0].int])
    of opcPopL: # pop to local
      stack[stackBottom + pc[0].int] = stack.pop()

    # objects
    of opcConstrObj: # construct object
      let
        id = pc.read[:uint16](0)
        fieldCount = pc[sizeof(uint16)].int
        fields = stack{^fieldCount}
      var obj = initObject(id, fieldCount)
      for i in 0..<fieldCount:
        obj.objectVal.fields[i] = fields[i]
      stack.setLen(stack.len - fieldCount)
      stack.push(obj)
      inc(pc, sizeof(uint16) + sizeof(uint8))
    of opcPushF: # push field
      let
        field = pc[0]
        obj = stack.pop()
      stack.push(obj.objectVal.fields[field])
      inc(pc, sizeof(uint8))
    of opcPopF: # pop to field
      let
        field = pc[0]
        value = stack.pop()
        obj = stack.pop()
      obj.objectVal.fields[field] = value
      inc(pc, sizeof(uint8))

    # other
    of opcDiscard:
      let n = pc[0].int
      stack.setLen(stack.len - n)
      inc(pc, sizeof(uint8))

    #--
    # Arithmetic
    #--

    of opcNegN: # negate a number
      unary(-a.numberVal)
    of opcAddN: # add two numbers
      binary(a.numberVal + b.numberVal)
    of opcSubN: # subtract two numbers
      binary(a.numberVal - b.numberVal)
    of opcMultN: # multiply two numbers
      binary(a.numberVal * b.numberVal)
    of opcDivN: # divide two numbers
      binary(a.numberVal / b.numberVal)

    #--
    # Logic
    #--

    of opcInvB: # negate a bool
      unary(not a.boolVal)

    #--
    # Relational
    #--

    of opcEqB: # bools equal
      binary(a.boolVal == b.boolVal)
    of opcEqN: # numbers equal
      binary(a.numberVal == b.numberVal)
    of opcLessN: # number less than a number
      binary(a.numberVal < b.numberVal)
    of opcGreaterN: # number less than or equal to number
      binary(a.numberVal > b.numberVal)

    #--
    # Execution
    #--

    of opcJumpFwd: # jump forward
      let n = pc.read[:uint16](0).int
      # jump n - 1, because we already advanced 1 after reading the opcode
      inc(pc, n - 1)
    of opcJumpFwdT: # jump forward if true
      if stack.peek().boolVal:
        let n = pc.read[:uint16](0).int
        inc(pc, n - 1)
      else:
        inc(pc, sizeof(uint16))
    of opcJumpFwdF: # jump forward if false
      if not stack.peek().boolVal:
        let n = pc.read[:uint16](0).int
        inc(pc, n - 1)
      else:
        inc(pc, sizeof(uint16))
    of opcJumpBack: # jump back
      let n = pc.read[:uint16](0).int
      dec(pc, n + 1)
    of opcCallD: # direct call
      let
        id = pc.read[:uint16](0).int
        theProc = script.procs[id]
      doCall(theProc)
      inc(pc, sizeof(uint16))
    of opcCallR: discard
    of opcReturnVal:
      let retVal = stack.pop()
      restoreFrame()
      stack.push(retVal)
    of opcReturnVoid:
      restoreFrame()
    of opcHalt: break

proc newVm*(): Vm =
  ## Create a new VM.
  result = Vm()

