#--
# the hayago programming language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

## The VM is the hot core of hayago, used for code execution.
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
  when defined(hayaVmWriteStackOps):
    echo "push ", stack

proc pop(stack: var Stack): Value =
  ## Pops a value off the stack.
  result = stack[^1]
  stack.setLen(stack.len - 1)
  when defined(hayaVmWriteStackOps):
    echo "pop  ", stack

proc peek(stack: Stack): Value =
  ## Peek the value at the top of the stack.
  result = stack[^1]

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
    chunk  = startChunk
    pc = chunk.code{0}
    stackBottom = 0

  # interpret loop
  while true:
    {.computedGoto.}

    let opcode = pc[0].Opcode
    when defined(hayaVmWritePcFlow):
      template relPc: int = cast[int](pc) - cast[int](chunk.code{0})
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
    template binaryInpl(someVal, op) =
      let b = stack.pop()
      op(stack[^1].someVal, b.someVal)

    template storeFrame() =
      when defined(hayaVmWritePcFlow):
        echo "storing frame"
      callStack.add((chunk: chunk,
                     pc: pc,
                     stackBottom: stackBottom))
    template restoreFrame() =
      when defined(hayaVmWritePcFlow):
        echo "restoring frame"
      # discard locals from current frame
      stack.setLen(stackBottom)
      # restore the frame
      let frame = callStack.pop()
      chunk = frame.chunk
      pc = frame.pc
      stackBottom = frame.stackBottom

    template doCall(theProc: Proc) =
      when defined(hayaVmWritePcFlow):
        echo "entering procedure " & theProc.name
      storeFrame()
      stackBottom = stack.len - theProc.paramCount
      case theProc.kind
      of pkNative:
        chunk = theProc.chunk
        pc = chunk.code{0}
        when defined(hayaVmWritePcFlow):
          echo "native proc; pc is now ", toHex(relPc.BiggestInt, 8)
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
    of opcPushI:  # push int
      let i = pc.read[:int64](0)
      stack.push(initValue(i))
      inc(pc, sizeof(float))
    of opcPushF:  # push float
      let f = pc.read[:float64](0)
      stack.push(initValue(f))
      inc(pc, sizeof(float))
    of opcPushS:  # push string
      let id = pc.read[:uint16](0)
      stack.push(initValue(chunk.strings[id]))
      inc(pc, sizeof(uint16))

    # variables
    of opcPushG:  # push global
      let
        id = pc.read[:uint16](0)
        name = chunk.strings[id]
      stack.push(vm.globals[name])
      inc(pc, sizeof(uint16))
    of opcPopG:  # pop to global
      let
        id = pc.read[:uint16](0)
        name = chunk.strings[id]
      vm.globals[name] = stack.pop()
      inc(pc, sizeof(uint16))
    of opcPushL:  # push local
      stack.push(stack[stackBottom + pc[0].int])
      inc(pc, sizeof(uint8))
    of opcPopL:  # pop to local
      stack[stackBottom + pc[0].int] = stack.pop()
      inc(pc, sizeof(uint8))

    # objects
    of opcConstrObj:  # construct object
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
    of opcGetF:  # push field
      let
        field = pc[0]
        obj = stack.pop()
      stack.push(obj.objectVal.fields[field])
      inc(pc, sizeof(uint8))
    of opcSetF:  # pop to field
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

    of opcNegI:  # negate a int
      unary(-a.intVal)
    of opcAddI:  # add two ints
      binaryInpl(intVal, `+=`)
    of opcSubI:  # subtract two ints
      binaryInpl(intVal, `-=`)
    of opcMultI:  # multiply two ints
      binaryInpl(intVal, `*=`)
    of opcDivI:  # divide two ints
      let
        b = stack.pop()
        a = stack.pop()
      stack.add(initValue(a.intVal div b.intVal))
    of opcNegF:  # negate a float
      unary(-a.floatVal)
    of opcAddF:  # add two floats
      binaryInpl(floatVal, `+=`)
    of opcSubF:  # subtract two floats
      binaryInpl(floatVal, `-=`)
    of opcMultF:  # multiply two floats
      binaryInpl(floatVal, `*=`)
    of opcDivF:  # divide two floats
      binaryInpl(floatVal, `/=`)

    #--
    # Logic
    #--

    of opcInvB:  # negate a bool
      unary(not a.boolVal)

    #--
    # Relational
    #--

    of opcEqB:  # bools equal
      binary(a.boolVal == b.boolVal)
    of opcEqI:  # ints equal
      binary(a.intVal == b.intVal)
    of opcLessI:  # int less than a int
      binary(a.intVal < b.intVal)
    of opcGreaterI:  # int less than or equal to int
      binary(a.intVal > b.intVal)
    of opcEqF:  # floats equal
      binary(a.floatVal == b.floatVal)
    of opcLessF:  # float less than a float
      binary(a.floatVal < b.floatVal)
    of opcGreaterF:  # float less than or equal to float
      binary(a.floatVal > b.floatVal)

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
      inc(pc, sizeof(uint16))
      doCall(theProc)
    of opcCallI: discard
    of opcReturnVal:
      let retVal = stack.pop()
      restoreFrame()
      stack.push(retVal)
    of opcReturnVoid:
      restoreFrame()
    of opcHalt:
      assert stack.len == 0,
        "stack was not completely emptied. remaining values: " & $stack
      break

proc newVm*(): Vm =
  ## Create a new VM.
  result = Vm()

