#~~
# the rod programming language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#~~

type
  RodOpcode* = enum
    #~ variables and values
    roNewGlobal = "new_global" ## \
      ## Creates a new global. This is used for checking the existence of
      ## globals.
    roPushConst = "push_const" ## \
      ## Pushes a constant onto the stack.
    roPushGlobal = "push_global" ## \
      ## Pushes a global onto the stack.
    roPushLocal = "push_local" ## \
      ## Pushes a local onto the stack.
    roPushField = "push_field" ## \
      ## Pushes an object's field onto the stack.
    roDiscard = "discard" ## \
      ## Pops a value off the stack and discards it.
    roPopGlobal = "pop_global" ## \
      ## Pops a value off the stack and stores it in a global.
    roPopLocal = "pop_local" ## \
      ## Pops a local off the stack and stores it in a local, resizing the \
      ## local storage as needed.
    roPopField = "pop_field" ## \
      ## Pops a value off the stack to an object's field.
    roStoreGlobal = "store_global" ## \
      ## Peeks at the value at the top of the stack and stores it in a global.
    roStoreLocal = "store_local" ## \
      ## Peeks at the value at the top of the stack and stores it in a local.
    roStoreField = "store_field" ## \
      ## Peeks at the value at the top of the stack and stores it on a field.
    roIntoBool = "into_bool" ## \
      ## Converts the value at the top of the stack into its boolean \
      ## representation.

    #~ functions and methods
    roPushMethod = "push_method" ## \
      ## Peeks at the value at the top of the stack, and pushes its class's \
      ## method onto the stack.
    roCallFn = "call_fn" ## \
      ## Pops a function off the stack, then pops ``n`` paramers, and calls \
      ## that function.
    roCallMethod = "call_method" ## \
      ## Pops ``n`` parameters off the stack, then the method, and then the \
      ## method's receiver. After that, it calls the popped method.

    #~ flow control
    roJump = "jump" ## \
      ## Jumps to an address in the bytecode.
    roJumpCond = "jump_cond" ## \
      ## Jumps to an address in the bytecode conditionally—if a value popped \
      ## off the stack is truthy, it jumps, if not, it continues to the next \
      ## instruction.
    roJumpShort = "jump_short" ## \
      ## Similar to ``roJumpCond``. The `short` stands for short-circuit, as \
      ## this opcode is used in short-circuiting operators ``||`` and ``&&``. \
      ## The difference between ``roJumpCond`` and this is, that this opcode \
      ## doesn't pop a value off the stack, while ``roJumpCond`` does.
    roReturn = "return" ## \
      ## Finishes the current call.
