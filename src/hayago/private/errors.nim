#--
# the hayago scripting language
# copyright (C) iLiquid, 2019-2020
# licensed under the MIT license
#--

## This module contains various error messages used in the code generator.

const
  ErrorFmt* = "$1($2, $3): $4"

  # lex errors
  ErrUnknownOperator* = "unknown operator: '$1'"
  ErrUntermStringLit* = "unterminated string literal"
  ErrUntermStroppedIdent* = "unterminated stropped identifier"
  ErrUnexpectedChar* = "unexpected character: '$1'"
  ErrXExpectedGotY* = "$1 expected, got $2"
  ErrOpExpectedGotY* = "'$1' expected, got $2"

  # parse errors
  ErrUnmatchedParen* = "right paren ')' expected"
  ErrGenericIllegalInProcType* = "generic params are not allowed in proc types"
  ErrAssignOpExpected* = "assignment operator '=' expected, got '$1'"
  ErrMissingToken* = "missing '$1'"
  ErrXExpected* = [
    1: "'$#' expected",
    2: "'$#' or '$#' expected",
  ]
  ErrProcNameExpected* = "proc name expected"
  ErrProcParamsExpected* = "proc params expected"

  # compile errors
  ErrShadowResult* = "variable shadows implicit 'result' variable"
  ErrLocalRedeclaration* = "'$1' is already declared in this scope"
  ErrGlobalRedeclaration* = "'$1' is already declared"
  ErrUndefinedReference* = "'$1' is not declared in the current scope"
  ErrLetReassignment* =
    "'$1' cannot be reassigned because it's a 'let' variable"
  ErrTypeMismatch* = "type mismatch: got <$1>, but expected <$2>"
  ErrTypeMismatchChoice* = "type mismatch: got <$1>, but expected one of:$2"
  ErrNotAProc* = "'$1' is not a procedure"
  ErrInvalidField* = "'$1' is not a valid field"
  ErrNonExistentField* = "field '$1' does not exist for <$2>"
  ErrInvalidAssignment* = "cannot assign to '$1'"
  ErrTypeIsNotAnObject* = "'$1' is not an object type"
  ErrObjectFieldsMustBeInitialized* = "all object fields must be initialized"
  ErrFieldInitMustBeAColonExpr* =
    "field initializer must be a colon expression: 'a: b'"
  ErrValueIsVoid* = "value does not have a valid type (its type is <void>)"
  ErrOnlyUsableInABlock* =
    "'$1' can only be used inside a loop or a block statement"
  ErrOnlyUsableInALoop* = "'$1' can only be used inside a loop"
  ErrOnlyUsableInAProc* = "'$1' can only be used inside a proc"
  ErrOnlyUsableInAnIterator* = "'$1' can only be used inside an iterator"
  ErrVarMustHaveValue* = "variable must have a value"
  ErrIterMustHaveYieldType* = "iterator must have a non-void yield type"
  ErrSymKindMismatch* = "$1 expected, but got $2"
  ErrInvalidSymName* = "'$1' is not a valid symbol name"
  ErrCouldNotInferGeneric* =
    "could not infer generic params for '$1'. specify them explicitly"
  ErrNotGeneric* = "'$1' is not generic"
  ErrGenericArgLenMismatch* = "got $1 generic arguments, but expected $2"
