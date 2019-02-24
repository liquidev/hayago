import unittest

import ../src/rod/scanner

suite "scanner":
  test "expect()":
    var
      scan = newScanner("null false true 123 \"Hello\"")
      tnull, tfalse, ttrue, tnum, tstr: RodToken
    assert scan.expect(tnull, rtkNull)
    assert scan.expect(tfalse, rtkBool)
    assert scan.expect(ttrue, rtkBool)
    assert scan.expect(tnum, rtkNum)
    assert scan.expect(tstr, rtkStr)
