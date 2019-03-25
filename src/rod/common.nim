#~~
# the rod programming language
# copyright (C) iLiquid, 2019
# licensed under the MIT license
#~~

## This module contains some common functions used throughout the languege,
## mainly for debugging or default settings.

import terminal

proc echoErr*(msg: varargs[string, `$`]) =
  var fullMsg = ""
  for m in msg: fullMsg.add(m)
  styledEcho(
    styleBright, fgRed, "rod/err: ",
    resetStyle, fullMsg
  )

proc echoWarn*(msg: varargs[string, `$`]) =
  var fullMsg = ""
  for m in msg: fullMsg.add(m)
  styledEcho(
    styleBright, fgYellow, "rod/warn: ",
    resetStyle, fullMsg
  )

proc echoInfo*(msg: varargs[string, `$`]) =
  var fullMsg = ""
  for m in msg: fullMsg.add(m)
  styledEcho(
    styleBright, fgBlue, "rod/info: ",
    resetStyle, fullMsg
  )

proc echoDebug*(msg: varargs[string, `$`]) =
  var fullMsg = ""
  for m in msg: fullMsg.add(m)
  styledEcho(
    styleBright, styleDim, fgDefault, "rod/dbg: ",
    resetStyle, fullMsg
  )
