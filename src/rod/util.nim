import terminal

proc warn*(warning: string) =
  styledEcho(styleBright, fgYellow,
             "warning: ",
             resetStyle,
             warning)
