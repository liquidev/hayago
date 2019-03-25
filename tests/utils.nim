import times

proc measureTime*(name: string, body: proc ()) =
  ## General tine measuring utility
  let start = epochTime()
  body()
  let duration = epochTime() - start
  echo name, " took ", duration * 1000, " ms"
