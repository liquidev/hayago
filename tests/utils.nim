import times

proc measureTime*(name: string, body: proc ()) =
  ## General tine measuring utility
  let start = cpuTime()
  body()
  let duration = cpuTime() - start
  echo name, " took ", duration * 1000, " ms"
