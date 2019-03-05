import times

proc measureTime*(name: string, body: proc ()) =
  ## General tine measuring utility
  let start = now()
  body()
  let finish = now() - start
  echo name, " took ", float(finish.inNanoseconds) / 1000000.0, " ms"
