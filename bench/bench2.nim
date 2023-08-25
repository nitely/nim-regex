import unicode
from regex import nil

func genText(): string {.compileTime.} =
  result = ""
  for _ in 0 .. 100000:
    result.add("a")
  result.add("sol")
  for _ in 0 .. 100000:
    result.add("b")
  #result.add("ฅ")
const text = genText()

const pattern4 = regex.re2(r"\w*sol\w*") #, {regex.RegexFlag.reAscii})

proc runBenchmarks() =
  var m2: regex.RegexMatch2
  for i in 0 ..< 500:
    discard regex.match(text, pattern4, m2)
  echo m2.captures

when isMainModule:
  runBenchmarks()