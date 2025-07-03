from ../src/regex import nil

const bench_text = staticRead("input-text.txt")
const email_find_all = regex.re2"[\w\.+-]+@[\w\.-]+\.[\w\.-]+"

proc main =
  var d = 0
  for i in 0 ..< 1000:
    for _ in regex.findAll(bench_text, email_find_all):
      d += 1
  doAssert d == 92

main()
