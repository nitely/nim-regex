import nimbench
import unicode
from re import nil
from regex import nil

var text = ""
for _ in 0 .. 100000:
  text.add("a")
text.add("sol")
for _ in 0 .. 100000:
  text.add("b")
#text.add("ฅ")

var pattern2 = re.re"^\w*sol\w*$"

bench(re_sol, m):
  var d: bool
  for i in 0 ..< m:
    d = re.match(text, pattern2)
  doNotOptimizeAway(d)

const pattern4 = regex.re(r"\w*sol\w*") #, {regex.RegexFlag.reAscii})

benchRelative(regex_sol, m):
  var m2: regex.RegexMatch
  for i in 0 ..< m:
    discard regex.match(text, pattern4, m2)
  doNotOptimizeAway(m2)

var dummyTextNums = """650-253-0001"""

var pattern_nums = re.re"^[0-9]+-[0-9]+-[0-9]+$"

bench(re_nums, m):
  var d: bool
  for i in 0 ..< m:
    d = re.match(dummyTextNums, pattern_nums)
  doNotOptimizeAway(d)

const n_pattern_nums = regex.re"[0-9]+-[0-9]+-[0-9]+"

benchRelative(nregex_nums, m):
  var m2: regex.RegexMatch
  for i in 0 ..< m:
    discard regex.match(dummyTextNums, n_pattern_nums, m2)
  doNotOptimizeAway(m2)

var pattern_nums2 = re.re"^[0-9]+..*$"

bench(re_nums2, m):
  var d: bool
  for i in 0 ..< m:
    d = re.match(dummyTextNums, pattern_nums2)
  doNotOptimizeAway(d)

const n_pattern_nums2 = regex.re"[0-9]+..*"

benchRelative(nregex_nums2, m):
  var m3: regex.RegexMatch
  for i in 0 ..< m:
    discard regex.match(dummyTextNums, n_pattern_nums2, m3)
  doNotOptimizeAway(m3)

var lits_find_re = re.re"do|re|mi|fa|sol"

bench(re_doremifasol_find, m):
  var d: int
  for i in 0 ..< m:
    d = re.find(text, lits_find_re)
  doNotOptimizeAway(d)

const lits_find = regex.re"do|re|mi|fa|sol"

benchRelative(regex_doremifasol_find, m):
  var m2: regex.RegexMatch
  for i in 0 ..< m:
    discard regex.find(text, lits_find, m2)
  doNotOptimizeAway(m2)

const bench_text = staticRead("input-text.txt")

var email_find_all_re = re.re"[\w\.+-]+@[\w\.-]+\.[\w\.-]+"

bench(re_email_find_all, m):
  var d = 0
  for i in 0 ..< m:
    for _ in re.findAll(bench_text, email_find_all_re):
      d += 1
  doAssert d == 92
  doNotOptimizeAway(d)

const email_find_all = regex.re"[\w\.+-]+@[\w\.-]+\.[\w\.-]+"

benchRelative(email_find_all, m):
  var d = 0
  for i in 0 ..< m:
    for _ in regex.findAll(bench_text, email_find_all):
      d += 1
  doAssert d == 92
  doNotOptimizeAway(d)

when false:
  bench(runes, m):
    for i in text.runes:
      memoryClobber()

bench(dummy, m):
  for i in 0 ..< m:
    memoryClobber()

when isMainModule:
  runBenchmarks()
