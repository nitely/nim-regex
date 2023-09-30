import nimbench
import unicode
from re import nil
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

var pattern2 = re.re"^\w*sol\w*$"

bench(re_sol, m):
  var d: bool
  for i in 0 ..< m:
    d = re.match(text, pattern2)
  doNotOptimizeAway(d)

const pattern4 = regex.re2(r"\w*sol\w*") #, {regex.RegexFlag.reAscii})

benchRelative(regex_sol, m):
  var m2: regex.RegexMatch2
  for i in 0 ..< m:
    discard regex.match(text, pattern4, m2)
  doNotOptimizeAway(m2)

benchRelative(regex_macro_sol, m):
  var d: bool
  for i in 0 ..< m:
    regex.match text, regex.rex"\w*sol\w*":
      d = true
  doNotOptimizeAway(d)

var dummyTextNums = """650-253-0001"""

var pattern_nums = re.re"^[0-9]+-[0-9]+-[0-9]+$"

bench(re_nums, m):
  var d: bool
  for i in 0 ..< m:
    d = re.match(dummyTextNums, pattern_nums)
  doNotOptimizeAway(d)

const n_pattern_nums = regex.re2"[0-9]+-[0-9]+-[0-9]+"

benchRelative(regex_nums, m):
  var m2: regex.RegexMatch2
  for i in 0 ..< m:
    discard regex.match(dummyTextNums, n_pattern_nums, m2)
  doNotOptimizeAway(m2)

benchRelative(regex_macro_nums, m):
  var d: bool
  for i in 0 ..< m:
    regex.match text, regex.rex"[0-9]+-[0-9]+-[0-9]+":
      d = true
  doNotOptimizeAway(d)

var pattern_nums2 = re.re"^[0-9]+..*$"

bench(re_nums2, m):
  var d: bool
  for i in 0 ..< m:
    d = re.match(dummyTextNums, pattern_nums2)
  doNotOptimizeAway(d)

const n_pattern_nums2 = regex.re2"[0-9]+..*"

benchRelative(regex_nums2, m):
  var m3: regex.RegexMatch2
  for i in 0 ..< m:
    discard regex.match(dummyTextNums, n_pattern_nums2, m3)
  doNotOptimizeAway(m3)

benchRelative(regex_macro_nums2, m):
  var d: bool
  for i in 0 ..< m:
    regex.match text, regex.rex"[0-9]+..*":
      d = true
  doNotOptimizeAway(d)

when false:  # XXX remove
  var lits_find_re = re.re"do|re|mi|fa|sol"

  bench(re_lits_find, m):
    var d: int
    for i in 0 ..< m:
      d = re.find(text, lits_find_re)
    doNotOptimizeAway(d)

  const lits_find = regex.re2"do|re|mi|fa|sol"

  benchRelative(regex_lits_find, m):
    var m2: regex.RegexMatch2
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

const email_find_all = regex.re2"[\w\.+-]+@[\w\.-]+\.[\w\.-]+"

benchRelative(regex_email_find_all, m):
  var d = 0
  for i in 0 ..< m:
    for _ in regex.findAll(bench_text, email_find_all):
      d += 1
  doAssert d == 92
  doNotOptimizeAway(d)

var uri_find_all_re = re.re"[\w]+://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?"

bench(re_uri_find_all, m):
  var d = 0
  for i in 0 ..< m:
    for _ in re.findAll(bench_text, uri_find_all_re):
      d += 1
  doAssert d == 5301
  doNotOptimizeAway(d)

const uri_find_all = regex.re2"[\w]+://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?"

benchRelative(regex_uri_find_all, m):
  var d = 0
  for i in 0 ..< m:
    for _ in regex.findAll(bench_text, uri_find_all):
      d += 1
  doAssert d == 5301
  doNotOptimizeAway(d)

var ip_find_all_re = re.re"(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])"

bench(re_ip_find_all, m):
  var d = 0
  for i in 0 ..< m:
    for _ in re.findAll(bench_text, ip_find_all_re):
      d += 1
  doAssert d == 5
  doNotOptimizeAway(d)

const ip_find_all = regex.re2"(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])"

benchRelative(regex_ip_find_all, m):
  var d = 0
  for i in 0 ..< m:
    for _ in regex.findAll(bench_text, ip_find_all):
      d += 1
  doAssert d == 5
  doNotOptimizeAway(d)

var url_find_all_re = re.re"https?://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?"

bench(re_url_find_all, m):
  var d = 0
  for i in 0 ..< m:
    for _ in re.findAll(bench_text, url_find_all_re):
      d += 1
  doAssert d == 5295
  doNotOptimizeAway(d)

const url_find_all = regex.re2"https?://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?"

benchRelative(regex_url_find_all, m):
  var d = 0
  for i in 0 ..< m:
    for _ in regex.findAll(bench_text, url_find_all):
      d += 1
  doAssert d == 5295
  doNotOptimizeAway(d)

when true:
  bench(runes, m):
    var d = 0
    for i in 0 ..< m:
      for _ in bench_text.runes:
        d += 1
    doNotOptimizeAway(d)

bench(dummy, m):
  for i in 0 ..< m:
    memoryClobber()

when isMainModule:
  runBenchmarks()

#[
# Profiling:
# (but extract the bench to another module without nimbench)
# open the log with KCachegrind

$ nim c --debugger:native --threads:off -d:danger -d:useMalloc -o:bin/bench2 bench/bench2.nim && valgrind --tool=callgrind -v ./bin/bench2
]#
