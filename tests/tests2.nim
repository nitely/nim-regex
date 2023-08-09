from std/unicode import runeLen
from std/sequtils import map

import ./regex

const nonCapture = -1 .. -2

template test(desc: string, body: untyped): untyped =
  when defined(runTestAtCT):
    static:
      (proc() =
        echo "[CT] " & desc
        body)()
  else:
    (proc() =
      when defined(forceRegexAtRuntime):
        echo "[RT] " & desc
      else:
        echo "[CT/RT] " & desc
      body)()

template check(condition: bool) =
  doAssert(condition)

template expect(exception: typedesc, body: untyped): untyped =
  doAssertRaises(exception):
    body

when defined(forceRegexAtRuntime):
  proc isMatch(s: string, pattern: Regex2): bool =
    var m: RegexMatch2
    result = match(s, pattern, m)
else:
  proc isMatch(s: string, pattern: static Regex2): bool =
    var m: RegexMatch2
    result = match(s, pattern, m)

proc raises(pattern: string): bool =
  result = false
  try:
    discard pattern.re2()
  except RegexError:
    result = true

proc raisesMsg(pattern: string): string =
  try:
    discard pattern.re2()
  except RegexError:
    result = getCurrentExceptionMsg()

proc matchWithCapt(s: string, pattern: static Regex2): seq[string] =
  var m: RegexMatch2
  check match(s, pattern, m)
  result.setLen m.captures.len
  for i, bounds in m.captures.pairs:
    result[i] = s[bounds]

proc matchWithBounds(s: string, pattern: static Regex2): seq[Slice[int]] =
  var m: RegexMatch2
  check match(s, pattern, m)
  return m.captures

proc toStrCaptures(m: RegexMatch2, s: string): seq[string] =
  result.setLen m.captures.len
  for i, bounds in m.captures.pairs:
    result[i] = s[bounds]

proc findWithCapt(s: string, pattern: Regex2): seq[string] =
  var m: RegexMatch2
  check find(s, pattern, m)
  result = m.toStrCaptures(s)

func findAllCapt(s: string, reg: Regex2): seq[seq[Slice[int]]] =
  result = map(
    findAll(s, reg),
    func (m: RegexMatch2): seq[Slice[int]] =
      for i in 0 .. m.groupsCount-1:
        result.add m.group(i))

func findAllStr(s: string, reg: Regex2): seq[string] =
  result = newSeq[string]()
  for bounds in findAllBounds(s, reg):
    result.add s[bounds]

func group(
  m: RegexMatch2, i: int, text: string
): string {.inline, raises: [].} =
  text[m.group(i)]

func group(
  m: RegexMatch2,
  groupName: string,
  text: string
): string {.inline, raises: [KeyError].} =
  text[m.group(groupName)]

test "tfull_match":
  check "".isMatch(re2"")
  check "a".isMatch(re2"a")
  check "ab".isMatch(re2"(a)b")
  check "aa".isMatch(re2"(a)*")
  check "aab".isMatch(re2"((a)*b)")
  check "abbbbccccd".isMatch(re2"a(b|c)*d")
  check "abbb".isMatch(re2"((a)*(b)*)")
  check "abbb".isMatch(re2"((a(b)*)*(b)*)")
  check "a".isMatch(re2"a|b")
  check "b".isMatch(re2"a|b")
  check(not "ab".isMatch(re2"a(b|c)*d"))
  check(not "a".isMatch(re2"b"))
  check(not "a".isMatch(re2""))
  # raw string need double "" instead of \" to escape,
  # this is a Nim thing
  check " \"word\" ".isMatch(re2"\s"".*""\s")

test "trepetition_cycle":
  check "**".isMatch(re2"\**")
  check "++".isMatch(re2"\++")
  check "??".isMatch(re2"\?+")
  check "??".isMatch(re2"\?*")
  check "?".isMatch(re2"\??")
  check "?".isMatch(re2"\???")
  check "**".isMatch(re2"\**?")
  check "++".isMatch(re2"\++?")
  check "??".isMatch(re2"\?+?")
  check "??".isMatch(re2"\?*?")
  check raises(r"a**")
  check raises(r"a++")
  check raises(r"a*+")
  check raises(r"a+*")
  check raises(r"a?+")
  check raises(r"a?*")
  check raises(r"a??+")
  check raises(r"a??*")
  check raises(r"a*??")
  check raises(r"a+??")
  check raises(r"a???")
  check raises(r"a{1,}*")
  check raises(r"a{1}*")
  check raises(r"a{1,}+")
  check raises(r"a{1}+")
  check raises(r"a{1,}??")
  check raises(r"a{1}??")
  check(not raises(r"a{1,}?"))
  check(not raises(r"a{1}?"))
  check "aaa".isMatch(re2"(a*)*")
  check "aaabbbaaa".isMatch(re2"((a*|b*))*")
  check raises(r"a*****")
  check raises(r"a*{,}")
  check "aaa".isMatch(re2"(a?)*")
  check "aaaa".isMatch(re2"((a)*(a)*)*")
  # Same as PCRE "^(a*)*?$"
  check "aa".matchWithCapt(re2"(a*)*?") == @["aa"]
  check "aa".matchWithCapt(re2"(a*)*?(a*)*?") == @["", "aa"]
  check "".matchWithBounds(re2"(a*)*?(a*)*?") == @[nonCapture, nonCapture]
  check "".matchWithCapt(re2"(a*)*?(a*)*?") == @["", ""]
  check "aa".matchWithCapt(re2"(.*?)") == @["aa"]
  check "aa".matchWithBounds(re2"(.*)*") == @[2 .. 1]
  check "aa".matchWithCapt(re2"(.*)*") == @[""]
  check "a".matchWithBounds(re2"(a*)*") == @[1 .. 0]
  check "a".matchWithCapt(re2"(a*)*") == @[""]
  check "a".matchWithBounds(re2"(a*)*(a*)*") == @[1 .. 0, 1 .. 0]
  check "a".matchWithCapt(re2"(a*)*(a*)*") == @["", ""]
  check "".matchWithBounds(re2"(a*)*") == @[0 .. -1]
  check "".matchWithCapt(re2"(a*)*") == @[""]
  check "".matchWithBounds(re2"(a*)*(a*)*") == @[0 .. -1, 0 .. -1]
  check "".matchWithCapt(re2"(a*)*(a*)*") == @["", ""]
  check "a".matchWithCapt(re2"(a*)*?") == @["a"]
  check "a".matchWithCapt(re2"(a?)*?") == @["a"]
  check "a".matchWithBounds(re2"(a*?)*") == @[1 .. 0]
  check "a".matchWithCapt(re2"(a*?)*") == @[""]
  check "a".matchWithCapt(re2"(a*?)*?") == @["a"]
  check "a".matchWithBounds(re2"(a??)*") == @[1 .. 0]
  check "a".matchWithCapt(re2"(a??)*") == @[""]
  check "ab".matchWithBounds(re2"(a??)*b") == @[1 .. 0]
  check "ab".matchWithCapt(re2"(a??)*b") == @[""]
  check "".matchWithBounds(re2"(a??)*") == @[0 .. -1]
  check "".matchWithCapt(re2"(a??)*") == @[""]
  check "a".matchWithCapt(re2"(a?)??") == @["a"]
  check "a".matchWithCapt(re2"(a*)??") == @["a"]
  check "a".matchWithCapt(re2"(a*)+?") == @["a"]
  check "".matchWithBounds(re2"(a?)+") == @[0 .. -1]
  check "".matchWithCapt(re2"(a?)+") == @[""]
  check "".matchWithBounds(re2"(a?)(a?)*") == @[0 .. -1, 0 .. -1]
  check "".matchWithCapt(re2"(a?)(a?)*") == @["", ""]
  check "a".matchWithBounds(re2"(a?)+") == @[1 .. 0]
  check "a".matchWithCapt(re2"(a?)+") == @[""]
  check "".matchWithBounds(re2"(a*)+") == @[0 .. -1]
  check "".matchWithCapt(re2"(a*)+") == @[""]
  check "".matchWithBounds(re2"(a*)(a*)*") == @[0 .. -1, 0 .. -1]
  check "".matchWithCapt(re2"(a*)(a*)*") == @["", ""]
  check "a".matchWithBounds(re2"(a*)+") == @[1 .. 0]
  check "a".matchWithCapt(re2"(a*)+") == @[""]
  check "b".matchWithBounds(re2"(a*)+b") == @[0 .. -1]
  check "b".matchWithCapt(re2"(a*)+b") == @[""]
  check "b".matchWithBounds(re2"(a*)+b*") == @[0 .. -1]
  check "b".matchWithCapt(re2"(a*)+b*") == @[""]
  check "ab".matchWithBounds(re2"(a*)+b") == @[1 .. 0]
  check "ab".matchWithCapt(re2"(a*)+b") == @[""]
  check "".matchWithBounds(re2"(a?)*") == @[0 .. -1]
  check "".matchWithCapt(re2"(a?)*") == @[""]
  check "a".matchWithBounds(re2"(a?)*") == @[1 .. 0]
  check "a".matchWithCapt(re2"(a?)*") == @[""]
  check "a".matchWithBounds(re2"(a?)*(a?)*") == @[1 .. 0, 1 .. 0]
  check "a".matchWithCapt(re2"(a?)*(a?)*") == @["", ""]
  check "ab".matchWithBounds(re2"(a?)*b") == @[1 .. 0]
  check "ab".matchWithCapt(re2"(a?)*b") == @[""]
  check "".matchWithBounds(re2"(a+)*") == @[nonCapture]
  check "".matchWithCapt(re2"(a+)*") == @[""]
  check "a".matchWithCapt(re2"(?:a*)*") == newSeq[string]()
  check "a".matchWithBounds(re2"(a?b?)*") == @[1 .. 0]
  check "a".matchWithCapt(re2"(a?b?)*") == @[""]
  check "".matchWithBounds(re2"(a?b?)*") == @[0 .. -1]
  check "".matchWithCapt(re2"(a?b?)*") == @[""]

test "talternations":
  check raises(r"a|?")
  check raises(r"a|?b")
  check raises(r"?|?")
  check raises(r"a|*")
  check raises(r"a|*b")
  check raises(r"a|+")
  check raises(r"a|+b")

test "tcaptures":
  check "ab".matchWithCapt(re2"(a)b") == @["a"]
  check "aa".matchWithCapt(re2"(a)*") == @["a"]
  check "aab".matchWithCapt(re2"((a)*b)") == @["aab", "a"]
  check "abbbbccccd".matchWithCapt(re2"a(b|c)*d") == @["c"]
  check "abbb".matchWithCapt(re2"((a)*(b)*)") == @["abbb", "a", "b"]
  check "abbb".matchWithCapt(re2"((a(b)*)*(b)*)") ==
    @["abbb", "abbb", "b", ""]
  check "aa".matchWithCapt(re2"(a)+") == @["a"]
  check "abab".matchWithCapt(re2"(ab)+") == @["ab"]
  check "a".matchWithCapt(re2"(a)?") == @["a"]
  check "ab".matchWithCapt(re2"(ab)?") == @["ab"]
  check "aaabbbaaa".matchWithCapt(re2"(a*|b*)*") == @[""]
  check "abab".matchWithCapt(re2"(a(b))*") == @["ab", "b"]
  check "aaanasdnasd".matchWithCapt(re2"((a)*n?(asd)*)*") ==
    @["", "a", "asd"]
  check "aaanasdnasd".matchWithCapt(re2"((a)*n?(asd))*") ==
    @["nasd", "a", "asd"]
  check "b".matchWithCapt(re2"(a)?b") == @[""]
  check "ฅa".matchWithCapt(re2"(\w)(a)") == @["ฅ", "a"]

test "tzero_or_more_op":
  check raisesMsg(r"*") ==
    "Invalid `*` operator, nothing to repeat"
  check raises(r"*abc")
  check(not raises(r"\b*"))

test "tone_or_more_op":
  check "aaaa".isMatch(re2"a+")
  check "abb".isMatch(re2"ab+")
  check "abaa".isMatch(re2"aba+")
  check(not "".isMatch(re2"a+"))
  check(not "b".isMatch(re2"a+"))
  check(not "aab".isMatch(re2"b+"))
  check raisesMsg(r"(+)") ==
    "Invalid `+` operator, nothing to repeat"
  check raises(r"+")
  check raises(r"+abc")
  check(not raises(r"\b+"))

test "tzero_or_one_op":
  check "a".isMatch(re2"a?")
  check "".isMatch(re2"a?")
  check "a".isMatch(re2"ab?")
  check "ab".isMatch(re2"ab?")
  check "aba".isMatch(re2"ab?a")
  check "aa".isMatch(re2"ab?a")
  check(not "aa".isMatch(re2"a?"))
  check(not "b".isMatch(re2"a?"))
  check(not "abb".isMatch(re2"ab?"))
  check raisesMsg(r"?") ==
    "Invalid `?` operator, nothing to make optional"
  check raises(r"?abc")
  check(not raises(r"\b?"))

test "tescape":
  check "(a)".isMatch(re2"\(a\)")
  check "a*b".isMatch(re2"a\*b")
  check "a*bbb".isMatch(re2"a\*b*")
  check "y".isMatch(re2"\y")
  check "\\".isMatch(re2"\\")
  check "\\\\".isMatch(re2"\\\\")

test "talphanum_shorthand":
  check "a".isMatch(re2"\w")
  check "abc123".isMatch(re2"\w*")
  check "a".matchWithCapt(re2"(\w)") == @["a"]

test "tdigit":
  check "1".isMatch(re2"\d")
  check "123".isMatch(re2"\d*")
  check "۲".isMatch(re2"\d")  # Kharosthi numeral
  check(not "⅕".isMatch(re2"\d"))

test "twhite_space_shorthand":
  check " ".isMatch(re2"\s")
  check "   ".isMatch(re2"\s*")
  check " \t\r\f\v".isMatch(re2"\s*")
  check "\u20".isMatch(re2"\s")  # New Line
  check "\u2028".isMatch(re2"\s")  # Line separator

test "talphanum_not_shorthand":
  check(not "a".isMatch(re2"\W"))
  check(not "abc123".isMatch(re2"\W*"))
  check "!@#".isMatch(re2"\W+")

test "tnot_digit":
  check(not "1".isMatch(re2"\D"))
  check(not "123".isMatch(re2"\D*"))
  check(not "۲".isMatch(re2"\D"))  # Kharosthi numeral
  check "⅕".isMatch(re2"\D")
  check "!@#".isMatch(re2"\D+")
  check "a".isMatch(re2"\D")

test "tnot_white_space_shorthand":
  check "asd123!@#".isMatch(re2"\S*")
  check(not " ".isMatch(re2"\S"))
  check(not "   ".isMatch(re2"\S*"))
  check(not "\t".isMatch(re2"\S"))
  check(not "\u20".isMatch(re2"\S"))
  check(not "\r".isMatch(re2"\S"))
  check(not "\f".isMatch(re2"\S"))
  check(not "\v".isMatch(re2"\S"))
  check(not "\u2028".isMatch(re2"\S"))  # Line separator

test "tset":
  check "a".isMatch(re2"[a]")
  check "a".isMatch(re2"[abc]")
  check "b".isMatch(re2"[abc]")
  check "c".isMatch(re2"[abc]")
  check(not "d".isMatch(re2"[abc]"))
  check "a".isMatch(re2"[\w]")
  check "1".isMatch(re2"[\w]")
  check "1".isMatch(re2"[\d]")
  check "*".isMatch(re2"[*]")
  check "*".isMatch(re2"[\*]")
  check "*".isMatch(re2"[a*]")
  check "a".isMatch(re2"[a*]")
  check "a".isMatch(re2"[a-z]")
  check "f".isMatch(re2"[a-z]")
  check "z".isMatch(re2"[a-z]")
  check(not "A".isMatch(re2"[a-z]"))
  check "0".isMatch(re2"[0-9]")
  check "5".isMatch(re2"[0-9]")
  check "9".isMatch(re2"[0-9]")
  check(not "a".isMatch(re2"[0-9]"))
  check "(".isMatch(re2"[()[\]{}]")
  check ")".isMatch(re2"[()[\]{}]")
  check "}".isMatch(re2"[()[\]{}]")
  check "{".isMatch(re2"[()[\]{}]")
  check "[".isMatch(re2"[()[\]{}]")
  check "]".isMatch(re2"[()[\]{}]")
  check "(".isMatch(re2"[]()[{}]")
  check ")".isMatch(re2"[]()[{}]")
  check "}".isMatch(re2"[]()[{}]")
  check "{".isMatch(re2"[]()[{}]")
  check "[".isMatch(re2"[]()[{}]")
  check "]".isMatch(re2"[]()[{}]")
  check "\\".isMatch(re2"[\\]")
  check "\\".isMatch(re2"[\\\]]")
  check "]".isMatch(re2"[\\\]]")
  check "00".isMatch(re2"[0-5][0-9]")
  check "59".isMatch(re2"[0-5][0-9]")
  check(not "95".isMatch(re2"[0-5][0-9]"))
  check "1".isMatch(re2"[0-57-9]")
  check "8".isMatch(re2"[0-57-9]")
  check(not "6".isMatch(re2"[0-57-9]"))
  check "4".isMatch(re2"[0-9A-Fa-f]")
  check "b".isMatch(re2"[0-9A-Fa-f]")
  check "B".isMatch(re2"[0-9A-Fa-f]")
  check(not "-".isMatch(re2"[0-9A-Fa-f]"))
  check "-".isMatch(re2"[a\-z]")
  check "a".isMatch(re2"[a\-z]")
  check "z".isMatch(re2"[a\-z]")
  check(not "b".isMatch(re2"[a\-z]"))
  check "a".isMatch(re2"[a-]")
  check "-".isMatch(re2"[a-]")
  check "+".isMatch(re2"[(+*)]")
  check "*".isMatch(re2"[(+*)]")
  check "(".isMatch(re2"[(+*)]")
  check "[".isMatch(re2"[[-\]]")
  check "]".isMatch(re2"[[-\]]")
  check(not "-".isMatch(re2"[[-\]]"))
  check "(".isMatch(re2"[(-\)]")
  check ")".isMatch(re2"[(-\)]")
  check(not "-".isMatch(re2"[(-\)]"))
  check "\\".isMatch(re2"[\\-\\)]")
  check(not "-".isMatch(re2"[\\-\\)]"))
  check "-".isMatch(re2"[-]")
  check "-".isMatch(re2"[\-]")
  check "-".isMatch(re2"[\-\-]")
  check "-".isMatch(re2"[\--]")
  check "-".isMatch(re2"[\--\-]")
  check "-".isMatch(re2"[\---]")
  check "b".isMatch(re2"[\--\-a-z]")
  check "b".isMatch(re2"[\---a-z]")
  check "b".isMatch(re2"[-a-z]")
  check "-".isMatch(re2"[-a-z]")
  check "a".isMatch(re2"[-a]")
  check "-".isMatch(re2"[-a]")
  check "b".isMatch(re2"[a-d-z]")
  check "-".isMatch(re2"[a-d-z]")
  check "z".isMatch(re2"[a-d-z]")
  check(not "e".isMatch(re2"[a-d-z]"))
  check "]".isMatch(re2"[]]")
  check "]".isMatch(re2"[\]]")
  check(not "[".isMatch(re2"[]]"))
  check(not "]]".isMatch(re2"[]]"))
  check(not "-".isMatch(re2"[[-\]]"))
  check(not "b".isMatch(re2"[c-d]"))
  check "-".isMatch(re2"[a\w-\wz]")
  check "-".isMatch(re2"[\w-a]")
  check "-".isMatch(re2"[\w-]")
  check "a".isMatch(re2"[\w-a]")
  check "1".isMatch(re2"[\w-a]")
  check "-".isMatch(re2"[db-c-f]")
  check(not "e".isMatch(re2"[db-c-f]"))
  check(not "-".isMatch(re2"[=-_]"))
  check "A".isMatch(re2"[\A]")
  check "b".isMatch(re2"[\b]")
  check "zz".isMatch(re2"[\z][\z]")
  check(not "z".isMatch(re2"[\z][\z]"))
  check raisesMsg(r"[a-\w]") ==
    "Invalid set range. Range can't contain " &
    "a character-class or assertion\n" &
    "[a-\\w]\n" &
    "   ^"
  check(not raises(r"[a-\b]"))
  check raisesMsg(r"[d-c]") ==
    "Invalid set range. " &
    "Start must be lesser than end\n" &
    "[d-c]\n" &
    "   ^"
  check raisesMsg(r"abc[]") ==
    "Invalid set. Missing `]`\n" &
    "abc[]\n" &
    "   ^"
  check raisesMsg(r"[]abc") ==
    "Invalid set. Missing `]`\n" &
    "[]abc\n" &
    "^"
  check raisesMsg(r"[abc") ==
    "Invalid set. Missing `]`\n" &
    "[abc\n" &
    "^"
  check raises(r"[a")
  check raises(r"[a-")
  check raises(r"[-a")
  check raises(r"[\\")
  check raises(r"[]")
  check raises(r"[^]")
  check raises(r"[]a")
  check raises(r"[-")
  check "a".isMatch(re2"[\u0061]")
  check(not "b".isMatch(re2"[\u0061]"))
  check "a".isMatch(re2"[\U00000061]")
  check "a".isMatch(re2"[\x61]")
  check "a".isMatch(re2"[\x{61}]")
  check "abab".isMatch(re2"[\x61-\x62]*")
  check "a".isMatch(re2"[\141]")

test "tnot_set":
  check "a".matchWithCapt(re2"([^b])") == @["a"]
  check "asd".matchWithCapt(re2"([^b]*)") == @["asd"]
  check "ab".matchWithCapt(re2"([^b]*b)") == @["ab"]
  check "asd123".matchWithCapt(re2"([^\d]*)(\d*)") == @["asd", "123"]
  check "asd123".matchWithCapt(re2"([asd]*)([^asd]*)") == @["asd", "123"]
  check "<asd123!@#>".matchWithCapt(re2"(<[^>]*>)") == @["<asd123!@#>"]
  check(not "a".isMatch(re2"[^a]"))
  check raisesMsg(r"[^]") ==
    "Invalid set. Missing `]`\n" &
    "[^]\n" &
    "^"
  check "^".isMatch(re2"[\^]")
  check "a".isMatch(re2"[\^a]")
  check(not "^".isMatch(re2"[^^]"))
  check "a".isMatch(re2"[^^]")
  check "a".isMatch(re2"[^-]")
  check(not "-".isMatch(re2"[^-]"))

test "trepetition_range":
  check(not "".isMatch(re2"a{0}"))
  check(not "".isMatch(re2"a{0,0}"))
  check "".isMatch(re2"a{0,2}")
  check "a".isMatch(re2"a{0}")
  check "a".isMatch(re2"a{0,0}")
  check "a".isMatch(re2"a{1}")
  check "aa".isMatch(re2"a{2}")
  check "aaa".isMatch(re2"a{3}")
  check(not "aaaa".isMatch(re2"a{3}"))
  check(not "".isMatch(re2"a{1}"))
  check "a".isMatch(re2"a{1,1}")
  check "a".isMatch(re2"a{1,2}")
  check "aa".isMatch(re2"a{1,2}")
  check(not "aaa".isMatch(re2"a{1,2}"))
  check(not "a".isMatch(re2"a{2,4}"))
  check "a".isMatch(re2"a{1,}")
  check "aa".isMatch(re2"a{1,}")
  check "aaa".isMatch(re2"a{1,}")
  check "aaaaaaaaaa".isMatch(re2"a{1,}")
  check "aa".isMatch(re2"a{2,}")
  check "aaaaaaaaaa".isMatch(re2"a{0,}")
  check "".isMatch(re2"a{0,}")
  check(not "a".isMatch(re2"a{2,}"))
  check "a{a,1}".isMatch(re2"a{a,1}")
  check(not "a".isMatch(re2"a{a,1}"))
  check(not "a1".isMatch(re2"a{a,1}"))
  check "a{".isMatch(re2"a{")
  check "a{{".isMatch(re2"a{{")
  check "a{}".isMatch(re2"a{}")
  check raises(r"a*{,}")
  check raises(r"a*{0}")
  check raises(r"a*{1}")
  check "aaa".matchWithCapt(re2"(a){0,}") == @["a"]
  check "aaa".matchWithCapt(re2"(a{0,}){0,}") == @[""]
  check "aaaaa".matchWithCapt(re2"(a){5}") == @["a"]
  check "a".matchWithCapt(re2"(a){1,5}") == @["a"]
  check "aaa".matchWithCapt(re2"(a){1,5}") == @["a"]
  check "".matchWithCapt(re2"(a){0,}") == @[""]
  check "aaa".matchWithCapt(re2"(a{0,}){0,}") == @[""]
  check "aaa".matchWithCapt(re2"(a{1}){0,}") == @["a"]
  check "aaaa".matchWithCapt(re2"(a{2}){0,}") == @["aa"]
  check "aaaa".matchWithCapt(re2"(a{0,3}){0,}") == @[""]
  check "".matchWithCapt(re2"(a{0,3}){0,}") == @[""]
  check "aaa".matchWithCapt(re2"(a{1,}){0,}") == @["aaa"]
  check "".matchWithCapt(re2"(a{1,}){0,}") == @[""]
  check(not "".isMatch(re2"(a{1,})"))
  check "a".matchWithCapt(re2"(a{1,})") == @["a"]
  check "aaa".matchWithCapt(re2"(a{1,})") == @["aaa"]
  check "abab".matchWithCapt(re2"(a(b)){2}") == @["ab", "b"]
  check raisesMsg(r"a{0,bad}") ==
    "Invalid repetition range. Range can only contain digits\n" &
    "a{0,bad}\n" &
    " ^"
  check raisesMsg(r"a{1111111111}") ==
    "Invalid repetition range. Max value is 32767\n" &
    "a{1111111111}\n" &
    " ^"
  check raisesMsg(r"a{0,101}") ==
    "Invalid repetition range. Expected 100 repetitions " &
    "or less, but found: 101\n" &
    "a{0,101}\n" &
    " ^"
  check(not raises(r"a{1,101}"))
  check raises(r"a{0,a}")
  check raises(r"a{,}")
  check raises(r"a{,1}")
  check raises(r"a{1x}")
  check raises(r"a{1,x}")
  check raises(r"a{1")
  check raises(r"a{1,")
  check raises(r"a{1,,}")
  check raises(r"a{1,,2}")
  check raises(r"a{1,,,2}")
  check raisesMsg(r"{10}") ==
    "Invalid repeition range, " &
    "nothing to repeat"
  check raisesMsg(r"abc\A{10}") ==
    "Invalid repetition range, either " &
    "char, shorthand (i.e: \\w), group, or set " &
    "expected before repetition range"

test "tnon_capturing_groups":
  check "abab".matchWithCapt(re2"(a(b))*") == @["ab", "b"]
  check "abab".matchWithCapt(re2"(?:a(b))*") == @["b"]
  check "abab".matchWithCapt(re2"(a(?:b))*") == @["ab"]
  check ")".matchWithCapt(re2"(\))") == @[")"]

test "tgreediness":
  check "a".matchWithCapt(re2"(a)??") == @["a"]
  check "aaa".matchWithCapt(re2"(a)*(a)*(a)*") == @["a", "", ""]
  check "aaa".matchWithCapt(re2"(a)*?(a)*(a)*?") == @["", "a", ""]
  check "aaa".matchWithCapt(re2"(a)*?(a)*?(a)*") == @["", "", "a"]
  check "aaa".matchWithCapt(re2"(a)*?(a)*?(a)*?") == @["", "", "a"]
  check "aaaa".matchWithCapt(re2"(a)*?(a)*?(a)*?") == @["", "", "a"]
  check "aa".matchWithCapt(re2"(a)?(aa?)") == @["a", "a"]
  check "aa".matchWithCapt(re2"(a)??(a)") == @["a", "a"]
  check "aa".matchWithCapt(re2"(a)??(aa?)") == @["", "aa"]
  check "aaa".matchWithCapt(re2"(a)+(a)+(a)?") == @["a", "a", ""]
  check "aaa".matchWithCapt(re2"(a)+?(a)+(a)?") == @["a", "a", ""]
  check "aaa".matchWithCapt(re2"(a)+?(a)+?(a)?") == @["a", "a", "a"]
  check "aaa".matchWithCapt(re2"(a){0,}(a){0,}(a){0,}") == @["a", "", ""]
  check "aaa".matchWithCapt(re2"(a){0,}?(a){0,}(a){0,}?") == @["", "a", ""]
  check "aaa".matchWithCapt(re2"(a){0,}?(a){0,}?(a){0,}") == @["", "", "a"]
  check "aaa".matchWithCapt(re2"(a){0,}?(a){0,}?(a){0,}?") == @["", "", "a"]
  check "aaa".matchWithCapt(re2"(a){1,}(a){1,}(a)?") == @["a", "a", ""]
  check "aaa".matchWithCapt(re2"(a){1,}?(a){1,}(a)?") == @["a", "a", ""]
  check "aaa".matchWithCapt(re2"(a){1,}?(a){1,}?(a)?") == @["a", "a", "a"]
  block:
    var m: RegexMatch2
    check match("aaaa", re2"(a*?)(a*?)(a*)", m)
    check m.toStrCaptures("aaaa") == @["", "", "aaaa"]
  block:
    var m: RegexMatch2
    check match("aaaa", re2"(a*)(a*?)(a*?)", m)
    check m.toStrCaptures("aaaa") == @["aaaa", "", ""]

test "tassertions":
  check "bbaa aa".matchWithCapt(re2"([\w ]*?)(\baa\b)") == @["bbaa ", "aa"]
  check "aa bbaa".matchWithCapt(re2"(\baa\b)([\w ]*)") == @["aa", " bbaa"]
  check "This island is great".matchWithCapt(
      re2"([\w ]*?)(\bis\b)([\w ]*?)") == @["This island ", "is", " great"]
  check "bbaabb".matchWithCapt(re2"([\w ]*?)(\Baa\B)([\w ]*?)") ==
    @["bb", "aa", "bb"]
  check "This is my sister".matchWithCapt(
      re2"([\w ]*?)(\Bis\B)([\w ]*?)") == @["This is my s", "is", "ter"]
  check "aa".isMatch(re2"\b\b\baa\b\b\b")
  check "bb".isMatch(re2"^^^^bb$$$$")
  check "bb".isMatch(re2"\A\A\A\Abb\z\z\z\z")

test "tdot_any_matcher":
  check "a".isMatch(re2".")
  check "asd123!@#".isMatch(re2".*")
  check "| (•□•) | (❍ᴥ❍ʋ)".isMatch(re2".*")
  check "ฅ^•ﻌ•^ฅ".matchWithCapt(re2"(.*)") == @["ฅ^•ﻌ•^ฅ"]
  check "\t".isMatch(re2".")
  check(not "\L".isMatch(re2".*"))

test "tgroup":
  block:
    var m: RegexMatch2
    check "foobar".match(re2"(\w*)", m)
    check m.group(0) == 0..5
  block:
    var m: RegexMatch2
    check "foobar".match(re2"(?P<foo>\w*)", m)
    check m.group(0) == 0..5
  block:
    var m: RegexMatch2
    check "ab".match(re2"(a)(b)", m)
    check m.group(0) == 0..0
    check m.group(1) == 1..1
  block:
    var m: RegexMatch2
    check match("ab", re2"(a)(b)", m)
    check m.toStrCaptures("ab") == @["a", "b"]
  block:
    var m: RegexMatch2
    check "abc".match(re2"(?P<foo>\w)+", m)
    check m.group("foo") == 2..2
  block:
    var m: RegexMatch2
    check "abc".match(re2"(\w)+", m)
    check m.group(0) == 2..2

test "tnamed_groups":
  block:
    var m: RegexMatch2
    check "foobar".match(re2"(?P<foo>\w*)", m)
    check m.group("foo") == 0..5
  block:
    var m: RegexMatch2
    check "foobar".match(re2"(?P<foo>(?P<bar>\w*))", m)
    check m.group("foo") == 0..5
    check m.group("bar") == 0..5
  block:
    var m: RegexMatch2
    check "aab".match(re2"(?P<foo>(?P<bar>a)*b)", m)
    check m.group("foo") == 0..2
    check m.group("bar") == 1..1
  block:
    var m: RegexMatch2
    check "aab".match(re2"((?P<bar>a)*b)", m)
    check m.group("bar") == 1..1

  check raisesMsg(r"abc(?Pabc)") ==
    "Invalid group name. Missing `<`\n" &
    "abc(?Pabc)\n" &
    "   ^"
  check raisesMsg(r"abc(?P<abc") ==
    "Invalid group name. Missing `>`\n" &
    "abc(?P<abc\n" &
    "   ^"
  check raisesMsg(r"a(?P<>abc)") ==
    "Invalid group name. Name can't be empty\n" &
    "a(?P<>abc)\n" &
    " ^"
  check raisesMsg(r"(a)b)") ==
    "Invalid capturing group. " &
    "Found too many closing symbols"
  check raisesMsg(r"(b(a)") ==
    "Invalid capturing group. " &
    "Found too many opening symbols"
  check raisesMsg(r"a(?P<asd)") ==
    "Invalid group name. Expected char in " &
    "{'a'..'z', 'A'..'Z', '0'..'9', '-', '_'}, " &
    "but found `)`\n" &
    "a(?P<asd)\n" &
    " ^"
  check(not raises(r"(?P<abcdefghijklmnopqrstuvwxyz" &
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_>abc)"))
  check(not raises(r"(\b)"))
  #[
  var manyGroups = newStringOfCap(int16.high * 3)
  for _ in 0 ..< int16.high - 1:
    manyGroups.add(r"(a)")
  check(not raises(manyGroups))
  manyGroups.add(r"(a)")
  check raisesMsg(manyGroups) ==
    "Invalid number of capturing " &
    "groups, the limit is 32766"
  ]#

test "tflags":
  check "foo\Lbar".isMatch(re2"(?s).*")
  check "foo\Lbar".isMatch(re2"(?s:.*)")
  check "foo\Lbar".isMatch(re2"(?ssss).*")
  check(not "foo\Lbar".isMatch(re2"(?s-s).*"))
  check(not "foo\Lbar".isMatch(re2"(?-s-s-s).*"))
  check(not "foo\Lbar".isMatch(re2"(?-ss).*"))
  check(not "foo\Lbar".isMatch(re2"(?-ss-ss).*"))
  check(not "foo\Lbar".isMatch(re2"(?-sssss-s).*"))
  check(not "foo\Lbar".isMatch(re2"(?s-s:.*)"))
  check(not "foo\Lbar".isMatch(re2"(?------s----s:.*)"))
  check "foo\Lbar".matchWithCapt(re2"((?s:.*))") == @["foo\Lbar"]
  check "a".matchWithCapt(re2"((?i:a))") == @["a"]
  check "A".matchWithCapt(re2"((?i:a))") == @["A"]
  check "ABC".matchWithCapt(re2"((?i:aBc))") == @["ABC"]
  check "a".matchWithCapt(re2"((?-i:a))") == @["a"]
  check(not "A".isMatch(re2"((?-i:a))"))
  check(not "A".isMatch(re2"((?-ii-i:a))"))
  check "a".matchWithCapt(re2"((?i)a)") == @["a"]
  check "A".matchWithCapt(re2"((?i)a)") == @["A"]
  check "a".matchWithCapt(re2"((?-i)a)") == @["a"]
  check(not "A".isMatch(re2"((?-i)a)"))
  check "AaA".isMatch(re2"(?i)a+")
  check "AaA".isMatch(re2"(?i)A+")
  check "AbC".isMatch(re2"(?i)abc")
  check(not "b".isMatch(re2"(?i)a"))
  check "A".isMatch(re2"(?-i)(?i)a")
  check(not "A".isMatch(re2"(?i)(?-i)a"))
  check "AaA".matchWithCapt(re2"((?i)a+)") == @["AaA"]
  check "A".isMatch(re2"(?i)[a]")
  check "a".isMatch(re2"(?i)[a]")
  check(not "@".isMatch(re2"(?i)[a]"))
  check "a".isMatch(re2"(?i)[A]")
  check "A".isMatch(re2"(?i)[A]")
  check "C".isMatch(re2"(?i)[a-z]")
  check "c".isMatch(re2"(?i)[a-z]")
  check(not "@".isMatch(re2"(?i)[a-z]"))
  check "c".isMatch(re2"(?i)[A-Z]")
  check "C".isMatch(re2"(?i)[A-Z]")

  check "aa".matchWithCapt(re2"((?U)a*)(a*)") == @["", "aa"]
  check "aa".matchWithCapt(re2"((?U)a*?)(a*)") == @["aa", ""]
  check "aa".matchWithCapt(re2"((?U-U)a*)(a*)") == @["aa", ""]
  # no empty matches
  check "aa".matchWithCapt(re2"(?U:(a)*)(a)*") == @["", "a"]
  check "aa".matchWithCapt(re2"((?U:a*))(a*)") == @["", "aa"]
  check "aa".matchWithCapt(re2"((?U:a*?))(a*)") == @["aa", ""]
  check "aa".matchWithCapt(re2"((?U-U:a*))(a*)") == @["aa", ""]

  check(not "a\Lb\L".isMatch(re2"(?sm)a.b(?-sm:.)"))
  check "a\Lb\L".isMatch(re2"(?ms)a.b(?s-m:.)")
  check "a\L".isMatch(re2"(?s)a.")
  check(not "a\L\L".isMatch(re2"(?s)a.$."))
  check "a\L\L".isMatch(re2"(?sm)a.$.")
  check(not "a\L\L".isMatch(re2"(?-sm)a.$."))
  check(not "a\L\L".isMatch(re2"(?s-m)a.$."))
  check "a\L\L".isMatch(re2"(?s-m)(?m:a.$.)")
  check(not "a\L\L".isMatch(re2"(?i-sm)(?s:a.$.)"))
  check "a\L\L".isMatch(re2"(?i-sm)(?sm:a.$.)")
  check(not "a\L\L".isMatch(re2"(?-sm)(?sm)(?-sm:a.$.)"))
  check "a\L\L".isMatch(re2"(?sm)(?-sm)(?sm:a.$.)")
  check(not "a\L\L".isMatch(re2"(?-sm)(?sm:(?-sm:a.$.))"))
  check "a\L\L".isMatch(re2"(?sm)(?-sm:(?sm:a.$.))")

  check "Ǝ".isMatch(re2"\w")
  check "Ǝ".isMatch(re2"(?u)\w")
  check(not "Ǝ".isMatch(re2"(?-u)\w"))
  check "abczABCZ0129_".isMatch(re2"(?-u)\w*")
  check(not "\t".isMatch(re2"(?-u)\w"))
  # todo: test every ascii kind
  check "Ǝ".isMatch(re2"(?u)[\w]")
  check(not "Ǝ".isMatch(re2"(?u)[^\w]"))
  check "Ǝ".isMatch(re2"(?-u)[^\w]")
  check(not "Ǝ".isMatch(re2"(?-u)[\w]"))
  check(not "\t".isMatch(re2"(?-u)[\w]"))
  check "ƎƎ".isMatch(re2"(?-u)[^\w](?u)\w")

  check "a".isMatch(re2"(?x)a")
  check "a".isMatch(re2"(?x)a ")
  check "a".isMatch(re2"(?x)a   ")
  check "a".isMatch(re2"(?x) a ")
  check "a".isMatch(re2("(?x)a\L   \L   \L"))
  check "a".isMatch(re2("(?x)\L a \L"))
  check "a".isMatch(re2"(?x: a )")
  check "a".isMatch(re2"""(?x)a""")
  check "a".isMatch(re2"""(?x)
    a
    """)
  check "a".isMatch(re2"""(?x:
    a
    )""")
  check "a".isMatch(re2"""(?x)(
    a
    )""")
  check "a".isMatch(re2"""(?x)
    a  # should ignore this comment
    """)
  check "a".isMatch(re2"""(?x:
    a  # should ignore this comment
    )""")
  check "aa ".isMatch(re2"(?x)a  (?-x)a ")
  check "a a".isMatch(re2"a (?x)a  ")
  check "aa".isMatch(re2"((?x)a    )a")
  check "aa".isMatch(re2"(?x:a    )a")
  check "a ".isMatch(re2"(?x)a\ ")
  check "a ".isMatch(re2"(?x)a\   ")
  check "a#".isMatch(re2"(?x)a\#")
  check "a ".isMatch(re2"(?x)a[ ]")
  check "a\n".isMatch(re2"(?x)a\n")
  check "aa ".isMatch(re2"""(?x)
    a    #    comment
    (?-x)a """)
  check "aaa".isMatch(re2"""(?x)  # comment
    a  # comment
    a  # comment
    a  # comment
    # comment""")
  check "12.0".isMatch(re2"""(?x)
    \d +  # the integral part
    \.    # the decimal point
    \d *  # some fractional digits""")
  check re2"""(?x)    # verbose mode
    ^                   # beginning of string
    M{0,4}              # thousands - 0 to 4 M's
    (CM|CD|D?C{0,3})    # hundreds - 900 (CM), 400 (CD), 0-300 (0 to 3 C's),
                        #            or 500-800 (D, followed by 0 to 3 C's)
    (XC|XL|L?X{0,3})    # tens - 90 (XC), 40 (XL), 0-30 (0 to 3 X's),
                        #        or 50-80 (L, followed by 0 to 3 X's)
    (IX|IV|V?I{0,3})    # ones - 9 (IX), 4 (IV), 0-3 (0 to 3 I's),
                        #        or 5-8 (V, followed by 0 to 3 I's)
    $                   # end of string
    """ in "MMMMDCCCLXXXVIII"

  check raisesMsg(r"(?uq)") ==
    "Invalid group flag, found q but " &
    "expected one of: i, m, s, U or u"
  check raisesMsg(r"(?u-q)") ==
    "Invalid group flag, found -q but " &
    "expected one of: -i, -m, -s, -U or -u"
  check raisesMsg(r"abc(?q)") ==
    "Invalid group. Unknown group type\n" &
    "abc(?q)\n" &
    "   ^"

test "tescaped_sequences":
  check "\x07".isMatch(re2"\a")
  check "\x0C".isMatch(re2"\f")
  check "\t".isMatch(re2"\t")
  check "\L".isMatch(re2"\n")
  check "\r".isMatch(re2"\r")
  check "\x0B".isMatch(re2"\v")
  check(not "a".isMatch(re2"\a"))
  check ".+*?()|[]{}^$".isMatch(re2"\.\+\*\?\(\)\|\[\]\{\}\^\$")

  check "\x07".isMatch(re2"[\a]")
  check "\x07".isMatch(re2"[\a-\a]")
  check(not "0".isMatch(re2"[\a-\a]"))
  #check "|".isMatch(re2"[a|b]")  # ????

test "tfind":
  #[
  block:
    var m: RegexMatch2
    check "abcd".find(re2"bc", m)
  block:
    var m: RegexMatch2
    check(not "abcd".find(re2"ac", m))
  block:
    var m: RegexMatch2
    check "a".find(re2"", m)
  block:
    var m: RegexMatch2
    check "abcd".find(re2"^abcd$", m)
  ]#
  block:
    var m: RegexMatch2
    check "2222".find(re2"(22)*", m)
    check m.group(0) == 2 .. 3
  block:
    var m: RegexMatch2
    check "abcd".find(re2"(ab)", m)
    check m.group(0) == 0 .. 1
  block:
    var m: RegexMatch2
    check "abcd".find(re2"(bc)", m)
    check m.group(0) == 1 .. 2
  block:
    var m: RegexMatch2
    check "abcd".find(re2"(cd)", m)
    check m.group(0) == 2 .. 3
  block:
    var m: RegexMatch2
    check "abcd".find(re2"bc", m)
    check m.boundaries == 1 .. 2
  block:
    var m: RegexMatch2
    check "aΪⒶ弢".find(re2"Ϊ", m)
    check m.boundaries == 1 .. 2
  block:
    var m: RegexMatch2
    check "aΪⒶ弢".find(re2"Ⓐ", m)
    check m.boundaries == 3 .. 5
  block:
    var m: RegexMatch2
    check "aΪⒶ弢".find(re2"弢", m)
    check m.boundaries == 6 .. 9

test "tcontains":
  check re2"bc" in "abcd"
  check re2"bd" notin "abcd"
  check re2"(23)+" in "2323"
  check re2"(23)+" in "23232"
  check re2"^(23)+$" notin "23232"

test "tsplit":
  check split("a,b,c", re2",") == @["a", "b", "c"]
  check split("00232this02939is39an22example111", re2"\d+") ==
    @["", "this", "is", "an", "example", ""]
  check split("AAA :   : BBB", re2"\s*:\s*") == @["AAA", "", "BBB"]
  check split("AAA :   : BBB :   : CCC", re2"\s*:\s*") ==
    @["AAA", "", "BBB", "", "CCC"]
  check split("", re2",") == @[""]
  check split(",,", re2",") == @["", "", ""]
  # nre's behaviour, differs from python
  check split("abc", re2"") == @["a", "b", "c"]
  check split("ab", re2"") == @["a", "b"]
  check split("ab", re2"\b") == @["ab"]
  check split("a b", re2" ") == @["a", "b"]
  check split(",a,Ϊ,Ⓐ,弢,", re2",") ==
    @["", "a", "Ϊ", "Ⓐ", "弢", ""]
  check split("弢", re2"\xAF") == @["弢"]  # "弢" == "\xF0\xAF\xA2\x94"
  block:
    var
      expected = ["", "a", "Ϊ", "Ⓐ", "弢", ""]
      i = 0
    for s in split("11a22Ϊ33Ⓐ44弢55", re2"\d+"):
      check s == expected[i]
      inc i

  check split("Words, words, words.", re2"\W+") ==
    @["Words", "words", "words", ""]
  check split("0a3B9", re2"[a-fA-F]+") ==
    @["0", "3", "9"]
  check split("1 2 3 4 5 6 ", re2" ") ==
    @["1", "2", "3", "4", "5", "6", ""]
  check split("1  2  ", re2" ") == @["1", "", "2", "", ""]
  check split("1 2", re2" ") == @["1", "2"]
  check split("foo", re2"foo") == @["", ""]
  check split("", re2"foo") == @[""]
  check split("bar", re2"foo") == @["bar"]

  check "12".split(re2"\w\b") == @["1", ""]
  check "12".split(re2"\w\B") == @["", "2"]

test "tsplitIncl":
  check "a,b".splitIncl(re2"(,)") == @["a", ",", "b"]
  check "12".splitIncl(re2"(\d)") == @["", "1", "", "2", ""]
  check splitIncl("aΪⒶ弢", re2"(\w)") ==
    @["", "a", "", "Ϊ", "", "Ⓐ", "", "弢", ""]
  check splitIncl("aΪⒶ弢", re2"") == @["a", "Ϊ", "Ⓐ", "弢"]
  check splitIncl("...words, words...", re2"(\W+)") ==
    @["", "...", "words", ", ", "words", "...", ""]
  check splitIncl("Words, words, words.", re2"(\W+)") ==
    @["Words", ", ", "words", ", ", "words", ".", ""]

  # regular split stuff
  check splitIncl("a,b,c", re2",") == @["a", "b", "c"]
  check splitIncl("00232this02939is39an22example111", re2"\d+") ==
    @["", "this", "is", "an", "example", ""]
  check splitIncl("AAA :   : BBB", re2"\s*:\s*") == @["AAA", "", "BBB"]
  check splitIncl("", re2",") == @[""]
  check splitIncl(",,", re2",") == @["", "", ""]
  check splitIncl("abc", re2"") == @["a", "b", "c"]
  check splitIncl(",a,Ϊ,Ⓐ,弢,", re2",") ==
    @["", "a", "Ϊ", "Ⓐ", "弢", ""]
  check splitIncl("弢", re2"\xAF") == @["弢"]  # "弢" == "\xF0\xAF\xA2\x94"
  check splitIncl("Words, words, words.", re2"\W+") ==
    @["Words", "words", "words", ""]
  check splitIncl("0a3B9", re2"[a-fA-F]+") ==
    @["0", "3", "9"]
  check splitIncl("1 2 3 4 5 6 ", re2" ") ==
    @["1", "2", "3", "4", "5", "6", ""]
  check splitIncl("1  2  ", re2" ") == @["1", "", "2", "", ""]
  check splitIncl("1 2", re2" ") == @["1", "2"]
  check splitIncl("foo", re2"foo") == @["", ""]
  check splitIncl("", re2"foo") == @[""]
  check splitIncl("ab", re2"") == @["a", "b"]
  check splitIncl("ab", re2"\b") == @["ab"]
  check splitIncl("a b", re2" ") == @["a", "b"]

test "tfindall":
  check findAllBounds("abcabc abc", re2"abc abc|abc") == @[0 .. 2, 3 .. 9]
  check findAllBounds("abcabc", re2"bc") == @[1 .. 2, 4 .. 5]
  check findAllBounds("aa", re2"a") == @[0 .. 0, 1 .. 1]
  check findAllBounds("a", re2"a") == @[0 .. 0]
  check findAllBounds("a", re2"b").len == 0
  check findAllBounds("", re2"b").len == 0
  check findAllBounds("abc ab", re2"\w+ *") == @[0 .. 3, 4 .. 5]
  check findAllBounds("AAA :   : BBB", re2"\s*:\s*") == @[3 .. 7, 8 .. 9]
  check findAllBounds("a\n  b\n c\n  ", re2"(?m)^") ==
    @[0 .. -1, 2 .. 1, 6 .. 5, 9 .. 8]
  check findAllBounds("a\n  b\n c\n  ", re2"(?m)$") ==
    @[1 .. 0, 5 .. 4, 8 .. 7, 11 .. 10]
  check findAllBounds("\n\n", re2"(?m)^") == @[0 .. -1, 1 .. 0, 2 .. 1]
  check findAllBounds("foobarbaz", re2"(?<=o)b") == @[3 .. 3]
  check findAllBounds("foobarbaz", re2"(?<!o)b") == @[6 .. 6]
  check findAllBounds("aaaabaaaa", re2"(?<!a)a") == @[0 .. 0, 5 .. 5]
  check findAllBounds("foobar", re2"o(?=b)") == @[2 .. 2]
  check findAllBounds("foobar", re2"o(?!b)") == @[1 .. 1]
  check findAllBounds("aaaabaaaa", re2"a(?!a)") == @[3 .. 3, 8 .. 8]
  check findAllBounds("aaa", re2"\w+b|\w") == @[0 .. 0, 1 .. 1, 2 .. 2]
  # This follows nre's empty match behaviour
  check findAllBounds("a", re2"") == @[0 .. -1, 1 .. 0]
  check findAllBounds("ab", re2"") == @[0 .. -1, 1 .. 0, 2 .. 1]
  check findAllBounds("a", re2"\b") == @[0 .. -1, 1 .. 0]
  check findAllBounds("aΪⒶ弢", re2"Ϊ") == @[1 .. 2]
  check findAllBounds("aΪⒶ弢", re2"Ⓐ") == @[3 .. 5]
  check findAllBounds("aΪⒶ弢", re2"弢") == @[6 .. 9]
  check findAllBounds("aΪⒶ弢aΪⒶ弢", re2"Ⓐ") == @[3 .. 5, 13 .. 15]
  # This is nre and Python's re behaviour,
  # they match aaa and then empty end
  check findAllBounds("aaa", re2"a*") == @[0 .. 2, 3 .. 2]
  check findAllBounds("aaab", re2"a*") == @[0 .. 2, 3 .. 2, 4 .. 3]
  check findAllBounds("aaa", re2"a+") == @[0 .. 2]
  check findAllBounds("foo", re2"") ==
    @[0 .. -1, 1 .. 0, 2 .. 1, 3 .. 2]
  check findAllBounds("abb", re2"a*") == @[0 .. 0, 1 .. 0, 2 .. 1, 3 .. 2]
  check findAllBounds("abbbbccccdXabbbbccccdX", re2"a(b|c)*d") ==
    @[0 .. 9, 11 .. 20]
  check findAllBounds("abbbXabbbX", re2"((a)*(b)*)") ==
    @[0 .. 3, 4 .. 3, 5 .. 8, 9 .. 8, 10 .. 9]
  check findAllBounds("abbbXabbbX", re2"((a)+(b)+)") ==
    @[0 .. 3, 5 .. 8]
  check findAllBounds("abbbXabbbX", re2"((a(b)*)+(b)*)") ==
    @[0 .. 3, 5 .. 8]
  check findAllBounds("abXabX", re2"a(b|c)*d").len == 0
  check findAllBounds("aaanasdnasdXaaanasdnasd", re2"((a)*n?(asd)*)+") ==
    @[0 .. 10, 11 .. 10, 12 .. 22, 23 .. 22]
  check findAllBounds("1xxx", re2"\d\w+x") == @[0 .. 3]
  check findAllBounds("xxxx", re2"\d\w+x").len == 0

test "tstarts_with":
  check "abc".startsWith(re2"ab")
  check(not "abc".startsWith(re2"bc"))
  check startsWith("弢ⒶΪ", re2"弢Ⓐ")
  check startsWith("弢", re("\xF0\xAF\xA2\x94"))
  check(not startsWith("弢", re("\xF0\xAF\xA2")))
  check "abc".startsWith(re2"\w")
  check(not "abc".startsWith(re2"\d"))
  check "abc".startsWith(re2"(a|b)")
  check "bc".startsWith(re2"(a|b)")
  check(not "c".startsWith(re2"(a|b)"))
  check startsWith("abc", re2"b", start = 1)
  check startsWith("abc", re2"(?<=a)b", start = 1)
  check(not startsWith("abc", re2"(?<=x)b", start = 1))
  check(not startsWith("abc", re2"^b", start = 1))

test "tends_with":
  check "abc".endsWith(re2"bc")
  check(not "abc".endsWith(re2"ab"))
  check endsWith("弢ⒶΪ", re2"ⒶΪ")
  check endsWith("弢", re("\xF0\xAF\xA2\x94"))
  check(not endsWith("弢", re("\xAF\xA2\x94")))
  check "abc".endsWith(re2"(b|c)")
  check "ab".endsWith(re2"(b|c)")
  check(not "a".endsWith(re2"(b|c)"))

test "tliterals":
  check "a".isMatch(re2"\u0061")
  check(not "b".isMatch(re2"\u0061"))
  check "b".isMatch(re2"\u0062")
  check "Ⓐ".isMatch(re2"\u24b6")
  check "Ⓐ".isMatch(re2"\u24B6")
  check raisesMsg(r"\u123") ==
    "Invalid unicode literal. Expected 4 hex digits, but found 3\n" &
    "\\u123\n" &
    "^"
  check raisesMsg(r"\u123@abc") ==
    "Invalid unicode literal. Expected hex digit, but found @\n" &
    "\\u123@abc\n" &
    "^"
  check "a".isMatch(re2"\U00000061")
  check(not "b".isMatch(re2"\U00000061"))
  check "b".isMatch(re2"\U00000062")
  check "弢".isMatch(re2"\U0002f894")
  check "弢".isMatch(re2"\U0002F894")
  check raisesMsg(r"\U123") ==
    "Invalid unicode literal. Expected 8 hex digits, but found 3\n" &
    "\\U123\n" &
    "^"
  check raisesMsg(r"\U123@a") ==
    "Invalid unicode literal. Expected hex digit, but found @\n" &
    "\\U123@a\n" &
    "^"
  check raisesMsg(r"\UFFFFFFFF") ==
    "Invalid unicode literal. FFFFFFFF value is too big\n" &
    "\\UFFFFFFFF\n" &
    "^"
  check "a".isMatch(re2"\x{61}")
  check "a".isMatch(re2"\x{061}")
  check(not "b".isMatch(re2"\x{61}"))
  check "Ⓐ".isMatch(re2"\x{24b6}")
  check "Ⓐ".isMatch(re2"\x{000024b6}")
  check "弢".isMatch(re2"\x{2f894}")
  check "弢".isMatch(re2"\x{0002f894}")
  check raises(r"\x{FFFFFFFF}")
  check(not raises(r"\x{7fffffff}"))
  check raisesMsg(r"\x{2f894") ==
    "Invalid unicode literal. Expected `}`\n" &
    "\\x{2f894\n" &
    "^"
  check raisesMsg(r"\x{00000000A}") ==
    "Invalid unicode literal. Expected at most 8 chars, found 9\n" &
    "\\x{00000000A}\n" &
    "^"
  check raisesMsg(r"\x{61@}") ==
    "Invalid unicode literal. Expected hex digit, but found @\n" &
    "\\x{61@}\n" &
    " ^"
  check "a".isMatch(re2"\x61")
  check "aa".isMatch(re2"\x61a")
  check "a".isMatch(re2"\x61")
  check "a".isMatch(re2"\141")
  check(not "b".isMatch(re2"\141"))
  check "aa".isMatch(re2"\141a")
  check "\u1ff".isMatch(re2"\777")
  check "888".isMatch(re2"\888")
  check raisesMsg(r"\12") ==
    "Invalid octal literal. Expected 3 octal digits, but found 2\n" &
    "\\12\n" &
    "^"
  check raisesMsg(r"\12@") ==
    "Invalid octal literal. Expected octal digit, but found @\n" &
    "\\12@\n" &
    "^"

test "tchar_class":
  check "a".isMatch(re2"\pL")
  check(not "a".isMatch(re2"\PL"))
  check(not "1".isMatch(re2"\pL"))
  check "1".isMatch(re2"\PL")
  check "aa".isMatch(re2"\pLa")
  check "1".isMatch(re2"\pN")
  check "_".isMatch(re2"\pP")
  check "+".isMatch(re2"\pS")
  check " ".isMatch(re2"\pZ")
  check raisesMsg(r"\pB") ==
    "Invalid unicode name. Found B\n" &
    "\\pB\n" &
    "^"
  check raisesMsg(r"\p11") ==
    "Invalid unicode name. Found 1\n" &
    "\\p11\n" &
    "^"
  check "a".isMatch(re2"\p{L}")
  check "ǅ".isMatch(re2"\p{Lt}")
  check(not "ǅ".isMatch(re2"\P{Lt}"))
  check(not "a".isMatch(re2"\p{Lt}"))
  check "a".isMatch(re2"\P{Lt}")
  check raisesMsg(r"\p{Bb}") ==
    "Invalid unicode name. Found Bb\n" &
    "\\p{Bb}\n" &
    "^"
  check raisesMsg(r"\p{11}") ==
    "Invalid unicode name. Expected chars in {'a'..'z', 'A'..'Z'}\n" &
    "\\p{11}\n" &
    "^"
  check raisesMsg(r"\p{11") ==
    "Invalid unicode name. Expected `}`\n" &
    "\\p{11\n" &
    "^"

test "tascii_set":
  check "d".isMatch(re2"[[:alnum:]]")
  check "5".isMatch(re2"[[:alnum:]]")
  check(not "{".isMatch(re2"[[:alnum:]]"))
  check "{".isMatch(re2"[[:alnum:]{]")
  check "-".isMatch(re2"[[:alnum:]-z]")
  check raisesMsg(r"[z-[:alnum:]]") ==
    "Invalid set range. " &
    "Start must be lesser than end\n" &
    "[z-[:alnum:]]\n" &
    "   ^"
  check "a".isMatch(re2"[[[[:alnum:]]")
  check "[".isMatch(re2"[[[:alnum:]]")
  check(not ":".isMatch(re2"[[:alnum:]]"))
  check ":".isMatch(re2"[:alnum:]")
  check(not "a".isMatch(re2"[[:^alnum:]]"))
  check "{".isMatch(re2"[[:^alnum:]]")
  check(not "5".isMatch(re2"[[:alpha:]]"))
  check(not "a".isMatch(re2"[[:digit:]]"))
  check "5".isMatch(re2"[[:alpha:][:digit:]]")
  check "a".isMatch(re2"[[:alpha:][:digit:]]")
  check raisesMsg(r"[[:abc:]]") ==
    "Invalid ascii set. `abc` is not a valid name\n" &
    "[[:abc:]]\n" &
    " ^"
  check raisesMsg(r"[[:alnum]]") ==
    "Invalid ascii set. Expected [:name:]\n" &
    "[[:alnum]]\n" &
    " ^"
  check raisesMsg(r"[[:alnum:") ==
    "Invalid ascii set. Expected [:name:]\n" &
    "[[:alnum:\n" &
    " ^"

test "treplace":
  check "a".replace(re2"(a)", "m($1)") ==
    "m(a)"
  check "a".replace(re2"(a)", "m($1) m($1)") ==
    "m(a) m(a)"
  check "aaa".replace(re2"(a*)", "m($1)") ==
    "m(aaa)m()"  # nre's behaviour
  check "abc".replace(re2"(a(b)c)", "m($1) m($2)") ==
    "m(abc) m(b)"
  check "abc".replace(re2"(a(b))(c)", "m($1) m($2) m($3)") ==
    "m(ab) m(b) m(c)"
  check "abcabc".replace(re2"(abc)*", "m($1)") ==
    "m(abc)m()"  # nre's behaviour
  check "abcabc".replace(re2"(abc)", "m($1)") ==
    "m(abc)m(abc)"
  check "abcabc".replace(re2"(abc)", "m($1)") ==
    "m(abc)m(abc)"
  check "abcab".replace(re2"(abc)", "m($1)") ==
    "m(abc)ab"
  check "abcabc".replace(re2"((abc)*)", "m($1) m($2)") ==
    "m(abcabc) m(abc)m() m()"  # nre's behaviour
  check "abcabc".replace(re2"((a)bc)*", "m($1) m($2)") ==
    "m(abc) m(a)m() m()"
  check "abc".replace(re2"(b)", "m($1)") == "am(b)c"
  check "abc".replace(re2"d", "m($1)") == "abc"
  check "abc".replace(re2"(d)", "m($1)") == "abc"
  check "aaa".replace(re2"a", "b") == "bbb"
  check "aaa".replace(re2"a", "b", 1) == "baa"
  check "Nim is awesome!".replace(re2"(\w\B)", "$1_") ==
    "N_i_m i_s a_w_e_s_o_m_e!"

  block:
    proc by(m: RegexMatch2, s: string): string =
      result = "m("
      for g in 0 ..< m.groupsCount:
        if m.group(g).a == -1:  # nonCapture
          continue
        result.add s[m.group(g)]
        result.add ','
      result.add ')'

    check "abc".replace(re2"(b)", by) == "am(b,)c"
    check "aaa".replace(re2"(a*)", by) == "m(aaa,)m(,)"
    check "aaa".replace(re2"(a)*", by) == "m(a,)m()"

  block:
    proc removeEvenWords(m: RegexMatch2, s: string): string =
      if s[m.group(1)].runeLen mod 2 != 0:
        result = s[m.group(0)]
      else:
        result = ""

    let
      text = "Es macht Spaß, alle geraden Wörter zu entfernen!"
      expected = "macht , geraden entfernen!"
    check text.replace(re2"((\w+)\s*)", removeEvenWords) == expected

# VM registry error on Nim < 1.1 (devel)
when not defined(runTestAtCT) or (NimMajor, NimMinor) > (1, 0):
  test "tmisc":
    block:
      var m: RegexMatch2
      check "abc".match(re2"[^^]+", m)
      check m.boundaries == 0 .. 2
    check(not "^".isMatch(re2"[^^]+"))
    block:
      var m: RegexMatch2
      check "kpd".match(re2"[^al-obc]+", m)
      check m.boundaries == 0 .. 2
    check(not "abc".isMatch(re2"[^al-obc]+"))
    block:
      var m: RegexMatch2
      check "almocb".match(re2"[al-obc]+", m)
      check m.boundaries == 0 .. 5
    check(not "defzx".isMatch(re2"[al-obc]+"))

    # From http://www.regular-expressions.info/examples.html
    # Grabbing HTML Tags
    block:
      var m: RegexMatch2
      check "one<TAG>two</TAG>three".find(re2"<TAG\b[^>]*>(.*?)</TAG>", m)
      check m.boundaries == 3 .. 16
    check("one<TAG>two</TAG>three".findWithCapt(
      re2"<TAG\b[^>]*>(.*?)</TAG>") == @["two"])
    # IP Addresses
    block:
      const ip = re2"""(?x)
      \b
      (25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
      \.
      (25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
      \.
      (25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
      \.
      (25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
      \b
      """
      check "127.0.0.1".findWithCapt(ip) ==
        @["127", "0", "0", "1"]
      check(not "127.0.0.999".isMatch(ip))
    # Floating Point Numbers
    block:
      var m: RegexMatch2
      check "3.14".find(re2"^[-+]?[0-9]*\.?[0-9]+$", m)
      check m.boundaries == 0 .. 3
    check "1.602e-19".findWithCapt(
      re2"^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$") == @["e-19"]
    # E-mail Addresses
    block:
      const email = re2"""(?x)
      \b
      [a-zA-Z0-9._%+-]+
      @
      (?:[a-zA-Z0-9-]+\.)+
      [a-zA-Z]{2,4}
      \b
      """
      var m: RegexMatch2
      check "john@server.department.company.com".find(email, m)
      check m.boundaries == 0 .. 33
      check(not "john@aol...com".isMatch(email))
    block:
      const email = re2"""(?x)
      [a-z0-9!#$%&'*+/=?^_`{|}~-]+
      (?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*
      @
      (?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+
      [a-z0-9](?:[a-z0-9-]*[a-z0-9])?
      """
      check "john@server.department.company.com".isMatch(email)
      check(not "john@aol...com".isMatch(email))
    block:
      const email = re2"""(?x)
      (?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+
      (?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"
      (?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|
      \\[\x01-\x09\x0b\x0c\x0e-\x7f])*"
      )@
      (?:
      (?:[a-z0-9]
      (?:[a-z0-9-]*[a-z0-9])?\.
      )+[a-z0-9]
      (?:[a-z0-9-]*[a-z0-9])?|\[
      (?:
      (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.
      ){3}
      (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:
      (?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|
      \\[\x01-\x09\x0b\x0c\x0e-\x7f])+
      )\]
      )
      """
      check "john@server.department.company.com".isMatch(email)
      check(not "john@aol...com".isMatch(email))
    # Date Validation
    block:
      const date = re2"""(?x)
      ^((?:19|20)\d\d)[- /.]
      (0[1-9]|1[012])[- /.]
      (0[1-9]|[12][0-9]|3[01])$
      """
      check "1999-01-01".findWithCapt(date) ==
        @["1999", "01", "01"]
      check "1999/01-01".findWithCapt(date) ==
        @["1999", "01", "01"]
      check(not "1999-13-33".isMatch(date))
    # Near operator emulation
    block:
      const nope = re2"\bword1\W+(?:\w+\W+){1,6}?word2\b"
      check(not "word1 word2".isMatch(nope))
      check "word1 1 word2".isMatch(nope)
      check "word1 1 2 3 4 5 6 word2".isMatch(nope)
      check(not "word1 1 2 3 4 5 6 7 word".isMatch(nope))

    # Unicode
    block:
      var m: RegexMatch2
      check "①②③".find(re2"①②③", m)
      check m.boundaries == 0 ..< "①②③".len
    block:
      var m: RegexMatch2
      check "①②③④⑤".find(re2"①②③", m)
      check m.boundaries == 0 ..< "①②③".len
    block:
      var m: RegexMatch2
      check "①②③".find(re2"①(②)③", m)
      check m.boundaries == 0 ..< "①②③".len
    check "①②③".findWithCapt(re2"①(②)③") == @["②"]
    block:
      var m: RegexMatch2
      check "①②③".find(re2"[①②③]*", m)
      check m.boundaries == 0 ..< "①②③".len
    #
    block:
      var m: RegexMatch2
      check "①②③".find(re2"[^④⑤]*", m)
      check m.boundaries == 0 ..< "①②③".len

test "tlook_around":
  check "ab".isMatch(re2"a(?=b)\w")
  check(not "ab".isMatch(re2"a(?=b)"))
  check(not "ab".isMatch(re2"a(?=c)\w"))
  check "ab".matchWithCapt(re2"(a(?=b))b") == @["a"]
  check "ab".isMatch(re2"\w(?<=a)b")
  check(not "ab".isMatch(re2"\w(?<=a)"))
  check(not "ab".isMatch(re2"\w(?<=c)b"))
  check "ab".matchWithCapt(re2"(a(?<=a))b") == @["a"]

test "tnegative_look_around":
  check "ab".isMatch(re2"a(?!c)\w")
  check(not "ab".isMatch(re2"a(?!c)"))
  check(not "ab".isMatch(re2"a(?!b)\w"))
  check "ab".matchWithCapt(re2"(a(?!c))\w") == @["a"]
  check "ab".isMatch(re2"\w(?<!c)b")
  check(not "ab".isMatch(re2"\w(?<!c)"))
  check(not "ab".isMatch(re2"\w(?<!a)b"))
  check "ab".matchWithCapt(re2"(\w(?<!c))b") == @["a"]

test "tfull_lookarounds":
  var m: RegexMatch2
  check match("ab", re2"a(?=b)\w")
  check(not match("ab", re2"a(?=x)\w"))
  check(not match("ab", re2"ab(?=x)"))
  check match("ab", re2"ab(?=$)")
  check match("abc", re2"a(?=b)\w+")
  check match("abc", re2"a(?=(bc))\w+", m) and
    m.captures == @[1 .. 2]
  check match("ab", re2"\w(?<=a)b")
  check(not match("ab", re2"\w(?<=x)b"))
  check(not match("ab", re2"(?<=x)ab"))
  check match("ab", re2"(?<=^)ab")
  check match("abc", re2"\w\w(?<=(b))c")
  check match("abc", re2"\w\w(?<=(b))c", m) and
    m.captures == @[1 .. 1]
  check match("abc", re2"\w\w(?<=(ab))c")
  check match("abc", re2"\w\w(?<=(ab))c", m) and
    m.captures == @[0 .. 1]
  check match("a", re2"\w(?<=a)")
  check(not match("a", re2"\w(?=a)"))
  check match("ab", re2"\w(?<=a(?<=a))b")
  check match("ab", re2"\w(?<=a(?<=a(?<=a)))b")
  check match("ab", re2"\w(?<=a(?=b))b")
  check(not match("ab", re2"\w(?=b(?=b))b"))  # JS, D
  check(not match("ab", re2"\w(?<=a(?=b(?=b)))b"))  # JS, D
  check match("ab", re2"\w(?<=a(?<=a)(?=b))b")
  check match("ab", re2"\w(?<=a(?<=a(?=b)))b")
  check(not match("ab", re2"\w(?<=(?<=a)a)b"))  # JS, D
  check match("aab", re2"\w\w(?<=aa(?=b))b")
  block:
    check match("aaab", re2"(\w*)(\w*?)", m) and
      m.captures == @[0 .. 3, 4 .. 3]
    check match("aaab", re2".*(?<=^(\w*)(\w*?)$)", m) and
      m.captures == @[0 .. 3, 4 .. 3]
    check match("aaab", re2".*(?<=^(\w*)(\w*)$)", m) and
      m.captures == @[0 .. -1, 0 .. 3]
    check match("aaab", re2"(\w*?)(\w*)(\w*?)", m) and
      m.captures == @[0 .. -1, 0 .. 3, 4 .. 3]
    check match("aaab", re2".*(?<=^(\w*?)(\w*)(\w*?)$)", m) and
      m.captures == @[0 .. -1, 0 .. 3, 4 .. 3]
    check match("aaab", re2"(\w*)(\w??)", m) and
      m.captures == @[0 .. 3, 4 .. 3]
    check match("aaab", re2".*(?<=^(\w*)(\w??)$)", m) and
      m.captures == @[0 .. 3, 4 .. 3]
    check match("aaab", re2".*(?<=^(\w*)(\w?)$)", m) and
      m.captures == @[0 .. 2, 3 .. 3]
    check match("aaab", re2".*(?<=^(\w*?)(\w?)(\w*?)$)", m) and
      m.captures == @[0 .. 2, 3 .. 3, 4 .. 3]
    check match("aaab", re2".*(?<=^(\w*?)(\w??)(\w*?)$)", m) and
      m.captures == @[0 .. 3, 4 .. 3, 4 .. 3]
    check match("aaab", re2".*(?<=^(\w??)(\w*?)$)", m) and
      m.captures == @[0 .. 0, 1 .. 3]
    check match("aaab", re2".*(?<=^(\w?)(\w*?)$)", m) and
      m.captures == @[0 .. 0, 1 .. 3]
    check match("aaab", re2".*(?<=^(\w*?)(\w??)$)", m) and
      m.captures == @[0 .. 3, 4 .. 3]
    check match("aaab", re2".*(?<=^(\w*)(\w??)$)", m) and
      m.captures == @[0 .. 3, 4 .. 3]
    check match("aaab", re2".*(?<=^(\w*?)(\w?)$)", m) and
      m.captures == @[0 .. 2, 3 .. 3]
    check match("aaab", re2".*(?<=^(\w*)(\w?)$)", m) and
      m.captures == @[0 .. 2, 3 .. 3]
    check match("aaab", re2".*(?<=^(\w*?)(\w*?)(\w??)$)", m) and
      m.captures == @[0 .. 3, 4 .. 3, 4 .. 3]
    check match("aaab", re2".*(?<=^(\w*?)(\w*)(\w??)$)", m) and
      m.captures == @[0 .. -1, 0 .. 3, 4 .. 3]
  block:
    check match("aaab", re2".*(?<=^(\w)\w+|(\w)\w+$)", m) and
      m.captures == @[0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(\d)\w+|(\w)\w+$)", m) and
      m.captures == @[nonCapture, 0 .. 0]
    check match("aaab", re2".*(?<=^(\w)\w+|(\w)\w+|(\w)\w+$)", m) and
      m.captures == @[0 .. 0, nonCapture, nonCapture]
    check match("aaab", re2".*(?<=^(\d)\w+|(\w)\w+|(\w)\w+$)", m) and
      m.captures == @[nonCapture, 0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(\d)\w+|(\d)\w+|(\w)\w+$)", m) and
      m.captures == @[nonCapture, nonCapture, 0 .. 0]
  block:
    check match("aaab", re2".*(?<=^(?:(\w)\w+|(\w)\w+)+?$)", m) and
      m.captures == @[0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(?:(\d)\w+|(\w)\w+)+?$)", m) and
      m.captures == @[nonCapture, 0 .. 0]
    check match("aaab", re2".*(?<=^(?:(\w)\w+|(\w)\w+|(\w)\w+)+?$)", m) and
      m.captures == @[0 .. 0, nonCapture, nonCapture]
    check match("aaab", re2".*(?<=^(?:(\d)\w+|(\w)\w+|(\w)\w+)+?$)", m) and
      m.captures == @[nonCapture, 0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(?:(\d)\w+|(\d)\w+|(\w)\w+)+?$)", m) and
      m.captures == @[nonCapture, nonCapture, 0 .. 0]
    check match("aaab", re2".*(?<=^(?:(\w)\w+|(\w)\w+)*?$)", m) and
      m.captures == @[0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(?:(\d)\w+|(\w)\w+)*?$)", m) and
      m.captures == @[nonCapture, 0 .. 0]
    check match("aaab", re2".*(?<=^(?:(\w)\w+|(\w)\w+|(\w)\w+)*?$)", m) and
      m.captures == @[0 .. 0, nonCapture, nonCapture]
    check match("aaab", re2".*(?<=^(?:(\d)\w+|(\w)\w+|(\w)\w+)*?$)", m) and
      m.captures == @[nonCapture, 0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(?:(\d)\w+|(\d)\w+|(\w)\w+)*?$)", m) and
      m.captures == @[nonCapture, nonCapture, 0 .. 0]
    check match("aaab", re2".*(?<=^(?:(\w)\w+|(\w)\w+)??$)", m) and
      m.captures == @[0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(?:(\d)\w+|(\w)\w+)??$)", m) and
      m.captures == @[nonCapture, 0 .. 0]
    check match("aaab", re2".*(?<=^(?:(\w)\w+|(\w)\w+|(\w)\w+)??$)", m) and
      m.captures == @[0 .. 0, nonCapture, nonCapture]
    check match("aaab", re2".*(?<=^(?:(\d)\w+|(\w)\w+|(\w)\w+)??$)", m) and
      m.captures == @[nonCapture, 0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(?:(\d)\w+|(\d)\w+|(\w)\w+)??$)", m) and
      m.captures == @[nonCapture, nonCapture, 0 .. 0]
  block:
    check match("aaab", re2".*(?<=^(\w)\w+?|(\w)\w+?$)", m) and
      m.captures == @[0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(\w)\w+|(\w)\w+$)", m) and
      m.captures == @[0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(\w)\w+?|(\w)\w+$)", m) and
      m.captures == @[0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(\w)\w+|(\w)\w+?$)", m) and
      m.captures == @[0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(\w+|\w{4}|\w{4}|\w+)*$)", m) and
      m.captures == @[0 .. 3]
    check match("aaab", re2"(\w*|\w{4}|\w{4}|\w*)*", m) and
      m.captures == @[4 .. 3]
    check match("aaab", re2".*(?<=^(\w*|\w{4}|\w{4}|\w*)*$)", m) and
      m.captures == @[0 .. 3]
    check match("aaab", re2".*(?<=^(\w+|\w{4}|\w{4}|\w+)+$)", m) and
      m.captures == @[0 .. 3]
    check match("aaab", re2".*(?<=^(\w)\w+|\w{4}|\w{4}|(\w)\w+$)", m) and
      m.captures == @[0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(\d)\w+|(\w{4})|(\w{4})|(\w)\w+$)", m) and
      m.captures == @[nonCapture, 0 .. 3, nonCapture, nonCapture]
    check match("aaab", re2".*(?<=^(\d)\w+|(\w)\w{3}|(\w)\w{3}|(\w)\w+$)", m) and
      m.captures == @[nonCapture, 0 .. 0, nonCapture, nonCapture]
    check match("aaab", re2".*(?<=^(\d)\w+|(\d)\w{3}|(\w)\w{3}|(\w)\w+$)", m) and
      m.captures == @[nonCapture, nonCapture, 0 .. 0, nonCapture]
    check match("aaab", re2".*(?<=^(\d)\w+|(\d)\w{3}|(\d)\w{3}|(\w)\w+$)", m) and
      m.captures == @[nonCapture, nonCapture, nonCapture, 0 .. 0]
    check match("aaab", re2"(\w*|\w{4}|\w{4}|\w*)+", m) and
      m.captures == @[4 .. 3]
    check match("aaab", re2".*(?<=^(\w*|\w{4}|\w{4}|\w*)+$)", m) and
      m.captures == @[0 .. 3]
    check match("aaab", re2".*(?<=^(\w+)|(\w{4})|(\w{10})$)", m) and
      m.captures == @[0 .. 3, nonCapture, nonCapture]
    check match("aaab", re2".*(?<=^(\w{10})|(\w{4})|(\w+)$)", m) and
      m.captures == @[nonCapture, 0 .. 3, nonCapture]
    check match("aaab", re2".*(?<=^((\w{10})|(\w{4})|(\w+))+$)", m) and
      m.captures == @[0 .. 3, nonCapture, 0 .. 3, nonCapture]
    check match("aaab", re2".*(?<=^((\w{10})|(\w{4})|(\w+))*$)", m) and
      m.captures == @[0 .. 3, nonCapture, 0 .. 3, nonCapture]
    check match("aaab", re2".*(?<=^(\w+|\w{4}|\w{4}|\w+)*?$)", m) and
      m.captures == @[0 .. 3]
    check match("aaab", re2".*(?<=^(\w*|\w{4}|\w{4}|\w*)*?$)", m) and
      m.captures == @[0 .. 3]
    check match("aaab", re2".*(?<=^(\w+|\w{4}|\w{4}|\w+)+?$)", m) and
      m.captures == @[0 .. 3]
  check findAllBounds(r"1abab", re2"(?<=\d\w*)ab") ==
    @[1 .. 2, 3 .. 4]
  check findAllBounds(r"abab", re2"(?<=\d\w*)ab").len == 0
  check findAllBounds(r"abab1", re2"ab(?=\w*\d)") ==
    @[0 .. 1, 2 .. 3]
  check findAllBounds(r"abab", re2"ab(?=\w*\d)").len == 0
  check findAllBounds("foo\nbar\nbar", re2"bar(?=$)") ==
    @[8 .. 10]
  check findAllBounds("foo\nbar\nbar", re2"(?m)bar(?=$)") ==
    @[4 .. 6, 8 .. 10]
  check findAllBounds("bar\nfoo\nbar", re2"(?<=^)bar") ==
    @[0 .. 2]
  check findAllBounds("bar\nfoo\nbar", re2"(?m)(?<=^)bar") ==
    @[0 .. 2, 8 .. 10]
  block:
    # There is a difference in how nesting is
    # handled by JS vs D; in D all of them start
    # from the outermost one, while in JS they
    # start from the outer one/parent. I think the JS way
    # is less mind blending, and it makes more sense to me
    #
    # These 2 match in D, but not in JS
    check(not match("ab", re2"\w(?<=a(?=b(?<=a)))b"))  # JS, !D
    check(not match("ab", re2"\w(?<=a(?<=a(?=b(?<=a))))b"))  # JS, !D
    # These 2 match in JS, but not in D
    check match("ab", re2"\w(?<=a(?=b(?<=b)))b")  # JS, !D
    check match("ab", re2"\w(?<=a(?<=a(?=b(?<=b))))b")  # JS, !D
  block:
    let asterisk = re2"(?<=(?<!\\)(?:\\\\)*)\*"
    check findAllBounds(r"*foo*", asterisk) ==
      @[0 .. 0, 4 .. 4]
    check findAllBounds(r"\*foo\*", asterisk).len == 0
    check findAllBounds(r"\\*foo\\*", asterisk) ==
      @[2 .. 2, 8 .. 8]
    check findAllBounds(r"\\\*foo\\\*", asterisk).len == 0
    check findAllBounds(r"\\\\*foo\\\\*", asterisk) ==
      @[4 .. 4, 12 .. 12]
    check findAllBounds(r"\\\\\*foo\\\\\*", asterisk).len == 0
  block:
    let asterisk = re2".*(?<=(?<!\\)(?:\\\\)*)\*"
    check match(r"*", asterisk)
    check(not match(r"\*", asterisk))
    check match(r"\\*", asterisk)
    check(not match(r"\\\*", asterisk))
    check match(r"\\\\*", asterisk)
    check(not match(r"\\\\\*", asterisk))
  block:
    check findAllBounds("foobarbaz", re2"(?<=o)b") == @[3 .. 3]
    check findAllBounds("foobar", re2"o(?=b)") == @[2 .. 2]
    check findAllBounds("100 in USD100", re2"(?<=USD)\d{3}") ==
      @[10 .. 12]
    check findAllBounds("100 in USD100", re2"\d{3}(?<=USD\d{3})") ==
      @[10 .. 12]
  block:
    check match("弢b", re2"弢(?=b)\w")
    check match("a弢", re2"a(?=弢)\w")
    check match("Ⓐb", re2"Ⓐ(?=b)\w")
    check match("aⒶ", re2"a(?=Ⓐ)\w")
    check match("Ⓐb", re2"Ⓐ(?=b)\w")
    check match("aΪ", re2"a(?=Ϊ)\w")
    check match("Ϊb", re2"Ϊ(?=b)\w")
    check match("弢Ⓐ", re2"弢(?=Ⓐ)\w")
  block:
    check match("a弢", re2"\w(?<=a)弢")
    check match("弢b", re2"\w(?<=弢)b")
    check match("aⒶ", re2"\w(?<=a)Ⓐ")
    check match("Ⓐb", re2"\w(?<=Ⓐ)b")
    check match("aΪ", re2"\w(?<=a)Ϊ")
    check match("Ϊb", re2"\w(?<=Ϊ)b")
    check match("弢Ⓐ", re2"\w(?<=弢)Ⓐ")
  block:  # Follows Nim re's behaviour
    check(not match("abc", re2"^bc", m, start = 1))
    check match("abc", re2"(?<=a)bc", m, start = 1)
    check(not match("abc", re2"(?<=x)bc", m, start = 1))
  block:
    check match("abcdefg", re2"\w+(?<=(ab)cd(?<=(cd)))\w+", m) and
      m.captures == @[0 .. 1, 2 .. 3]
    check match("abcdefg", re2"\w+(?<=(ab)(?=(cd)))\w+", m) and
      m.captures == @[0 .. 1, 2 .. 3]
    check match("abcdefg", re2"\w+(?<=(ab)(?=(cd)(?<=(cd))))\w+", m) and
      m.captures == @[0 .. 1, 2 .. 3, 2 .. 3]
    check match("abcdefg", re2"\w+(?=(cd)(?<=(cd)))\w+", m) and
      m.captures == @[2 .. 3, 2 .. 3]

test "tpretty_errors":
  block:
    var exp = ""
    for _ in 0 ..< 30:
      exp.add('x')
    exp.add("(?Pabc")
    check raisesMsg(exp) ==
      "Invalid group name. Missing `<`\n" &
      "~16 chars~xxxxxxxxxxxxxx(?Pabc\n" &
      "                        ^"
  block:
    var exp = "(?Pabc"
    for _ in 0 ..< 30:
      exp.add('x')
    check raisesMsg(exp) ==
      "Invalid group name. Missing `<`\n" &
      "(?Pabcxxxxxxxxxxxxxxxxxxxxxxxx~6 chars~\n" &
      "^"
  block:
    var exp = ""
    for _ in 0 ..< 30:
      exp.add('x')
    exp.add("(?Pabc")
    for _ in 0 ..< 30:
      exp.add('x')
    check raisesMsg(exp) ==
      "Invalid group name. Missing `<`\n" &
      "~16 chars~xxxxxxxxxxxxxx(?Pabcxxxxxxxxxx~20 chars~\n" &
      "                        ^"
  check(raisesMsg(r"""(?x)  # comment
      (?Pabc  # comment
      # comment""") ==
    "Invalid group name. Missing `<`\n" &
    "~8 chars~comment       (?Pabc  # commen~17 chars~\n" &
    "                       ^")
  check raisesMsg(r"aaa(?Pabc") ==
    "Invalid group name. Missing `<`\n" &
    "aaa(?Pabc\n" &
    "   ^"
  check raisesMsg(r"(?Pabc") ==
    "Invalid group name. Missing `<`\n" &
    "(?Pabc\n" &
    "^"
  # unicode chars may have a wider width,
  # so better to just truncate them
  check raisesMsg(r"弢(?Pabc") ==
    "Invalid group name. Missing `<`\n" &
    "~1 chars~(?Pabc\n" &
    "         ^"
  check raisesMsg(r"弢弢弢(?Pabc") ==
    "Invalid group name. Missing `<`\n" &
    "~3 chars~(?Pabc\n" &
    "         ^"
  check raisesMsg(r"弢aaa(?Pabc") ==
    "Invalid group name. Missing `<`\n" &
    "~4 chars~(?Pabc\n" &
    "         ^"

test "treuse_regex_match":
  block:
    var m: RegexMatch2
    check "2222".find(re2"(22)*", m)
    check m.group(0) == 2 .. 3

    check "abcd".find(re2"(ab)", m)
    check m.group(0) == 0 .. 1

    check "abcd".find(re2"(bc)", m)
    check m.group(0) == 1 .. 2

    check "abcd".find(re2"(cd)", m)
    check m.group(0) == 2 .. 3

  block:
    var m: RegexMatch2
    check "foobar".match(re2"(?P<foo>(?P<bar>\w*))", m)
    check m.group("foo") == 0..5
    check m.group("bar") == 0..5

    check "foobar".match(re2"(?P<foo>\w*)", m)
    check m.group("foo") == 0..5
    expect(ValueError):
      discard m.group("bar")

test "tisInitialized":
  block:
    var re: Regex2
    check(not re.isInitialized)
    re = re2"foo"
    check re.isInitialized

test "capturingGroupsNames":
  block:
    let text = "hello world"
    var m: RegexMatch2
    check text.match(re2"(?P<greet>hello) (?P<who>world)", m)
    check m.groupsCount == 2
    for name in @["greet", "who"]:
      check m.groupNames.contains(name)
  block:
    let text = "hello world"
    var m: RegexMatch2
    check text.match(re2"(?P<greet>hello) (?P<who>world)", m)
    check m.group("greet", text) == "hello"
    check m.group("who", text) == "world"
  block:
    let text = "hello world foo bar"
    var m: RegexMatch2
    check text.match(re2"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    check m.group("greet", text) == "hello"
    check m.group("who", text) == "bar"
  block:
    let text = "hello world her"
    var m: RegexMatch2
    check text.match(re2"(?P<greet>hello) (?P<who>world) (?P<who>her)", m)
    check m.group("who", text) == "her"
  block:
    let text = "hello world foo bar"
    var m: RegexMatch2
    check text.match(re2"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    check m.group("who", text) == "bar"
  block:
    let text = "hello"
    var m: RegexMatch2
    check text.match(re2"(?P<greet>hello)\s?(?P<who>world)?", m)
    check m.group("greet", text) == "hello"
    check m.group("who", text) == ""
  block:
    let text = "hello"
    var m: RegexMatch2
    check text.match(re2"(hello)\s?(world)?", m)
    check m.group(0, text) == "hello"
    check m.group(1, text) == ""
  block:
    let text = ""
    var m: RegexMatch2
    check text.match(re2"(hello)?\s?(world)?", m)
    check m.group(0, text) == ""
    check m.group(1, text) == ""
  block:
    let text = "hello world foo bar"
    var m: RegexMatch2
    check text.match(re2"(hello) (?:([^\s]+)\s?)+", m)
    check m.group(1, text) == "bar"

# XXX raise a compile error when regex contains unicode
#     in ascii mode
test "tflags":
  var m: RegexMatch2
  #check match("abc", re(r"abc", {reAscii}), m)
  check match("弢弢弢", re2"\w{3}", m)
  #check(not match("弢弢弢", re(r"\w{3}", {reAscii}), m))
  check re2"\w" in "弢"
  #check re(r"\w", {reAscii}) notin "弢"
  #check re(r"\w", {reAscii}) in "a"
  #check "%ab%".find(re(r"\w{2}", {reAscii}), m)
  check "%弢弢%".find(re2"\w{2}", m)
  #check(not "%弢弢%".find(re(r"\w{2}", {reAscii}), m))

test "tfindopt":
  var m: RegexMatch2
  check(not find("bar", re2"foo", m))
  check(not find("bar", re2"baz", m))
  check "abcd".find(re2"bc", m)
  check m.boundaries == 1 .. 2
  check "bcd".find(re2"bc", m)
  check m.boundaries == 0 .. 1
  check "bc".find(re2"bc", m)
  check m.boundaries == 0 .. 1
  check "ababcd".find(re2"bc", m)
  check m.boundaries == 3 .. 4
  check "abc@xyz".find(re2"\w@", m)
  check m.boundaries == 2 .. 3
  check "ab1c@xyz".find(re2"\d\w@", m)
  check m.boundaries == 2 .. 4
  check "##axyz##".find(re2"(a|b)xyz", m)
  check m.boundaries == 2 .. 5
  check "##bxyz##".find(re2"(a|b)xyz", m)
  check m.boundaries == 2 .. 5
  check "##x#ax#axy#bxyz##".find(re2"(a|b)xyz", m)
  check m.boundaries == 11 .. 14
  check "##z#xyz#yz#bxyz##".find(re2"(a|b)xyz", m)
  check m.boundaries == 11 .. 14
  check "#xabcx#abc#".find(re2"\babc\b", m)
  check m.boundaries == 7 .. 9
  check "#foo://#".find(re2"[\w]+://", m)
  check m.boundaries == 1 .. 6
  check "x#foo://#".find(re2"[\w]+://", m)
  check m.boundaries == 2 .. 7

test "tfindallopt":
  check findAllBounds("bar", re2"foo").len == 0
  check findAllBounds("bar", re2"baz").len == 0
  check findAllBounds("abcd", re2"bc") ==
    @[1 .. 2]
  check findAllBounds("bcd", re2"bc") ==
    @[0 .. 1]
  check findAllBounds("bc", re2"bc") ==
    @[0 .. 1]
  check findAllBounds("ababcd", re2"bc") ==
    @[3 .. 4]
  check findAllBounds("abcdbcbc", re2"bc") ==
    @[1 .. 2, 4 .. 5, 6 .. 7]
  check findAllBounds("abc@xyz", re2"\w@") ==
    @[2 .. 3]
  check findAllBounds("abc@xyz@@", re2"\w@") ==
    @[2 .. 3, 6 .. 7]
  check findAllBounds("ab1c@xyz", re2"\d\w@") ==
    @[2 .. 4]
  check findAllBounds("ab1c@xy1z@z@", re2"\d\w@") ==
    @[2 .. 4, 7 .. 9]
  check findAllBounds("##axyz##", re2"(a|b)xyz") ==
    @[2 .. 5]
  check findAllBounds("##bxyz##", re2"(a|b)xyz") ==
    @[2 .. 5]
  check findAllBounds("##axyz##bxyz", re2"(a|b)xyz") ==
    @[2 .. 5, 8 .. 11]
  check findAllBounds("##x#ax#axy#bxyz##", re2"(a|b)xyz") ==
    @[11 .. 14]
  check findAllBounds("##z#xyz#yz#bxyz##", re2"(a|b)xyz") ==
    @[11 .. 14]
  check findAllBounds("#xabcx#abc#", re2"\babc\b") ==
    @[7 .. 9]
  check findAllBounds("#xabcx#abc#abc", re2"\babc\b") ==
    @[7 .. 9, 11 .. 13]
  check findAllBounds("#foo://#", re2"[\w]+://") ==
    @[1 .. 6]
  check findAllBounds("x#foo://#", re2"[\w]+://") ==
    @[2 .. 7]
  check findAllBounds("foobarbaz", re2"(?<=o)b") ==
    @[3 .. 3]
  check findAllBounds("foobarbaz", re2"(?<=r)b") ==
    @[6 .. 6]
  check findAllBounds("foobarbaz", re2"(?<!o)b") ==
    @[6 .. 6]
  check findAllBounds("foobarbaz", re2"(?<!r)b") ==
    @[3 .. 3]
  check findAllBounds("x@x@y@y@y@x@xy@yx@@", re2"(?<!x)@\w+") ==
    @[5 .. 6, 7 .. 8, 9 .. 10, 14 .. 16]
  check findAllBounds("foobar", re2"o(?=b)") ==
    @[2 .. 2]
  check findAllBounds("foobarbaz", re2"a(?=r)") ==
    @[4 .. 4]
  check findAllBounds("foobar", re2"o(?!b)") ==
    @[1 .. 1]
  check findAllBounds("foobarbaz", re2"a(?!r)") ==
    @[7 .. 7]
  check findAllBounds("x@x@y@y@y@x@xy@yx@@", re2"\w+@(?!x)") ==
    @[2 .. 3, 4 .. 5, 6 .. 7, 12 .. 14, 15 .. 17]
  check findAllBounds("abcdef", re2"^abcd") ==
    @[0 .. 3]
  check findAllBounds("abcdef", re2"cdef$") ==
    @[2 .. 5]
  check findAllBounds("abcdef", re2"^abcdef$") ==
    @[0 .. 5]
  check findAllBounds("abcdef", re2"^abcx").len == 0
  check findAllBounds("abcdef", re2"xcdef$").len == 0
  check findAllBounds("abc\nabc\na", re2"(?m)^a") ==
    @[0 .. 0, 4 .. 4, 8 .. 8]
  check findAllBounds("x\nabcxx\nabcxx", re2"(?m)x$\n?[^x]*") ==
    @[0 .. 4, 6 .. 10, 12 .. 12]
  check findAllBounds("#f1o2o3@bar#", re2"(\w\d)*?@\w+") ==
    @[1 .. 10]
  check findAllBounds("foo@bar@baz", re2"\w+@\w+") ==
    @[0 .. 6]
  check findAllBounds("foo@111@111", re2"\w+@\d+") ==
    @[0 .. 6]
  check findAllBounds("foo@111@111", re2"\w*@\d+") ==
    @[0 .. 6, 7 .. 10]
  check findAllBounds("foo@111@@111", re2"\w+@\d+") ==
    @[0 .. 6]
  check findAllBounds("foo@111foo@111", re2"\w+@\d+") ==
    @[0 .. 6, 7 .. 13]
  check findAllBounds("111@111@111", re2"\d+@\w+") ==
    @[0 .. 6]
  check findAllBounds("abbbbccccd@abbbbccccd@", re2"\w(b|c)*d@") ==
    @[0 .. 10, 11 .. 21]
  check findAllBounds("abXabX", re2"\w(b|c)*@").len == 0
  check findAllBounds("abbcXabbcX", re2"((a(b)*)+(c))") ==
    @[0 .. 3, 5 .. 8]
  check findAllBounds("aaanasdnasd@aaanasdnasd@", re2"((a)*n?(asd)*)+@") ==
    @[0 .. 11, 12 .. 23]
  check findAllBounds("a1@a1a1@", re2"(\w\d)+@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@a@a", re2"(\w\d)+@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@a@a1a@1@a@a1@", re2"(\w\d)+@") ==
    @[0 .. 2, 3 .. 7, 18 .. 20]
  check findAllBounds("a1@a1a1@", re2"(\w\d)*@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@a@a", re2"(\w\d)*@") ==
    @[0 .. 2, 3 .. 7, 9 .. 9]
  check findAllBounds("a1@a1a1@", re2"(\w\d)+?@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@", re2"(\w\d)*?@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@", re2"\w+\d+?@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@", re2"\w+?\d+@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@", re2"\w+?\d+?@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("ab@&%", re2"(a|ab)\w@&%") ==
    @[0 .. 4]
  check findAllBounds("abc@&%", re2"(a|ab)\w@&%") ==
    @[0 .. 5]
  check findAllBounds("ab@&%abc@&%", re2"(a|ab)\w@&%") ==
    @[0 .. 4, 5 .. 10]
  check findAllCapt("ab@&%", re2"(?:(a)|(ab))\w@&%") ==
    @[@[0 .. 0, nonCapture]]
  check findAllCapt("a#b@&%", re2"(?:(a)|(a#))\w@&%") ==
    @[@[nonCapture, 0 .. 1]]
  check findAllCapt("abc@&%", re2"(?:(a)|(ab))\w@&%") ==
    @[@[nonCapture, 0 .. 1]]
  check findAllCapt("aba@&%", re2"(?:(ab)|(a))\w@&%") ==
    @[@[0 .. 1, nonCapture]]
  check findAllCapt("ab@&%abc@&%", re2"(?:(a)|(ab))\w@&%") ==
    @[@[0 .. 0, nonCapture], @[nonCapture, 5 .. 6]]
  check findAllCapt("abc@&%ab@&%", re2"(?:(a)|(ab))\w@&%") ==
    @[@[nonCapture, 0 .. 1], @[6 .. 6, nonCapture]]
  check findAllBounds("x@xxzx@xxz", re2"\w+@\w+(?=z)") == @[0 .. 3, 4 .. 8]
  check findAllBounds("1x@11zx@xxz", re2"\d+\w+@\w+(?=z)") == @[0 .. 4]
  check findAllBounds("1x@11zx@xxz", re2"\d+\w+@\w+") == @[0 .. 6]
  check findAllBounds("1x@1x@1x", re2"\d+\w+@\w+") == @[0 .. 4]
  check findAllBounds("1x@1x@1x", re2"\d+\w+@(\d\w)+") == @[0 .. 4]
  check findAllBounds("2222", re2"22") == @[0 .. 1, 2 .. 3]
  block overlapTests:
    check findAllBounds("1x@1xx@1x", re2"\d+\w+@(1\w)+") == @[0 .. 4]
    check findAllBounds("1x@1x1xx@1x", re2"\d+\w+@(1\w)+") == @[0 .. 6]
    check findAllBounds("1x@1xx1x@1x", re2"\d+\w+@(1\w)+") == @[0 .. 4, 6 .. 10]
    check findAllBounds("1x@1xx@1x", re2"\d\w+@(\d\w)+") == @[0 .. 4]
    check findAllBounds("2x1x@xx", re2"(1\w)+@\w+") == @[2 .. 6]
    check findAllBounds("2x1x1x@xx", re2"(1\w)+@\w+") == @[2 .. 8]
  check findAllBounds("bcbc#bc", re2"\bbc\B") == @[0 .. 1]
  check findAllBounds("bcbc#bca", re2"\bbc\B") == @[0 .. 1, 5 .. 6]
  check findAllBounds("bcbc#bc#xbcx#bcbcbc#bc", re2"\bbc\B") ==
    @[0 .. 1, 13 .. 14]
  check findAllBounds("bcbc#bc#xbcx#bcbcbc#bc", re2"\Bbc\b") ==
    @[2 .. 3, 17 .. 18]
  check findAllBounds("bcbc", re2"\Bbc") == @[2 .. 3]
  check findAllBounds("bcbcbc", re2"\Bbc") == @[2 .. 3, 4 .. 5]
  check findAllBounds("bc#bc#xbcx", re2"\Bbc\B") == @[7 .. 8]
  check findAllBounds("bcbc#bca", re2"\Bbc\b") == @[2 .. 3]
  check findAllBounds("bcbc#bc", re2"\Bbc\b") == @[2 .. 3]
  check findAllBounds("abcabc", re2"bc") == @[1 .. 2, 4 .. 5]
  check findAllBounds("bcbc#bc", re2"\bbc\b") == @[5 .. 6]
  check findAllBounds("bcbc", re2"\bbc") == @[0 .. 1]
  check findAllBounds("bc#bc", re2"\bbc\b") == @[0 .. 1, 3 .. 4]
  check findAllBounds("bcabc", re2"^bc") == @[0 .. 1]
  check findAllBounds("bcbc", re2"^bc") == @[0 .. 1]
  check findAllBounds("bcabc\nbc\nabc\nbcbc", re2"(?m)^bc") ==
    @[0 .. 1, 6 .. 7, 13 .. 14]
  check findAllBounds("弢@弢@弢", re2"\w+@\w+") == @[0 .. 8]
  check findAllBounds("弢ⒶΪ@弢ⒶΪ@弢ⒶΪ", re2"\w+@\w+") == @[0 .. 18]
  check findAllBounds("۲@弢۲⅕@弢", re2"\d+@\w+") == @[0 .. 8]
  check findAllBounds("۲۲@弢ⒶΪ11@弢ⒶΪ", re2"\d+@弢ⒶΪ") ==
    @[0 .. 13, 14 .. 25]
  check findAllBounds("#xa弢ⒶΪx#a弢ⒶΪ#", re2"\ba弢ⒶΪ\b") == @[14 .. 23]
  check findAllBounds("#xa弢ⒶΪx#a弢ⒶΪ#a弢ⒶΪ", re2"\ba弢ⒶΪ\b") ==
    @[14 .. 23, 25 .. 34]
  check findAllBounds("۲Ⓐ@۲弢Ϊ@۲弢", re2"\d+\w+@(۲\w)+") == @[0 .. 11]
  check findAllBounds("۲弢@۲弢۲ΪΪ@۲弢", re2"\d+\w+@(۲\w)+") == @[0 .. 16]
  check findAllBounds("۲Ϊ@۲弢Ⓐ۲Ⓐ@۲弢", re2"\d+\w+@(۲\w)+") ==
    @[0 .. 10, 14 .. 25]

test "tmisc2":
  var m: RegexMatch2
  check "one<TAG>two</TAG>tree".find(re2"<TAG>.*?</TAG>", m)
  check m.boundaries == 3 .. 16
  check "one<TAG>two</TAG>tree".find(re2"<TAG>[\w<>/]*?</TAG>", m)
  check m.boundaries == 3 .. 16
  check "<TAG>two</TAG>".match(re2"<TAG>.*?</TAG>", m)
  check match("abc", re2"abc", m)
  check match("ab", re2"a(b|c)", m)
  check match("ac", re2"a(b|c)", m)
  check(not match("ad", re2"a(b|c)", m))
  check match("ab", re2"(ab)*", m)
  check match("abab", re2"(ab)*", m)
  check(not match("ababc", re2"(ab)*", m))
  check(not match("a", re2"(ab)*", m))
  check match("ab", re2"(ab)+", m)
  check match("abab", re2"(ab)+", m)
  check(not match("ababc", re2"(ab)+", m))
  check(not match("a", re2"(ab)+", m))
  check match("aa", re2"\b\b\baa\b\b\b", m)
  check(not match("cac", re2"c\ba\bc", m))
  check match("abc", re2"[abc]+", m)
  check match("abc", re2"[\w]+", m)
  check match("弢弢弢", re2"[\w]+", m)
  check(not match("abc", re2"[\d]+", m))
  check match("123", re2"[\d]+", m)
  check match("abc$%&", re2".+", m)
  check(not match("abc$%&\L", re2"(.+)", m))
  check(not match("abc$%&\L", re2".+", m))
  check(not match("弢", re2"\W", m))
  check match("$%&", re2"\W+", m)
  check match("abc123", re2"[^\W]+", m)
  check match("aabcd", re2"(aa)bcd", m) and
    m.captures == @[0 .. 1]
  check match("aabc", re2"(aa)(bc)", m) and
    m.captures == @[0 .. 1, 2 .. 3]
  check match("ab", re2"a(b|c)", m) and
    m.captures == @[1 .. 1]
  check match("ab", re2"(ab)*", m) and
    m.captures == @[0 .. 1]
  check match("abab", re2"(ab)*", m) and
    m.captures == @[2 .. 3]
  check match("ab", re2"((a))b", m) and
    m.captures == @[0 .. 0, 0 .. 0]
  check match("c", re2"((ab)*)c", m) and
    m.captures == @[0 .. -1, nonCapture]
  check match("aab", re2"((a)*b)", m) and
    m.captures == @[0 .. 2, 1 .. 1]
  check match("abbbbcccc", re2"a(b|c)*", m) and
    m.captures == @[8 .. 8]
  check match("ab", re2"(a*)(b*)", m) and
    m.captures == @[0 .. 0, 1 .. 1]
  check match("ab", re2"(a)*(b)*", m) and
    m.captures == @[0 .. 0, 1 .. 1]
  check match("ab", re2"(a)*b*", m) and
    m.captures == @[0 .. 0]
  check match("abbb", re2"((a(b)*)*(b)*)", m) and
    m.captures == @[0 .. 3, 0 .. 3, 3 .. 3, nonCapture]
  check match("aa", re2"(a)+", m) and
    m.captures == @[1 .. 1]
  check match("abab", re2"(ab)+", m) and
    m.captures == @[2 .. 3]
  check match("a", re2"(a)?", m) and
    m.captures == @[0 .. 0]
  check match("ab", re2"(ab)?", m) and
    m.captures == @[0 .. 1]
  check match("aaabbbaaa", re2"(a*|b*)*", m) and
    m.captures == @[9 .. 8]
  check match("abab", re2"(a(b))*", m) and
    m.captures == @[2 .. 3, 3 .. 3]
  check match("aaanasdnasd", re2"((a)*n?(asd)*)*", m) and
    m.captures == @[11 .. 10, 2 .. 2, 8 .. 10]
  check match("aaanasdnasd", re2"((a)*n?(asd))*", m) and
    m.captures == @[7 .. 10, 2 .. 2, 8 .. 10]

test "tmisc2_5":
  var m: RegexMatch2
  check match("abd", re2"((ab)c)|((ab)d)", m) and
    m.captures == @[nonCapture, nonCapture, 0 .. 2, 0 .. 1]
  check match("aaa", re2"(a*)", m) and
    m.captures == @[0 .. 2]
  check match("aaaa", re2"(a*)(a*)", m) and
    m.captures == @[0 .. 3, 4 .. 3]
  check match("aaaa", re2"(a*?)(a*?)", m) and
    m.captures == @[0 .. -1, 0 .. 3]
  check match("aaaa", re2"(a)*(a)", m) and
    m.captures == @[2 .. 2, 3 .. 3]
  check "11222211".find(re2"(22)+", m) and
    m.group(0) == 4 .. 5
  check match("650-253-0001", re2"[0-9]+-[0-9]+-[0-9]+", m)
  check(not match("abc-253-0001", re2"[0-9]+-[0-9]+-[0-9]+", m))
  check(not match("650-253", re2"[0-9]+-[0-9]+-[0-9]+", m))
  check(not match("650-253-0001-abc", re2"[0-9]+-[0-9]+-[0-9]+", m))
  check match("650-253-0001", re2"[0-9]+..*", m)
  check(not match("abc-253-0001", re2"[0-9]+..*", m))
  check(not match("6", re2"[0-9]+..*", m))
  # VM registry error on Nim < 1.1 (devel)
  when not defined(runTestAtCT) or (NimMajor, NimMinor) > (1, 0):
    block:
      const re1 = re2"((11)*)+(111)*"
      check match("", re1)
      check match("11", re1)
      check match("111", re1)
      check match("11111", re1)
      check match("1111111", re1)
      check match("1111111111", re1)
      check(not match("1", re1))
    block:
      const re1 = re2"(11)+(111)*"
      check(not match("", re1))
      check match("11", re1)
      check(not match("111", re1))
      check match("11111", re1)

test "tmisc2_6":
  block:
    const re1 = re2"(aabb)(ab)*"
    check match("aabb", re1)
    check match("aabbab", re1)
    check match("aabbabab", re1)
    check(not match("ab", re1))
    check(not match("aabbaba", re1))
  block:
    const re1 = re2"0(10)*"
    check match("0", re1)
    check match("010", re1)
    check(not match("", re1))
    check(not match("0101", re1))
    check(not match("0100", re1))
    check(not match("00", re1))
    check(not match("000", re1))
  block:
    const re1 = re2"(11)*|(111)*"
    check match("", re1)
    check match("11", re1)
    check match("111", re1)
    check match("1111", re1)
    check match("111111", re1)
    check(not match("1", re1))

test "tmisc3":
  var m: RegexMatch2
  block:  # issue #61
    const a = "void __mingw_setusermatherr (int (__attribute__((__cdecl__)) *)(struct _exception *));"
    check replace(a, re2"__attribute__[ ]*\(\(.*?\)\)([ ,;])", "$1") ==
      "void __mingw_setusermatherr (int ( *)(struct _exception *));"
    check replace(a, re2"__attribute__[ ]*\(\(.*?\)\)(.*?[ ,;])", "$1") ==
      "void __mingw_setusermatherr (int ( *)(struct _exception *));"
    check find(a, re2"__attribute__[ ]*\(\(.*?\)\)([ ,;])", m) and
      a[m.boundaries] == "__attribute__((__cdecl__)) "
    check find(a, re2"__attribute__[ ]*\(\(.*?\)\)(.*?[ ,;])", m) and
      a[m.boundaries] == "__attribute__((__cdecl__)) "
    # non-greedy
    check find(a, re2"__attribute__[ ]*\(\(.*\)\)([ ,;])", m) and
      a[m.boundaries] == "__attribute__((__cdecl__)) *)(struct _exception *));"
  # issue #29
  check replace("foo", re2"", "-") == "-f-o-o-"
  block:  # issue #13
    const input = """foo
              bar
      baxx
                bazz
    """
    const expected = """//foo
//              bar
//      baxx
//                bazz
//    """
    check replace(input, re2"(?m)^", "//") == expected
  check replace("bar", re2"^", "foo") == "foobar"
  check replace("foo", re2"$", "bar") == "foobar"
  block:
    const input = """foo
              bar
      baxx
                bazz
    """
    const expected = """foo//
              bar//
      baxx//
                bazz//
    //"""
    check replace(input, re2"(?m)$", "//") == expected
  check(not find("foobarbar", re2"^bar", m, start=3))
  check find("foobar\nbar", re2"(?m)^bar", m, start=3) and
    m.boundaries == 7 .. 9
  check find("foo\nbar\nbar", re2"(?m)^bar", m, start=3) and
    m.boundaries == 4 .. 6
  check find("foo\nbar\nbar", re2"(?m)^bar", m, start=4) and
    m.boundaries == 4 .. 6
  block:
    # The bounds must contain the empty match index
    check find("foo\nbar\nbar", re2"(?m)^", m) and
      m.boundaries == 0 .. -1
    check find("foo\nbar\nbar", re2"(?m)^", m, start=1) and
      m.boundaries == 4 .. 3
    check find("foo\nbar\nbar", re2"(?m)^", m, start=4) and
      m.boundaries == 4 .. 3
    check find("foo\nbar\nbar", re2"(?m)^", m, start=5) and
      m.boundaries == 8 .. 7
    check find("foo\nbar\nbar", re2"(?m)^", m, start=8) and
      m.boundaries == 8 .. 7
    check(not find("foo\nbar\nbar", re2"(?m)^", m, start=9))
    check find("foo\nbar\nbar", re2"(?m)$", m) and
      m.boundaries == 3 .. 2
    check find("foo\nbar\nbar", re2"(?m)$", m, start=3) and
      m.boundaries == 3 .. 2
    check find("foo\nbar\nbar", re2"(?m)$", m, start=4) and
      m.boundaries == 7 .. 6
    check find("foo\nbar\nbar", re2"(?m)$", m, start=7) and
      m.boundaries == 7 .. 6
    check find("foo\nbar\nbar", re2"(?m)$", m, start=8) and
      m.boundaries == 11 .. 10
    check find("foo\nbar\nbar", re2"(?m)$", m, start=11) and
      m.boundaries == 11 .. 10
    # start is out of bounds, but this is what Nim's re
    # does, nre throws an error
    #check find("foo\nbar\nbar", re2"(?m)$", m, start=12) and
    #  m.boundaries == 12 .. 11
  # XXX make this return false?
  check match("abc", re2"(?m)$", m, start=50)
  check match("abc", re2"(?m)^", m, start=50)
  # this should be valid though
  check match("", re2"(?m)^", m)
  check match("", re2"(?m)$", m)
  block:
    const input = """foo
              bar
      baxx
                bazz
    """
    const expected = @[
      "foo\n",
      "              bar\n",
      "      baxx\n",
      "                bazz\n",
      "    "
    ]
    check split(input, re2"(?m)^") == expected
  block:
    const input = """foo
              bar
      baxx
                bazz
    """
    const expected = @[
      "foo",
      "\n              bar",
      "\n      baxx",
      "\n                bazz",
      "\n    "
    ]
    check split(input, re2"(?m)$") == expected
  check split("acb\nb\nc\n", re2"(?m)^") == @["acb\n", "b\n", "c\n"]
  check split("a b", re2"\b") == @["a", " ", "b"]
  check split("ab", re2"\b") == @["ab"]
  check split("iaiaiai", re2"i") == @["", "a", "a", "a", ""]
  check split("aiaia", re2"i") == @["a", "a", "a"]
  check split("aaa", re2"a") == @["", "", "", ""]
  check split("a\na\na", re2"(?m)^") == @["a\n", "a\n", "a"]
  check split("\n\n", re2"(?m)^") == @["\n", "\n"]
  check split("foobar", re2"(?<=o)b") == @["foo", "ar"]
  check split("foobarbaz", re2"(?<!o)b") == @["foobar", "az"]
  check split("foobar", re2"o(?=b)") == @["fo", "bar"]
  check split("foobar", re2"o(?!b)") == @["f", "obar"]
  block:
    var m: RegexMatch2
    check find("abcxyz", re2"(abc)|\w+", m)
    check m.boundaries == 0 .. 2
    check find("xyzabc", re2"(abc)|\w+", m)
    check m.boundaries == 0 .. 5
  check findAllStr(
    "He was carefully disguised but captured quickly by police.",
    re2"\w+ly") == @["carefully", "quickly"]

test "fix#83":
  block:
    let pattern = "^src/(?:[^\\/]*(?:\\/|$))*[^/]*\\.nim$"
    check "src/dir/foo.nim".match(pattern.re)
  check match("foo", re2"(?:$)*\w*")
  check match("foo", re2"($)*\w*")
  check match("foo", re2"^*\w*")
  check match("foo", re2"([^/]*$)*\w*")
  check match("src/dir/foo.nim", re2"^src/(?:[^/]*(?:/|$))*[^/]*\.nim$")
  check(not match("foo", re2"$($*)*(\w*)"))
  check(not match("foo", re2"$($$*$*)*(\w*)"))
  check(not match("foo", re2"($|$)($*)*(\w*)"))
  check(not match("foo", re2"($|$$)($*)*(\w*)"))
  check(not match("foo", re2"($$|$)($*)*(\w*)"))
  check(not match("foo", re2"($|$)(\w*)"))
  check(not match("foo", re2"($|$$)(\w*)"))
  check(not match("foo", re2"($$|$)(\w*)"))
  check(not match("foo", re2"(^$|$)(\w*)"))
  check(not match("foo", re2"($|^$)(\w*)"))
  check match("", re2"$*\w*")
  check match("foo", re2"($*?)(\w*)")
  check match("foo", re2"($*)(\w*)")
  check match("foox", re2"($*)(\w*)x")
  check match("foo", re2"(a*?$*?)*?(\w*)")
  check match("foox", re2"(a*$*)*(\w*)x")
  check match("aaa", re2"($|^)(a*)")
  check match("aaa", re2"(^|$)(a*)")
  block:
    check findAllBounds("aaaxaaa", re2"^(a*)") == @[0 .. 2]
    check findAllBounds("aaaxaaa", re2"$(a*)") == @[7 .. 6]
    check findAllBounds("aaaxaaa", re2"($|^)(a*)") == @[0 .. 2, 7 .. 6]
    check findAllBounds("aaaxaaa", re2"(^|$)(a*)") == @[0 .. 2, 7 .. 6]

test "escapere2":
  check escapeRe("abc") == "abc"
  check escapeRe("123") == "123"
  check escapeRe("!") == "!"
  check escapeRe("*") == r"\*"
  check escapeRe" #$&()*+-.?[\]^{|}~" ==
    r"\ \#\$\&\(\)\*\+\-\.\?\[\\\]\^\{\|\}\~"
  check escapeRe("\L") == "\\\L"
  check escapeRe"aΪⒶ弢" == "aΪⒶ弢"
  check escapeRe"$Ϊ$Ⓐ$弢$" == r"\$Ϊ\$Ⓐ\$弢\$"
  check match("$", re(escapeRe"$"))
  block:
    var s = ""
    for c in 0 .. 255:
      s.add c.char
    discard re(escapeRe(s))

test "issue_98":
  check match("", re2"|")
  check match("a", re2"a|")
  check match("", re2"a|")
  check(not match("b", re2"a|"))
  check match("b", re2"|b")
  check match("", re2"|b")
  check(not match("a", re2"|b"))
  check match("", re2"(|)")
  check match("a", re2"(a|)")
  check match("", re2"(a|)")
  check(not match("b", re2"(a|)"))
  check match("b", re2"(|b)")
  check match("", re2"(|b)")
  check(not match("a", re2"(|b)"))
  check match("", re2"||")
  check match("a", re2"a||")
  check match("", re2"a||")
  check match("b", re2"||b")
  check match("", re2"||b")
  check match("", re2"a||b")
  check match("a", re2"a||b")
  check match("b", re2"a||b")
  check match("", re2"|||")
  check match("a", re2"a|||")
  check match("", re2"a|||")
  check match("b", re2"|||b")
  check match("", re2"|||b")
  check match("a", re2"a|||b")
  check match("b", re2"a|||b")
  check match("", re2"(||)")
  check match("a", re2"(a||)")
  check match("", re2"(a||)")
  check match("b", re2"(||b)")
  check match("", re2"(||b)")
  check match("1.1.1.1", re2"(\d+)\.(\d+)(\.(\d+)|)(\.(\d+)|)")
  check match("1.1.1", re2"(\d+)\.(\d+)(\.(\d+)|)(\.(\d+)|)")
  check match("1.1", re2"(\d+)\.(\d+)(\.(\d+)|)(\.(\d+)|)")

test "issue_101":
  var m: RegexMatch2
  check match("TXT1/TXT2.1", re2"(TXT1)/TXT2()\.(\d+)")
  check match("TXT1/TXT2.1", re2"(TXT1)/TXT2(?:)\.(\d+)")
  check match("TXT1/TXT2.1", re2"(TXT1)/TXT2(?i:)\.(\d+)")
  check match("TXT1/TXT2.1", re2"(TXT1)/TXT2(?P<foo>)\.(\d+)")
  check match("TXT1/TXT2.1", re2"(TXT1)/TXT2()\.(\d+)", m) and
    m.captures == @[0 .. 3, 9 .. 8, 10 .. 10]
  check match(" ", re2"(?x)     (?-x) ")
  check match("aa", re2"((?x)a    )a")
  check match("aa", re2"((?x)   a    )a")
  check match(" ", re2"((?x)) ")
  check match(" ", re2"((?x)     ) ")
  check match(" ", re2"(?x:(?x)     ) ")
  check match(" ", re2"(?x:) ")
  check match(" ", re2"(?x:   ) ")
  check match(" ", re2"((?x:)) ")
  check match("A", re2"(?xi)     a")
  check(not match("A", re2"((?xi))     a"))
  check(not match("A", re2"(?xi:(?xi)     )a"))
