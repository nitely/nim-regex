from sequtils import map

import regex

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

template check(conditions: bool) =
  doAssert(conditions)

template expect(exception: typedesc, body: untyped): untyped =
  doAssertRaises(exception):
    body

when defined(forceRegexAtRuntime):
  proc isMatch(s: string, pattern: Regex): bool =
    var m: RegexMatch
    result = match(s, pattern, m)
else:
  proc isMatch(s: string, pattern: static Regex): bool =
    var m: RegexMatch
    result = match(s, pattern, m)

proc raises(pattern: string): bool =
  result = false
  try:
    discard pattern.re()
  except RegexError:
    result = true

proc raisesMsg(pattern: string): string =
  try:
    discard pattern.re()
  except RegexError:
    result = getCurrentExceptionMsg()

proc toStrCaptures(m: RegexMatch, s: string): seq[seq[string]] =
  result = newSeq[seq[string]](m.groupsCount)
  var j = 0
  for i in 0 ..< m.groupsCount:
    result[i] = newSeq[string](m.group(i).len)
    j = 0
    for cbounds in m.group(i):
      result[i][j] = s[cbounds]
      inc j

proc matchWithCapt(s: string, pattern: static Regex): seq[seq[string]] =
  var m: RegexMatch
  doAssert match(s, pattern, m)
  result = m.toStrCaptures(s)

proc findWithCapt(s: string, pattern: Regex): seq[seq[string]] =
  var m: RegexMatch
  doAssert find(s, pattern, m)
  result = m.toStrCaptures(s)

func findAllBounds(s: string, reg: Regex): seq[Slice[int]] =
  result = map(
    findAll(s, reg),
    func (m: RegexMatch): Slice[int] =
      m.boundaries)

test "tfull_match":
  check "".isMatch(re"")
  check "a".isMatch(re"a")
  check "ab".isMatch(re"(a)b")
  check "aa".isMatch(re"(a)*")
  check "aab".isMatch(re"((a)*b)")
  check "abbbbccccd".isMatch(re"a(b|c)*d")
  check "abbb".isMatch(re"((a)*(b)*)")
  check "abbb".isMatch(re"((a(b)*)*(b)*)")
  check "a".isMatch(re"a|b")
  check "b".isMatch(re"a|b")
  check(not "ab".isMatch(re"a(b|c)*d"))
  check(not "a".isMatch(re"b"))
  check(not "a".isMatch(re""))
  # raw string need double "" instead of \" to escape,
  # this is a Nim thing
  check " \"word\" ".isMatch(re"\s"".*""\s")

test "trepetition_cycle":
  check "aaa".isMatch(re"a**")
  check "aaa".isMatch(re"(a*)*")
  check "aaabbbaaa".isMatch(re"((a*|b*))*")
  check "aaa".isMatch(re"a*****")
  check raises(r"a*{,}")
  check "aaa".isMatch(re"(a?)*")
  check "aaaa".isMatch(re"((a)*(a)*)*")

test "tcaptures":
  check "ab".matchWithCapt(re"(a)b") == @[@["a"]]
  check "aa".matchWithCapt(re"(a)*") == @[@["a", "a"]]
  check "aab".matchWithCapt(re"((a)*b)") ==
    @[@["aab"], @["a", "a"]]
  check "abbbbccccd".matchWithCapt(re"a(b|c)*d") ==
    @[@["b", "b", "b", "b", "c", "c", "c", "c"]]
  check "abbb".matchWithCapt(re"((a)*(b)*)") ==
    @[@["abbb"], @["a"], @["b", "b", "b"]]
  check "abbb".matchWithCapt(re"((a(b)*)*(b)*)") ==
    @[@["abbb"], @["abbb"], @["b", "b", "b"], @[]]
  check "aa".matchWithCapt(re"(a)+") == @[@["a", "a"]]
  check "abab".matchWithCapt(re"(ab)+") == @[@["ab", "ab"]]
  check "a".matchWithCapt(re"(a)?") == @[@["a"]]
  check "ab".matchWithCapt(re"(ab)?") == @[@["ab"]]
  check "aaabbbaaa".matchWithCapt(re"(a*|b*)*") ==
    @[@["aaa", "bbb", "aaa"]]
  check "abab".matchWithCapt(re"(a(b))*") ==
    @[@["ab", "ab"], @["b", "b"]]
  # Following two should match the same
  check "aaanasdnasd".matchWithCapt(re"((a)*n?(asd)*)*") ==
    @[@["aaanasd", "nasd"], @["a", "a", "a"], @["asd", "asd"]]
  check "aaanasdnasd".matchWithCapt(re"((a)*n?(asd))*") ==
    @[@["aaanasd", "nasd"], @["a", "a", "a"], @["asd", "asd"]]
  check "b".matchWithCapt(re"(a)?b") ==
    @[newSeq[string]()]
  check "ฅa".matchWithCapt(re"(\w)(a)") ==
    @[@["ฅ"], @["a"]]

test "tzero_or_more_op":
  check raisesMsg(r"*") ==
    "Invalid `*` operator, nothing to repeat"
  check raises(r"*abc")
  check(not raises(r"\b*"))

test "tone_or_more_op":
  check "aaaa".isMatch(re"a+")
  check "abb".isMatch(re"ab+")
  check "abaa".isMatch(re"aba+")
  check(not "".isMatch(re"a+"))
  check(not "b".isMatch(re"a+"))
  check(not "aab".isMatch(re"b+"))
  check raisesMsg(r"(+)") ==
    "Invalid `+` operator, nothing to repeat"
  check raises(r"+")
  check raises(r"+abc")
  check(not raises(r"\b+"))

test "tzero_or_one_op":
  check "a".isMatch(re"a?")
  check "".isMatch(re"a?")
  check "a".isMatch(re"ab?")
  check "ab".isMatch(re"ab?")
  check "aba".isMatch(re"ab?a")
  check "aa".isMatch(re"ab?a")
  check(not "aa".isMatch(re"a?"))
  check(not "b".isMatch(re"a?"))
  check(not "abb".isMatch(re"ab?"))
  check raisesMsg(r"?") ==
    "Invalid `?` operator, nothing to make optional"
  check raises(r"?abc")
  check(not raises(r"\b?"))

test "tescape":
  check "(a)".isMatch(re"\(a\)")
  check "a*b".isMatch(re"a\*b")
  check "a*bbb".isMatch(re"a\*b*")
  check "y".isMatch(re"\y")
  check "\\".isMatch(re"\\")
  check "\\\\".isMatch(re"\\\\")

test "talphanum_shorthand":
  check "a".isMatch(re"\w")
  check "abc123".isMatch(re"\w*")
  check "a".matchWithCapt(re"(\w)") == @[@["a"]]

test "tdigit":
  check "1".isMatch(re"\d")
  check "123".isMatch(re"\d*")
  check "۲".isMatch(re"\d")  # Kharosthi numeral
  check(not "⅕".isMatch(re"\d"))

test "twhite_space_shorthand":
  check " ".isMatch(re"\s")
  check "   ".isMatch(re"\s*")
  check " \t\r\f\v".isMatch(re"\s*")
  check "\u20".isMatch(re"\s")  # New Line
  check "\u2028".isMatch(re"\s")  # Line separator

test "talphanum_not_shorthand":
  check(not "a".isMatch(re"\W"))
  check(not "abc123".isMatch(re"\W*"))
  check "!@#".isMatch(re"\W+")

test "tnot_digit":
  check(not "1".isMatch(re"\D"))
  check(not "123".isMatch(re"\D*"))
  check(not "۲".isMatch(re"\D"))  # Kharosthi numeral
  check "⅕".isMatch(re"\D")
  check "!@#".isMatch(re"\D+")
  check "a".isMatch(re"\D")

test "tnot_white_space_shorthand":
  check "asd123!@#".isMatch(re"\S*")
  check(not " ".isMatch(re"\S"))
  check(not "   ".isMatch(re"\S*"))
  check(not "\t".isMatch(re"\S"))
  check(not "\u20".isMatch(re"\S"))
  check(not "\r".isMatch(re"\S"))
  check(not "\f".isMatch(re"\S"))
  check(not "\v".isMatch(re"\S"))
  check(not "\u2028".isMatch(re"\S"))  # Line separator

test "tset":
  check "a".isMatch(re"[a]")
  check "a".isMatch(re"[abc]")
  check "b".isMatch(re"[abc]")
  check "c".isMatch(re"[abc]")
  check(not "d".isMatch(re"[abc]"))
  check "a".isMatch(re"[\w]")
  check "1".isMatch(re"[\w]")
  check "1".isMatch(re"[\d]")
  check "*".isMatch(re"[*]")
  check "*".isMatch(re"[\*]")
  check "*".isMatch(re"[a*]")
  check "a".isMatch(re"[a*]")
  check "a".isMatch(re"[a-z]")
  check "f".isMatch(re"[a-z]")
  check "z".isMatch(re"[a-z]")
  check(not "A".isMatch(re"[a-z]"))
  check "0".isMatch(re"[0-9]")
  check "5".isMatch(re"[0-9]")
  check "9".isMatch(re"[0-9]")
  check(not "a".isMatch(re"[0-9]"))
  check "(".isMatch(re"[()[\]{}]")
  check ")".isMatch(re"[()[\]{}]")
  check "}".isMatch(re"[()[\]{}]")
  check "{".isMatch(re"[()[\]{}]")
  check "[".isMatch(re"[()[\]{}]")
  check "]".isMatch(re"[()[\]{}]")
  check "(".isMatch(re"[]()[{}]")
  check ")".isMatch(re"[]()[{}]")
  check "}".isMatch(re"[]()[{}]")
  check "{".isMatch(re"[]()[{}]")
  check "[".isMatch(re"[]()[{}]")
  check "]".isMatch(re"[]()[{}]")
  check "\\".isMatch(re"[\\]")
  check "\\".isMatch(re"[\\\]]")
  check "]".isMatch(re"[\\\]]")
  check "00".isMatch(re"[0-5][0-9]")
  check "59".isMatch(re"[0-5][0-9]")
  check(not "95".isMatch(re"[0-5][0-9]"))
  check "1".isMatch(re"[0-57-9]")
  check "8".isMatch(re"[0-57-9]")
  check(not "6".isMatch(re"[0-57-9]"))
  check "4".isMatch(re"[0-9A-Fa-f]")
  check "b".isMatch(re"[0-9A-Fa-f]")
  check "B".isMatch(re"[0-9A-Fa-f]")
  check(not "-".isMatch(re"[0-9A-Fa-f]"))
  check "-".isMatch(re"[a\-z]")
  check "a".isMatch(re"[a\-z]")
  check "z".isMatch(re"[a\-z]")
  check(not "b".isMatch(re"[a\-z]"))
  check "a".isMatch(re"[a-]")
  check "-".isMatch(re"[a-]")
  check "+".isMatch(re"[(+*)]")
  check "*".isMatch(re"[(+*)]")
  check "(".isMatch(re"[(+*)]")
  check "[".isMatch(re"[[-\]]")
  check "]".isMatch(re"[[-\]]")
  check(not "-".isMatch(re"[[-\]]"))
  check "(".isMatch(re"[(-\)]")
  check ")".isMatch(re"[(-\)]")
  check(not "-".isMatch(re"[(-\)]"))
  check "\\".isMatch(re"[\\-\\)]")
  check(not "-".isMatch(re"[\\-\\)]"))
  check "-".isMatch(re"[-]")
  check "-".isMatch(re"[\-]")
  check "-".isMatch(re"[\-\-]")
  check "-".isMatch(re"[\--]")
  check "-".isMatch(re"[\--\-]")
  check "-".isMatch(re"[\---]")
  check "b".isMatch(re"[\--\-a-z]")
  check "b".isMatch(re"[\---a-z]")
  check "b".isMatch(re"[-a-z]")
  check "-".isMatch(re"[-a-z]")
  check "a".isMatch(re"[-a]")
  check "-".isMatch(re"[-a]")
  check "b".isMatch(re"[a-d-z]")
  check "-".isMatch(re"[a-d-z]")
  check "z".isMatch(re"[a-d-z]")
  check(not "e".isMatch(re"[a-d-z]"))
  check "]".isMatch(re"[]]")
  check "]".isMatch(re"[\]]")
  check(not "[".isMatch(re"[]]"))
  check(not "]]".isMatch(re"[]]"))
  check(not "-".isMatch(re"[[-\]]"))
  check(not "b".isMatch(re"[c-d]"))
  check "-".isMatch(re"[a\w-\wz]")
  check "-".isMatch(re"[\w-a]")
  check "-".isMatch(re"[\w-]")
  check "a".isMatch(re"[\w-a]")
  check "1".isMatch(re"[\w-a]")
  check "-".isMatch(re"[db-c-f]")
  check(not "e".isMatch(re"[db-c-f]"))
  check(not "-".isMatch(re"[=-_]"))
  check "A".isMatch(re"[\A]")
  check "b".isMatch(re"[\b]")
  check "zz".isMatch(re"[\z][\z]")
  check(not "z".isMatch(re"[\z][\z]"))
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
  check "a".isMatch(re"[\u0061]")
  check(not "b".isMatch(re"[\u0061]"))
  check "a".isMatch(re"[\U00000061]")
  check "a".isMatch(re"[\x61]")
  check "a".isMatch(re"[\x{61}]")
  check "abab".isMatch(re"[\x61-\x62]*")
  check "a".isMatch(re"[\141]")

test "tnot_set":
  check "a".matchWithCapt(re"([^b])") == @[@["a"]]
  check "asd".matchWithCapt(re"([^b]*)") == @[@["asd"]]
  check "ab".matchWithCapt(re"([^b]*b)") == @[@["ab"]]
  check "asd123".matchWithCapt(re"([^\d]*)(\d*)") ==
    @[@["asd"], @["123"]]
  check "asd123".matchWithCapt(re"([asd]*)([^asd]*)") ==
    @[@["asd"], @["123"]]
  check "<asd123!@#>".matchWithCapt(re"(<[^>]*>)") ==
    @[@["<asd123!@#>"]]
  check(not "a".isMatch(re"[^a]"))
  check raisesMsg(r"[^]") ==
    "Invalid set. Missing `]`\n" &
    "[^]\n" &
    "^"
  check "^".isMatch(re"[\^]")
  check "a".isMatch(re"[\^a]")
  check(not "^".isMatch(re"[^^]"))
  check "a".isMatch(re"[^^]")
  check "a".isMatch(re"[^-]")
  check(not "-".isMatch(re"[^-]"))

test "trepetition_range":
  check(not "".isMatch(re"a{0}"))
  check(not "".isMatch(re"a{0,0}"))
  check(not "".isMatch(re"a{,0}"))
  check "".isMatch(re"a{,2}")
  check "a".isMatch(re"a{0}")
  check "a".isMatch(re"a{0,0}")
  check "a".isMatch(re"a{,0}")
  check "a".isMatch(re"a{1}")
  check "aa".isMatch(re"a{2}")
  check "aaa".isMatch(re"a{3}")
  check(not "aaaa".isMatch(re"a{3}"))
  check(not "".isMatch(re"a{1}"))
  check "a".isMatch(re"a{1,1}")
  check "a".isMatch(re"a{1,2}")
  check "aa".isMatch(re"a{1,2}")
  check(not "aaa".isMatch(re"a{1,2}"))
  check(not "a".isMatch(re"a{2,4}"))
  check "a".isMatch(re"a{1,}")
  check "aa".isMatch(re"a{1,}")
  check "aaa".isMatch(re"a{1,}")
  check "aaaaaaaaaa".isMatch(re"a{1,}")
  check "aa".isMatch(re"a{2,}")
  check "a".isMatch(re"a{,}")
  check "aa".isMatch(re"a{,}")
  check "aaaaaaaaaa".isMatch(re"a{,}")
  check "".isMatch(re"a{,}")
  check "aaaaaaaaaa".isMatch(re"a{0,}")
  check "".isMatch(re"a{0,}")
  check(not "a".isMatch(re"a{2,}"))
  check raises(r"a*{,}")
  check raises(r"a*{0}")
  check raises(r"a*{1}")
  check "aaa".matchWithCapt(re"(a){,}") ==
    @[@["a", "a", "a"]]
  check "aaa".matchWithCapt(re"(a{,}){,}") == @[@["aaa"]]
  check "aaaaa".matchWithCapt(re"(a){5}") ==
    @[@["a", "a", "a", "a", "a"]]
  check "a".matchWithCapt(re"(a){1,5}") == @[@["a"]]
  check "aaa".matchWithCapt(re"(a){1,5}") ==
    @[@["a", "a", "a"]]
  check "".matchWithCapt(re"(a){,}") ==
    @[newSeq[string]()]
  check "aaa".matchWithCapt(re"(a{,}){,}") == @[@["aaa"]]
  check "aaa".matchWithCapt(re"(a{1}){,}") ==
    @[@["a", "a", "a"]]
  check "aaaa".matchWithCapt(re"(a{2}){,}") ==
    @[@["aa", "aa"]]
  check "aaaa".matchWithCapt(re"(a{,3}){,}") ==
    @[@["aaa", "a"]]
  check "".matchWithCapt(re"(a{,3}){,}") ==
    @[newSeq[string]()]
  check "aaa".matchWithCapt(re"(a{1,}){,}") ==
    @[@["aaa"]]
  check "".matchWithCapt(re"(a{1,}){,}") ==
    @[newSeq[string]()]
  check(not "".isMatch(re"(a{1,})"))
  check "a".matchWithCapt(re"(a{1,})") == @[@["a"]]
  check "aaa".matchWithCapt(re"(a{1,})") == @[@["aaa"]]
  check "abab".matchWithCapt(re"(a(b)){2}") ==
    @[@["ab", "ab"], @["b", "b"]]
  check raisesMsg(r"a{bad}") ==
    "Invalid repetition range. Range can only contain digits\n" &
    "a{bad}\n" &
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
  check raises(r"a{a,1}")
  check raises(r"a{-1}")
  check raisesMsg(r"{10}") ==
    "Invalid repeition range, " &
    "nothing to repeat"
  check raisesMsg(r"abc\A{10}") ==
    "Invalid repetition range, either " &
    "char, shorthand (i.e: \\w), group, or set " &
    "expected before repetition range"

test "tnon_capturing_groups":
  check "abab".matchWithCapt(re"(a(b))*") ==
    @[@["ab", "ab"], @["b", "b"]]
  check "abab".matchWithCapt(re"(?:a(b))*") ==
    @[@["b", "b"]]
  check "abab".matchWithCapt(re"(a(?:b))*") ==
    @[@["ab", "ab"]]
  check ")".matchWithCapt(re"(\))") == @[@[")"]]

test "tgreediness":
  check "a".matchWithCapt(re"(a)??") ==
    @[@["a"]]
  check "aaa".matchWithCapt(re"(a)*(a)*(a)*") ==
    @[@["a", "a", "a"], @[], @[]]
  check "aaa".matchWithCapt(re"(a)*?(a)*(a)*?") ==
    @[@[], @["a", "a", "a"], @[]]
  check "aaa".matchWithCapt(re"(a)*?(a)*?(a)*") ==
    @[@[], @[], @["a", "a", "a"]]
  check "aaa".matchWithCapt(re"(a)*?(a)*?(a)*?") ==
    @[@[], @[], @["a", "a", "a"]]
  check "aaaa".matchWithCapt(re"(a)*?(a)*?(a)*?") ==
    @[@[], @[], @["a", "a", "a", "a"]]
  check "aa".matchWithCapt(re"(a)?(aa?)") ==
    @[@["a"], @["a"]]
  check "aa".matchWithCapt(re"(a)??(a)") ==
    @[@["a"], @["a"]]
  check "aa".matchWithCapt(re"(a)??(aa?)") ==
    @[@[], @["aa"]]
  check "aaa".matchWithCapt(re"(a)+(a)+(a)?") ==
    @[@["a", "a"], @["a"], @[]]
  check "aaa".matchWithCapt(re"(a)+?(a)+(a)?") ==
    @[@["a"], @["a", "a"], @[]]
  check "aaa".matchWithCapt(re"(a)+?(a)+?(a)?") ==
    @[@["a"], @["a"], @["a"]]
  check "aaa".matchWithCapt(re"(a){,}(a){,}(a){,}") ==
    @[@["a", "a", "a"], @[], @[]]
  check "aaa".matchWithCapt(re"(a){,}?(a){,}(a){,}?") ==
    @[@[], @["a", "a", "a"], @[]]
  check "aaa".matchWithCapt(re"(a){,}?(a){,}?(a){,}") ==
    @[@[], @[], @["a", "a", "a"]]
  check "aaa".matchWithCapt(re"(a){,}?(a){,}?(a){,}?") ==
    @[@[], @[], @["a", "a", "a"]]
  check "aaa".matchWithCapt(re"(a){1,}(a){1,}(a)?") ==
    @[@["a", "a"], @["a"], @[]]
  check "aaa".matchWithCapt(re"(a){1,}?(a){1,}(a)?") ==
    @[@["a"], @["a", "a"], @[]]
  check "aaa".matchWithCapt(re"(a){1,}?(a){1,}?(a)?") ==
    @[@["a"], @["a"], @["a"]]
  block:
    var m: RegexMatch
    check match("aaaa", re"(a*?)(a*?)(a*)", m)
    check m.toStrCaptures("aaaa") ==
      @[@[""], @[""], @["aaaa"]]
  block:
    var m: RegexMatch
    check match("aaaa", re"(a*)(a*?)(a*?)", m)
    check m.toStrCaptures("aaaa") ==
       @[@["aaaa"], @[""], @[""]]

test "tassertions":
  check "bbaa aa".matchWithCapt(re"([\w ]*?)(\baa\b)") ==
    @[@["bbaa "], @["aa"]]
  check "aa bbaa".matchWithCapt(re"(\baa\b)([\w ]*)") ==
    @[@["aa"], @[" bbaa"]]
  check "This island is great".matchWithCapt(
      re"([\w ]*?)(\bis\b)([\w ]*?)") ==
    @[@["This island "], @["is"], @[" great"]]
  check "bbaabb".matchWithCapt(re"([\w ]*?)(\Baa\B)([\w ]*?)") ==
    @[@["bb"], @["aa"], @["bb"]]
  check "This is my sister".matchWithCapt(
      re"([\w ]*?)(\Bis\B)([\w ]*?)") ==
    @[@["This is my s"], @["is"], @["ter"]]
  check "aa".isMatch(re"\b\b\baa\b\b\b")
  check "bb".isMatch(re"^^^^bb$$$$")
  check "bb".isMatch(re"\A\A\A\Abb\z\z\z\z")

test "tdot_any_matcher":
  check "a".isMatch(re".")
  check "asd123!@#".isMatch(re".*")
  check "| (•□•) | (❍ᴥ❍ʋ)".isMatch(re".*")
  check "ฅ^•ﻌ•^ฅ".matchWithCapt(re"(.*)") ==
    @[@["ฅ^•ﻌ•^ฅ"]]
  check "\t".isMatch(re".")
  check(not "\L".isMatch(re".*"))

test "tgroup":
  block:
    var m: RegexMatch
    check "foobar".match(re"(\w*)", m)
    check m.group(0) == @[0..5]
  block:
    var m: RegexMatch
    check "foobar".match(re"(?P<foo>\w*)", m)
    check m.group(0) == @[0..5]
  block:
    var m: RegexMatch
    check "ab".match(re"(a)(b)", m)
    check m.group(0) == @[0..0]
    check m.group(1) == @[1..1]
  block:
    var m: RegexMatch
    check match("ab", re"(a)(b)", m)
    check m.toStrCaptures("ab") ==
      @[@["a"], @["b"]]
  block:
    let
      expected = ["a", "b", "c"]
      text = "abc"
    var m: RegexMatch
    check text.match(re"(?P<foo>\w)+", m)
    var i = 0
    for bounds in m.group("foo"):
      check expected[i] == text[bounds]
      inc i
  block:
    let
      expected = ["a", "b", "c"]
      text = "abc"
    var m: RegexMatch
    check text.match(re"(\w)+", m)
    var i = 0
    for bounds in m.group(0):
      check expected[i] == text[bounds]
      inc i

test "tnamed_groups":
  block:
    var m: RegexMatch
    check "foobar".match(re"(?P<foo>\w*)", m)
    check m.group("foo") == @[0..5]
  block:
    var m: RegexMatch
    check "foobar".match(re"(?P<foo>(?P<bar>\w*))", m)
    check m.group("foo") == @[0..5]
    check m.group("bar") == @[0..5]
  block:
    var m: RegexMatch
    check "aab".match(re"(?P<foo>(?P<bar>a)*b)", m)
    check m.group("foo") == @[0..2]
    check m.group("bar") == @[0..0, 1..1]
  block:
    var m: RegexMatch
    check "aab".match(re"((?P<bar>a)*b)", m)
    check m.group("bar") == @[0..0, 1..1]

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
  check raisesMsg(r"a()") ==
    "Invalid group. Empty group is not allowed\n" &
    "a()\n" &
    " ^"
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
  check "foo\Lbar".isMatch(re"(?s).*")
  check "foo\Lbar".isMatch(re"(?s:.*)")
  check "foo\Lbar".isMatch(re"(?ssss).*")
  check(not "foo\Lbar".isMatch(re"(?s-s).*"))
  check(not "foo\Lbar".isMatch(re"(?-s-s-s).*"))
  check(not "foo\Lbar".isMatch(re"(?-ss).*"))
  check(not "foo\Lbar".isMatch(re"(?-ss-ss).*"))
  check(not "foo\Lbar".isMatch(re"(?-sssss-s).*"))
  check(not "foo\Lbar".isMatch(re"(?s-s:.*)"))
  check(not "foo\Lbar".isMatch(re"(?------s----s:.*)"))
  check "foo\Lbar".matchWithCapt(re"((?s:.*))") ==
    @[@["foo\Lbar"]]
  check "a".matchWithCapt(re"((?i:a))") == @[@["a"]]
  check "A".matchWithCapt(re"((?i:a))") == @[@["A"]]
  check "ABC".matchWithCapt(re"((?i:aBc))") ==
    @[@["ABC"]]
  check "a".matchWithCapt(re"((?-i:a))") == @[@["a"]]
  check(not "A".isMatch(re"((?-i:a))"))
  check(not "A".isMatch(re"((?-ii-i:a))"))
  check "a".matchWithCapt(re"((?i)a)") == @[@["a"]]
  check "A".matchWithCapt(re"((?i)a)") == @[@["A"]]
  check "a".matchWithCapt(re"((?-i)a)") == @[@["a"]]
  check(not "A".isMatch(re"((?-i)a)"))
  check "AaA".isMatch(re"(?i)a+")
  check "AaA".isMatch(re"(?i)A+")
  check "AbC".isMatch(re"(?i)abc")
  check(not "b".isMatch(re"(?i)a"))
  check "A".isMatch(re"(?-i)(?i)a")
  check(not "A".isMatch(re"(?i)(?-i)a"))
  check "AaA".matchWithCapt(re"((?i)a+)") == @[@["AaA"]]
  check "A".isMatch(re"(?i)[a]")
  check "a".isMatch(re"(?i)[a]")
  check(not "@".isMatch(re"(?i)[a]"))
  check "a".isMatch(re"(?i)[A]")
  check "A".isMatch(re"(?i)[A]")
  check "C".isMatch(re"(?i)[a-z]")
  check "c".isMatch(re"(?i)[a-z]")
  check(not "@".isMatch(re"(?i)[a-z]"))
  check "c".isMatch(re"(?i)[A-Z]")
  check "C".isMatch(re"(?i)[A-Z]")

  check "aa".matchWithCapt(re"((?U)a*)(a*)") ==
    @[@[""], @["aa"]]
  check "aa".matchWithCapt(re"((?U)a*?)(a*)") ==
    @[@["aa"], @[""]]
  check "aa".matchWithCapt(re"((?U-U)a*)(a*)") ==
    @[@["aa"], @[""]]
  # no empty matches
  check "aa".matchWithCapt(re"(?U:(a)*)(a)*") ==
    @[@[], @["a", "a"]]
  check "aa".matchWithCapt(re"((?U:a*))(a*)") ==
    @[@[""], @["aa"]]
  check "aa".matchWithCapt(re"((?U:a*?))(a*)") ==
    @[@["aa"], @[""]]
  check "aa".matchWithCapt(re"((?U-U:a*))(a*)") ==
    @[@["aa"], @[""]]

  check(not "a\Lb\L".isMatch(re"(?sm)a.b(?-sm:.)"))
  check "a\Lb\L".isMatch(re"(?ms)a.b(?s-m:.)")
  check "a\L".isMatch(re"(?s)a.")
  check(not "a\L\L".isMatch(re"(?s)a.$."))
  check "a\L\L".isMatch(re"(?sm)a.$.")
  check(not "a\L\L".isMatch(re"(?-sm)a.$."))
  check(not "a\L\L".isMatch(re"(?s-m)a.$."))
  check "a\L\L".isMatch(re"(?s-m)(?m:a.$.)")
  check(not "a\L\L".isMatch(re"(?i-sm)(?s:a.$.)"))
  check "a\L\L".isMatch(re"(?i-sm)(?sm:a.$.)")
  check(not "a\L\L".isMatch(re"(?-sm)(?sm)(?-sm:a.$.)"))
  check "a\L\L".isMatch(re"(?sm)(?-sm)(?sm:a.$.)")
  check(not "a\L\L".isMatch(re"(?-sm)(?sm:(?-sm:a.$.))"))
  check "a\L\L".isMatch(re"(?sm)(?-sm:(?sm:a.$.))")

  check "Ǝ".isMatch(re"\w")
  check "Ǝ".isMatch(re"(?u)\w")
  check(not "Ǝ".isMatch(re"(?-u)\w"))
  check "abczABCZ0129_".isMatch(re"(?-u)\w*")
  check(not "\t".isMatch(re"(?-u)\w"))
  # todo: test every ascii kind
  check "Ǝ".isMatch(re"(?u)[\w]")
  check(not "Ǝ".isMatch(re"(?u)[^\w]"))
  check "Ǝ".isMatch(re"(?-u)[^\w]")
  check(not "Ǝ".isMatch(re"(?-u)[\w]"))
  check(not "\t".isMatch(re"(?-u)[\w]"))
  check "ƎƎ".isMatch(re"(?-u)[^\w](?u)\w")

  check "a".isMatch(re"(?x)a")
  check "a".isMatch(re"(?x)a ")
  check "a".isMatch(re"(?x)a   ")
  check "a".isMatch(re"(?x) a ")
  check "a".isMatch(re("(?x)a\L   \L   \L"))
  check "a".isMatch(re("(?x)\L a \L"))
  check "a".isMatch(re"(?x: a )")
  check "a".isMatch(re"""(?x)a""")
  check "a".isMatch(re"""(?x)
    a
    """)
  check "a".isMatch(re"""(?x:
    a
    )""")
  check "a".isMatch(re"""(?x)(
    a
    )""")
  check "a".isMatch(re"""(?x)
    a  # should ignore this comment
    """)
  check "a".isMatch(re"""(?x:
    a  # should ignore this comment
    )""")
  check "aa ".isMatch(re"(?x)a  (?-x)a ")
  check "a a".isMatch(re"a (?x)a  ")
  check "aa".isMatch(re"((?x)a    )a")
  check "aa".isMatch(re"(?x:a    )a")
  check "a ".isMatch(re"(?x)a\ ")
  check "a ".isMatch(re"(?x)a\   ")
  check "a#".isMatch(re"(?x)a\#")
  check "a ".isMatch(re"(?x)a[ ]")
  check "a\n".isMatch(re"(?x)a\n")
  check "aa ".isMatch(re"""(?x)
    a    #    comment
    (?-x)a """)
  check "aaa".isMatch(re"""(?x)  # comment
    a  # comment
    a  # comment
    a  # comment
    # comment""")
  check "12.0".isMatch(re"""(?x)
    \d +  # the integral part
    \.    # the decimal point
    \d *  # some fractional digits""")
  check re"""(?x)    # verbose mode
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

test "tor_op":
  check raisesMsg(r"|") ==
    "Invalid OR conditional, nothing " &
    "to match at right/left side of the condition"
  check raises(r"abc|")
  check raises(r"|abc")

test "tescaped_sequences":
  check "\x07".isMatch(re"\a")
  check "\x0C".isMatch(re"\f")
  check "\t".isMatch(re"\t")
  check "\L".isMatch(re"\n")
  check "\r".isMatch(re"\r")
  check "\x0B".isMatch(re"\v")
  check(not "a".isMatch(re"\a"))
  check ".+*?()|[]{}^$".isMatch(re"\.\+\*\?\(\)\|\[\]\{\}\^\$")

  check "\x07".isMatch(re"[\a]")
  check "\x07".isMatch(re"[\a-\a]")
  check(not "0".isMatch(re"[\a-\a]"))
  #check "|".isMatch(re"[a|b]")  # ????

test "tfind":
  block:
    var m: RegexMatch
    check "abcd".find(re"bc", m)
  block:
    var m: RegexMatch
    check(not "abcd".find(re"ac", m))
  block:
    var m: RegexMatch
    check "a".find(re"", m)
  block:
    var m: RegexMatch
    check "abcd".find(re"^abcd$", m)
  check "2222".findWithCapt(re"(22)*") ==
    @[@["22", "22"]]
  block:
    var m: RegexMatch
    check "2222".find(re"(22)*", m)
    check m.group(0) == @[0 .. 1, 2 .. 3]
  block:
    var m: RegexMatch
    check "abcd".find(re"(ab)", m)
    check m.group(0) == @[0 .. 1]
  block:
    var m: RegexMatch
    check "abcd".find(re"(bc)", m)
    check m.group(0) == @[1 .. 2]
  block:
    var m: RegexMatch
    check "abcd".find(re"(cd)", m)
    check m.group(0) == @[2 .. 3]
  block:
    var m: RegexMatch
    check "abcd".find(re"bc", m)
    check m.boundaries == 1 .. 2
  block:
    var m: RegexMatch
    check "aΪⒶ弢".find(re"Ϊ", m)
    check m.boundaries == 1 .. 2
  block:
    var m: RegexMatch
    check "aΪⒶ弢".find(re"Ⓐ", m)
    check m.boundaries == 3 .. 5
  block:
    var m: RegexMatch
    check "aΪⒶ弢".find(re"弢", m)
    check m.boundaries == 6 .. 9

test "tcontains":
  check re"bc" in "abcd"
  check re"bd" notin "abcd"
  check re"(23)+" in "2323"
  check re"(23)+" in "23232"
  check re"^(23)+$" notin "23232"

test "tsplit":
  check split("a,b,c", re",") == @["a", "b", "c"]
  check split("00232this02939is39an22example111", re"\d+") ==
    @["", "this", "is", "an", "example", ""]
  check split("AAA :   : BBB", re"\s*:\s*") == @["AAA", "", "BBB"]
  check split("", re",") == @[""]
  check split(",,", re",") == @["", "", ""]
  check split("abc", re"") == @["a", "b", "c"]
  check split(",a,Ϊ,Ⓐ,弢,", re",") ==
    @["", "a", "Ϊ", "Ⓐ", "弢", ""]
  check split("弢", re"\xAF") == @["弢"]  # "弢" == "\xF0\xAF\xA2\x94"
  block:
    var
      expected = ["", "a", "Ϊ", "Ⓐ", "弢", ""]
      i = 0
    for s in split("11a22Ϊ33Ⓐ44弢55", re"\d+"):
      check s == expected[i]
      inc i

  check split("Words, words, words.", re"\W+") ==
    @["Words", "words", "words", ""]
  check split("0a3B9", re"[a-fA-F]+") ==
    @["0", "3", "9"]
  check split("1 2 3 4 5 6 ", re" ") ==
    @["1", "2", "3", "4", "5", "6", ""]
  check split("1  2  ", re" ") == @["1", "", "2", "", ""]
  check split("1 2", re" ") == @["1", "2"]
  check split("foo", re"foo") == @["", ""]
  check split("", re"foo") == @[""]

  check "12".split(re"\w\b") == @["1", ""]
  check "12".split(re"\w\B") == @["", "2"]

# XXX empty maches need fixing not just here, but in general
test "tsplitIncl":
  check "a,b".splitIncl(re"(,)") == @["a", ",", "b"]
  check "12".splitIncl(re"(\d)") == @["", "1", "", "2", ""]
  check splitIncl("aΪⒶ弢", re"(\w)") ==
    @["", "a", "", "Ϊ", "", "Ⓐ", "", "弢", ""]
  check splitIncl("aΪⒶ弢", re"") == @["a", "Ϊ", "Ⓐ", "弢"]
  check splitIncl("...words, words...", re"(\W+)") ==
    @["", "...", "words", ", ", "words", "...", ""]
  check splitIncl("Words, words, words.", re"(\W+)") ==
    @["Words", ", ", "words", ", ", "words", ".", ""]

  # regular split stuff
  check splitIncl("a,b,c", re",") == @["a", "b", "c"]
  check splitIncl("00232this02939is39an22example111", re"\d+") ==
    @["", "this", "is", "an", "example", ""]
  check splitIncl("AAA :   : BBB", re"\s*:\s*") == @["AAA", "", "BBB"]
  check splitIncl("", re",") == @[""]
  check splitIncl(",,", re",") == @["", "", ""]
  check splitIncl("abc", re"") == @["a", "b", "c"]
  check splitIncl(",a,Ϊ,Ⓐ,弢,", re",") ==
    @["", "a", "Ϊ", "Ⓐ", "弢", ""]
  check splitIncl("弢", re"\xAF") == @["弢"]  # "弢" == "\xF0\xAF\xA2\x94"
  check splitIncl("Words, words, words.", re"\W+") ==
    @["Words", "words", "words", ""]
  check splitIncl("0a3B9", re"[a-fA-F]+") ==
    @["0", "3", "9"]
  check splitIncl("1 2 3 4 5 6 ", re" ") ==
    @["1", "2", "3", "4", "5", "6", ""]
  check splitIncl("1  2  ", re" ") == @["1", "", "2", "", ""]
  check splitIncl("1 2", re" ") == @["1", "2"]
  check splitIncl("foo", re"foo") == @["", ""]
  check splitIncl("", re"foo") == @[""]

test "tfindall":
  check findAllBounds("abcabc", re"bc") == @[1 .. 2, 4 .. 5]
  check findAllBounds("aa", re"a") == @[0 .. 0, 1 .. 1]
  check findAllBounds("a", re"a") == @[0 .. 0]
  check findAllBounds("a", re"b") == newSeq[Slice[int]]()
  check findAllBounds("", re"b") == newSeq[Slice[int]]()
  # This follows nre's empty match behaviour
  check findAllBounds("a", re"") == @[0 .. -1, 1 .. 0]
  check findAllBounds("ab", re"") == @[0 .. -1, 1 .. 0, 2 .. 1]
  check findAllBounds("a", re"\b") == @[0 .. -1, 1 .. 0]
  check findAllBounds("aΪⒶ弢", re"Ϊ") == @[1 .. 2]
  check findAllBounds("aΪⒶ弢", re"Ⓐ") == @[3 .. 5]
  check findAllBounds("aΪⒶ弢", re"弢") == @[6 .. 9]
  check findAllBounds("aΪⒶ弢aΪⒶ弢", re"Ⓐ") == @[3 .. 5, 13 .. 15]
  # This is nre and Python's re behaviour,
  # they match aaa and then empty end
  check findAllBounds("aaa", re"a*") == @[0 .. 2, 3 .. 2]
  check findAllBounds("aaab", re"a*") == @[0 .. 2, 3 .. 2, 4 .. 3]
  check findAllBounds("aaa", re"a+") == @[0 .. 2]
  check findAllBounds("foo", re"") ==
    @[0 .. -1, 1 .. 0, 2 .. 1, 3 .. 2]

test "tfindandcaptureall":
  check findAndCaptureAll("abcabc", re"bc") == @["bc", "bc"]
  check findAndCaptureAll("a1b2c3a4b5c6", re"\d") == @["1", "2", "3", "4", "5", "6"]

test "tstarts_with":
  check "abc".startsWith(re"ab")
  check(not "abc".startsWith(re"bc"))
  check startsWith("弢ⒶΪ", re"弢Ⓐ")
  check startsWith("弢", re("\xF0\xAF\xA2\x94"))
  check(not startsWith("弢", re("\xF0\xAF\xA2")))
  check "abc".startsWith(re"\w")
  check(not "abc".startsWith(re"\d"))
  check "abc".startsWith(re"(a|b)")
  check "bc".startsWith(re"(a|b)")
  check(not "c".startsWith(re"(a|b)"))

test "tends_with":
  check "abc".endsWith(re"bc")
  check(not "abc".endsWith(re"ab"))
  check endsWith("弢ⒶΪ", re"ⒶΪ")
  check endsWith("弢", re("\xF0\xAF\xA2\x94"))
  check(not endsWith("弢", re("\xAF\xA2\x94")))
  check "abc".endsWith(re"(b|c)")
  check "ab".endsWith(re"(b|c)")
  check(not "a".endsWith(re"(b|c)"))

test "tliterals":
  check "a".isMatch(re"\u0061")
  check(not "b".isMatch(re"\u0061"))
  check "b".isMatch(re"\u0062")
  check "Ⓐ".isMatch(re"\u24b6")
  check "Ⓐ".isMatch(re"\u24B6")
  check raisesMsg(r"\u123") ==
    "Invalid unicode literal. Expected 4 hex digits, but found 3\n" &
    "\\u123\n" &
    "^"
  check raisesMsg(r"\u123@abc") ==
    "Invalid unicode literal. Expected hex digit, but found @\n" &
    "\\u123@abc\n" &
    "^"
  check "a".isMatch(re"\U00000061")
  check(not "b".isMatch(re"\U00000061"))
  check "b".isMatch(re"\U00000062")
  check "弢".isMatch(re"\U0002f894")
  check "弢".isMatch(re"\U0002F894")
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
  check "a".isMatch(re"\x{61}")
  check "a".isMatch(re"\x{061}")
  check(not "b".isMatch(re"\x{61}"))
  check "Ⓐ".isMatch(re"\x{24b6}")
  check "Ⓐ".isMatch(re"\x{000024b6}")
  check "弢".isMatch(re"\x{2f894}")
  check "弢".isMatch(re"\x{0002f894}")
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
  check "a".isMatch(re"\x61")
  check "aa".isMatch(re"\x61a")
  check "a".isMatch(re"\x61")
  check "a".isMatch(re"\141")
  check(not "b".isMatch(re"\141"))
  check "aa".isMatch(re"\141a")
  check "\u1ff".isMatch(re"\777")
  check "888".isMatch(re"\888")
  check raisesMsg(r"\12") ==
    "Invalid octal literal. Expected 3 octal digits, but found 2\n" &
    "\\12\n" &
    "^"
  check raisesMsg(r"\12@") ==
    "Invalid octal literal. Expected octal digit, but found @\n" &
    "\\12@\n" &
    "^"

test "tchar_class":
  check "a".isMatch(re"\pL")
  check(not "a".isMatch(re"\PL"))
  check(not "1".isMatch(re"\pL"))
  check "1".isMatch(re"\PL")
  check "aa".isMatch(re"\pLa")
  check "1".isMatch(re"\pN")
  check "_".isMatch(re"\pP")
  check "+".isMatch(re"\pS")
  check " ".isMatch(re"\pZ")
  check raisesMsg(r"\pB") ==
    "Invalid unicode name. Found B\n" &
    "\\pB\n" &
    "^"
  check raisesMsg(r"\p11") ==
    "Invalid unicode name. Found 1\n" &
    "\\p11\n" &
    "^"
  check "a".isMatch(re"\p{L}")
  check "ǅ".isMatch(re"\p{Lt}")
  check(not "ǅ".isMatch(re"\P{Lt}"))
  check(not "a".isMatch(re"\p{Lt}"))
  check "a".isMatch(re"\P{Lt}")
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
  check "d".isMatch(re"[[:alnum:]]")
  check "5".isMatch(re"[[:alnum:]]")
  check(not "{".isMatch(re"[[:alnum:]]"))
  check "{".isMatch(re"[[:alnum:]{]")
  check "-".isMatch(re"[[:alnum:]-z]")
  check raisesMsg(r"[z-[:alnum:]]") ==
    "Invalid set range. " &
    "Start must be lesser than end\n" &
    "[z-[:alnum:]]\n" &
    "   ^"
  check "a".isMatch(re"[[[[:alnum:]]")
  check "[".isMatch(re"[[[:alnum:]]")
  check(not ":".isMatch(re"[[:alnum:]]"))
  check ":".isMatch(re"[:alnum:]")
  check(not "a".isMatch(re"[[:^alnum:]]"))
  check "{".isMatch(re"[[:^alnum:]]")
  check(not "5".isMatch(re"[[:alpha:]]"))
  check(not "a".isMatch(re"[[:digit:]]"))
  check "5".isMatch(re"[[:alpha:][:digit:]]")
  check "a".isMatch(re"[[:alpha:][:digit:]]")
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
  check "a".replace(re"(a)", "m($1)") ==
    "m(a)"
  check "a".replace(re"(a)", "m($1) m($1)") ==
    "m(a) m(a)"
  check "aaa".replace(re"(a*)", "m($1)") ==
    "m(aaa)m()"  # nre's behaviour
  check "abc".replace(re"(a(b)c)", "m($1) m($2)") ==
    "m(abc) m(b)"
  check "abc".replace(re"(a(b))(c)", "m($1) m($2) m($3)") ==
    "m(ab) m(b) m(c)"
  check "abcabc".replace(re"(abc)*", "m($1)") ==
    "m(abcabc)m()"  # nre's behaviour
  check "abcabc".replace(re"(abc)", "m($1)") ==
    "m(abc)m(abc)"
  check "abcabc".replace(re"(abc)", "m($1)") ==
    "m(abc)m(abc)"
  check "abcab".replace(re"(abc)", "m($1)") ==
    "m(abc)ab"
  check "abcabc".replace(re"((abc)*)", "m($1) m($2)") ==
    "m(abcabc) m(abcabc)m() m()"  # nre's behaviour
  check "abcabc".replace(re"((a)bc)*", "m($1) m($2)") ==
    "m(abcabc) m(aa)m() m()"
  check "abc".replace(re"(b)", "m($1)") == "am(b)c"
  check "abc".replace(re"d", "m($1)") == "abc"
  check "abc".replace(re"(d)", "m($1)") == "abc"
  check "aaa".replace(re"a", "b") == "bbb"
  check "aaa".replace(re"a", "b", 1) == "baa"
  check "Nim is awesome!".replace(re"(\w\B)", "$1_") ==
    "N_i_m i_s a_w_e_s_o_m_e!"

  block:
    proc by(m: RegexMatch, s: string): string =
      result = "m("
      for g in 0 ..< m.groupsCount:
        for sl in m.group(g):
          result.add(s[sl])
          result.add(',')
      result.add(')')

    check "abc".replace(re"(b)", by) == "am(b,)c"
    check "aaa".replace(re"(a*)", by) == "m(aaa,)m(,)"
    check "aaa".replace(re"(a)*", by) == "m(a,a,a,)m()"

  block:
    proc removeEvenWords(m: RegexMatch, s: string): string =
      if m.group(1).len mod 2 != 0:
        result = s[m.group(0)[0]]
      else:
        result = ""

    let
      text = "Es macht Spaß, alle geraden Wörter zu entfernen!"
      expected = "macht , geraden entfernen!"
    check text.replace(re"((\w)+\s*)", removeEvenWords) == expected

# VM registry error on Nim < 1.1 (devel)
when not defined(runTestAtCT) or (NimMajor, NimMinor) > (1, 0):
  test "tmisc":
    block:
      var m: RegexMatch
      check "abc".match(re"[^^]+", m)
      check m.boundaries == 0 .. 2
    check(not "^".isMatch(re"[^^]+"))
    block:
      var m: RegexMatch
      check "kpd".match(re"[^al-obc]+", m)
      check m.boundaries == 0 .. 2
    check(not "abc".isMatch(re"[^al-obc]+"))
    block:
      var m: RegexMatch
      check "almocb".match(re"[al-obc]+", m)
      check m.boundaries == 0 .. 5
    check(not "defzx".isMatch(re"[al-obc]+"))

    # From http://www.regular-expressions.info/examples.html
    # Grabbing HTML Tags
    block:
      var m: RegexMatch
      check "one<TAG>two</TAG>three".find(re"<TAG\b[^>]*>(.*?)</TAG>", m)
      check m.boundaries == 3 .. 16
    check("one<TAG>two</TAG>three".findWithCapt(
      re"<TAG\b[^>]*>(.*?)</TAG>") == @[@["two"]])
    # IP Addresses
    block:
      const ip = re"""(?x)
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
        @[@["127"], @["0"], @["0"], @["1"]]
      check(not "127.0.0.999".isMatch(ip))
    # Floating Point Numbers
    block:
      var m: RegexMatch
      check "3.14".find(re"^[-+]?[0-9]*\.?[0-9]+$", m)
      check m.boundaries == 0 .. 3
    check "1.602e-19".findWithCapt(
      re"^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$") == @[@["e-19"]]
    # E-mail Addresses
    block:
      const email = re"""(?x)
      \b
      [a-zA-Z0-9._%+-]+
      @
      (?:[a-zA-Z0-9-]+\.)+
      [a-zA-Z]{2,4}
      \b
      """
      var m: RegexMatch
      check "john@server.department.company.com".find(email, m)
      check m.boundaries == 0 .. 33
      check(not "john@aol...com".isMatch(email))
    block:
      const email = re"""(?x)
      [a-z0-9!#$%&'*+/=?^_`{|}~-]+
      (?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*
      @
      (?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+
      [a-z0-9](?:[a-z0-9-]*[a-z0-9])?
      """
      check "john@server.department.company.com".isMatch(email)
      check(not "john@aol...com".isMatch(email))
    block:
      const email = re"""(?x)
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
      const date = re"""(?x)
      ^((?:19|20)\d\d)[- /.]
      (0[1-9]|1[012])[- /.]
      (0[1-9]|[12][0-9]|3[01])$
      """
      check "1999-01-01".findWithCapt(date) ==
        @[@["1999"], @["01"], @["01"]]
      check "1999/01-01".findWithCapt(date) ==
        @[@["1999"], @["01"], @["01"]]
      check(not "1999-13-33".isMatch(date))
    # Near operator emulation
    block:
      const nope = re"\bword1\W+(?:\w+\W+){1,6}?word2\b"
      check(not "word1 word2".isMatch(nope))
      check "word1 1 word2".isMatch(nope)
      check "word1 1 2 3 4 5 6 word2".isMatch(nope)
      check(not "word1 1 2 3 4 5 6 7 word".isMatch(nope))

    # Unicode
    block:
      var m: RegexMatch
      check "①②③".find(re"①②③", m)
      check m.boundaries == 0 ..< "①②③".len
    block:
      var m: RegexMatch
      check "①②③④⑤".find(re"①②③", m)
      check m.boundaries == 0 ..< "①②③".len
    block:
      var m: RegexMatch
      check "①②③".find(re"①(②)③", m)
      check m.boundaries == 0 ..< "①②③".len
    check "①②③".findWithCapt(re"①(②)③") == @[@["②"]]
    block:
      var m: RegexMatch
      check "①②③".find(re"[①②③]*", m)
      check m.boundaries == 0 ..< "①②③".len
    #
    block:
      var m: RegexMatch
      check "①②③".find(re"[^④⑤]*", m)
      check m.boundaries == 0 ..< "①②③".len

test "tlook_around":
  check "ab".isMatch(re"a(?=b)\w")
  check(not "ab".isMatch(re"a(?=b)"))
  check(not "ab".isMatch(re"a(?=c)\w"))
  check "ab".matchWithCapt(re"(a(?=b))b") == @[@["a"]]

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
    var m: RegexMatch
    check "2222".find(re"(22)*", m)
    check m.group(0) == @[0 .. 1, 2 .. 3]

    check "abcd".find(re"(ab)", m)
    check m.group(0) == @[0 .. 1]

    check "abcd".find(re"(bc)", m)
    check m.group(0) == @[1 .. 2]

    check "abcd".find(re"(cd)", m)
    check m.group(0) == @[2 .. 3]

  block:
    var m: RegexMatch
    check "foobar".match(re"(?P<foo>(?P<bar>\w*))", m)
    check m.group("foo") == @[0..5]
    check m.group("bar") == @[0..5]

    check "foobar".match(re"(?P<foo>\w*)", m)
    check m.group("foo") == @[0..5]
    expect(ValueError):
      discard m.group("bar")

test "tisInitialized":
  block:
    var re: Regex
    check(not re.isInitialized)
    re = re"foo"
    check re.isInitialized

test "capturingGroupsNames":
  block:
    let text = "hello world"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?P<who>world)", m)
    doAssert m.groupsCount == 2
    for name in @["greet", "who"]:
      doAssert m.groupNames.contains(name)

  block:
    let text = "hello world"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?P<who>world)", m)
    doAssert m.group("greet", text) == @["hello"]
    doAssert m.group("who", text) == @["world"]
    
  block:
    let text = "hello world foo bar"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    doAssert m.group("greet", text) == @["hello"]
    let whoGroups = m.group("who", text)
    for w in @["foo", "bar", "world"]:
      doAssert whoGroups.contains(w)

  block:
    let text = "hello world"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?P<who>world)", m)
    doAssert m.groupFirstCapture("greet", text) == "hello"
    doAssert m.groupFirstCapture("who", text) == "world"

  ## First capture
  block:
    let text = "hello world her"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?P<who>world) (?P<who>her)", m)
    doAssert m.groupFirstCapture("greet", text) == "hello"

  block:
    let text = "hello world foo bar"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    # "who" captures @["world", "foo", "bar"]
    doAssert m.groupFirstCapture("who", text) == "world"
  
  block:
    let text = "hello"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello)\s?(?P<who>world)?", m)
    doAssert m.groupFirstCapture("greet", text) == "hello"
    doAssert m.groupFirstCapture("who", text) == ""

  ## Last capture
  block:
    let text = "hello world her"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?P<who>world) (?P<who>her)", m)
    doAssert m.groupLastCapture("who", text) == "her"

  block:
    let text = "hello world foo bar"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    # "who" captures @["world", "foo", "bar"]
    doAssert m.groupLastCapture("who", text) == "bar"

  block:
    let text = "hello"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello)\s?(?P<who>world)?", m)
    doAssert m.groupLastCapture("greet", text) == "hello"
    doAssert m.groupLastCapture("who", text) == ""

# XXX raise a compile error when regex contains unicode
#     in ascii mode
test "tflags":
  var m: RegexMatch
  #check match("abc", re(r"abc", {reAscii}), m)
  check match("弢弢弢", re"\w{3}", m)
  #check(not match("弢弢弢", re(r"\w{3}", {reAscii}), m))
  check re"\w" in "弢"
  #check re(r"\w", {reAscii}) notin "弢"
  #check re(r"\w", {reAscii}) in "a"
  #check "%ab%".find(re(r"\w{2}", {reAscii}), m)
  check "%弢弢%".find(re"\w{2}", m)
  #check(not "%弢弢%".find(re(r"\w{2}", {reAscii}), m))

test "tmisc2":
  var m: RegexMatch
  check "one<TAG>two</TAG>tree".find(re"<TAG>.*?</TAG>", m)
  check m.boundaries == 3 .. 16
  check "one<TAG>two</TAG>tree".find(re"<TAG>[\w<>/]*?</TAG>", m)
  check m.boundaries == 3 .. 16
  check "<TAG>two</TAG>".match(re"<TAG>.*?</TAG>", m)
  check match("abc", re"abc", m)
  check match("ab", re"a(b|c)", m)
  check match("ac", re"a(b|c)", m)
  check(not match("ad", re"a(b|c)", m))
  check match("ab", re"(ab)*", m)
  check match("abab", re"(ab)*", m)
  check(not match("ababc", re"(ab)*", m))
  check(not match("a", re"(ab)*", m))
  check match("ab", re"(ab)+", m)
  check match("abab", re"(ab)+", m)
  check(not match("ababc", re"(ab)+", m))
  check(not match("a", re"(ab)+", m))
  check match("aa", re"\b\b\baa\b\b\b", m)
  check(not match("cac", re"c\ba\bc", m))
  check match("abc", re"[abc]+", m)
  check match("abc", re"[\w]+", m)
  check match("弢弢弢", re"[\w]+", m)
  check(not match("abc", re"[\d]+", m))
  check match("123", re"[\d]+", m)
  check match("abc$%&", re".+", m)
  check(not match("abc$%&\L", re"(.+)", m))
  check(not match("abc$%&\L", re".+", m))
  check(not match("弢", re"\W", m))
  check match("$%&", re"\W+", m)
  check match("abc123", re"[^\W]+", m)
  check match("aabcd", re"(aa)bcd", m) and
    m.captures == @[@[0 .. 1]]
  check match("aabc", re"(aa)(bc)", m) and
    m.captures == @[@[0 .. 1], @[2 .. 3]]
  check match("ab", re"a(b|c)", m) and
    m.captures == @[@[1 .. 1]]
  check match("ab", re"(ab)*", m) and
    m.captures == @[@[0 .. 1]]
  check match("abab", re"(ab)*", m) and
    m.captures == @[@[0 .. 1, 2 .. 3]]
  check match("ab", re"((a))b", m) and
    m.captures == @[@[0 .. 0], @[0 .. 0]]
  check match("c", re"((ab)*)c", m) and
    m.captures == @[@[0 .. -1], @[]]
  check match("aab", re"((a)*b)", m) and
    m.captures == @[@[0 .. 2], @[0 .. 0, 1 .. 1]]
  check match("abbbbcccc", re"a(b|c)*", m) and
    m.captures == @[@[1 .. 1, 2 .. 2, 3 .. 3, 4 .. 4, 5 .. 5, 6 .. 6, 7 .. 7, 8 .. 8]]
  check match("ab", re"(a*)(b*)", m) and
    m.captures == @[@[0 .. 0], @[1 .. 1]]
  check match("ab", re"(a)*(b)*", m) and
    m.captures == @[@[0 .. 0], @[1 .. 1]]
  check match("ab", re"(a)*b*", m) and
    m.captures == @[@[0 .. 0]]
  check match("abbb", re"((a(b)*)*(b)*)", m) and
    m.captures == @[@[0 .. 3], @[0 .. 3], @[1 .. 1, 2 .. 2, 3 .. 3], @[]]
  check match("aa", re"(a)+", m) and
    m.captures == @[@[0 .. 0, 1 .. 1]]
  check match("abab", re"(ab)+", m) and
    m.captures == @[@[0 .. 1, 2 .. 3]]
  check match("a", re"(a)?", m) and
    m.captures == @[@[0 .. 0]]
  check match("ab", re"(ab)?", m) and
    m.captures == @[@[0 .. 1]]
  check match("aaabbbaaa", re"(a*|b*)*", m) and
    m.captures == @[@[0 .. 2, 3 .. 5, 6 .. 8]]
  check match("abab", re"(a(b))*", m) and
    m.captures == @[@[0 .. 1, 2 .. 3], @[1 .. 1, 3 .. 3]]
  check match("aaanasdnasd", re"((a)*n?(asd)*)*", m) and
    m.captures == @[@[0 .. 6, 7 .. 10], @[0 .. 0, 1 .. 1, 2 .. 2], @[4 .. 6, 8 .. 10]]
  check match("aaanasdnasd", re"((a)*n?(asd))*", m) and
    m.captures == @[@[0 .. 6, 7 .. 10], @[0 .. 0, 1 .. 1, 2 .. 2], @[4 .. 6, 8 .. 10]]
  check match("abd", re"((ab)c)|((ab)d)", m) and
    m.captures == @[@[], @[], @[0 .. 2], @[0 .. 1]]
  check match("aaa", re"(a*)", m) and
    m.captures == @[@[0 .. 2]]
  check match("aaaa", re"(a*)(a*)", m) and
    m.captures == @[@[0 .. 3], @[4 .. 3]]
  check match("aaaa", re"(a*?)(a*?)", m) and
    m.captures == @[@[0 .. -1], @[0 .. 3]]
  check match("aaaa", re"(a)*(a)", m) and
    m.captures == @[@[0 .. 0, 1 .. 1, 2 .. 2], @[3 .. 3]]
  check "11222211".find(re"(22)+", m) and
    m.group(0) == @[2 .. 3, 4 .. 5]
  check match("650-253-0001", re"[0-9]+-[0-9]+-[0-9]+", m)
  check(not match("abc-253-0001", re"[0-9]+-[0-9]+-[0-9]+", m))
  check(not match("650-253", re"[0-9]+-[0-9]+-[0-9]+", m))
  check(not match("650-253-0001-abc", re"[0-9]+-[0-9]+-[0-9]+", m))
  check match("650-253-0001", re"[0-9]+..*", m)
  check(not match("abc-253-0001", re"[0-9]+..*", m))
  check(not match("6", re"[0-9]+..*", m))
  block:
    const re1 = re"(11)*+(111)*"
    check match("", re1)
    check match("11", re1)
    check match("111", re1)
    check match("11111", re1)
    check match("1111111", re1)
    check match("1111111111", re1)
    check(not match("1", re1))
  block:
    const re1 = re"(11)+(111)*"
    check(not match("", re1))
    check match("11", re1)
    check(not match("111", re1))
    check match("11111", re1)
  block:
    const re1 = re"(aabb)(ab)*"
    check match("aabb", re1)
    check match("aabbab", re1)
    check match("aabbabab", re1)
    check(not match("ab", re1))
    check(not match("aabbaba", re1))
  block:
    const re1 = re"0(10)*"
    check match("0", re1)
    check match("010", re1)
    check(not match("", re1))
    check(not match("0101", re1))
    check(not match("0100", re1))
    check(not match("00", re1))
    check(not match("000", re1))
  block:
    const re1 = re"(11)*|(111)*"
    check match("", re1)
    check match("11", re1)
    check match("111", re1)
    check match("1111", re1)
    check match("111111", re1)
    check(not match("1", re1))
  block:  # issue #61
    const a = "void __mingw_setusermatherr (int (__attribute__((__cdecl__)) *)(struct _exception *));"
    check replace(a, re"__attribute__[ ]*\(\(.*?\)\)([ ,;])", "$1") ==
      "void __mingw_setusermatherr (int ( *)(struct _exception *));"
    check replace(a, re"__attribute__[ ]*\(\(.*?\)\)(.*?[ ,;])", "$1") ==
      "void __mingw_setusermatherr (int ( *)(struct _exception *));"
    check find(a, re"__attribute__[ ]*\(\(.*?\)\)([ ,;])", m) and
      a[m.boundaries] == "__attribute__((__cdecl__)) "
    check find(a, re"__attribute__[ ]*\(\(.*?\)\)(.*?[ ,;])", m) and
      a[m.boundaries] == "__attribute__((__cdecl__)) "
    # non-greedy
    check find(a, re"__attribute__[ ]*\(\(.*\)\)([ ,;])", m) and
      a[m.boundaries] == "__attribute__((__cdecl__)) *)(struct _exception *));"
  # issue #29
  check replace("foo", re"", "-") == "-f-o-o-"
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
    check replace(input, re"(?m)^", "//") == expected
  check replace("bar", re"^", "foo") == "foobar"
  check replace("foo", re"$", "bar") == "foobar"
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
    check replace(input, re"(?m)$", "//") == expected
  # We treat start as text[start..^1], see issue #64
  check find("foobarbar", re"^bar", m, start=3)
  check find("foobarbar", re"^bar", m, start=3) and
    m.boundaries == 3 .. 5
  check find("foobar\nbar", re"(?m)^bar", m, start=4) and
    m.boundaries == 7 .. 9
  block:
    # The bounds must contain the empty match index
    check find("foo\nbar\nbar", re"(?m)^", m) and
      m.boundaries == 0 .. -1
    check find("foo\nbar\nbar", re"(?m)^", m, start=1) and
      m.boundaries == 1 .. 0
    check find("foo\nbar\nbar", re"(?m)$", m) and
      m.boundaries == 3 .. 2
    check find("foo\nbar\nbar", re"(?m)$", m, start=3) and
      m.boundaries == 3 .. 2
    check find("foo\nbar\nbar", re"(?m)$", m, start=4) and
      m.boundaries == 7 .. 6
    check find("foo\nbar\nbar", re"(?m)$", m, start=7) and
      m.boundaries == 7 .. 6
    check find("foo\nbar\nbar", re"(?m)$", m, start=8) and
      m.boundaries == 11 .. 10
    check find("foo\nbar\nbar", re"(?m)$", m, start=11) and
      m.boundaries == 11 .. 10
    # start is out of bounds, but this is what Nim's re
    # does, nre throws an error
    check find("foo\nbar\nbar", re"(?m)$", m, start=12) and
      m.boundaries == 12 .. 11
  # XXX make this return false?
  check match("abc", re"(?m)$", m, start=50)
  check match("abc", re"(?m)^", m, start=50)
  # this should be valid though
  check match("", re"(?m)^", m)
  check match("", re"(?m)$", m)
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
    check split(input, re"(?m)^") == expected
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
    check split(input, re"(?m)$") == expected
  check split("acb\nb\nc\n", re"(?m)^") == @["acb\n", "b\n", "c\n"]
  check split("a b", re"\b") == @["a", " ", "b"]
  check split("ab", re"\b") == @["ab"]
  check split("iaiaiai", re"i") == @["", "a", "a", "a", ""]
  check split("aiaia", re"i") == @["a", "a", "a"]
  check split("aaa", re"a") == @["", "", "", ""]
  check split("a\na\na", re"(?m)^") == @["a\n", "a\n", "a"]
  check split("\n\n", re"(?m)^") == @["\n", "\n"]
