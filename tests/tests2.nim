import ./regex

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
  proc isMatch(s: string, pattern: Regex): bool =
    var m: RegexMatch2
    result = match2(s, pattern, m)
else:
  proc isMatch(s: string, pattern: static Regex): bool =
    var m: RegexMatch2
    result = match2(s, pattern, m)

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

proc matchWithCapt(s: string, pattern: static Regex): seq[string] =
  var m: RegexMatch2
  check match2(s, pattern, m)
  result.setLen m.captures.len
  for i, bounds in m.captures.pairs:
    result[i] = s[bounds]

proc toStrCaptures(m: RegexMatch2, s: string): seq[string] =
  result.setLen m.captures.len
  for i, bounds in m.captures.pairs:
    result[i] = s[bounds]

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
  check "**".isMatch(re"\**")
  check "++".isMatch(re"\++")
  check "??".isMatch(re"\?+")
  check "??".isMatch(re"\?*")
  check "?".isMatch(re"\??")
  check "?".isMatch(re"\???")
  check "**".isMatch(re"\**?")
  check "++".isMatch(re"\++?")
  check "??".isMatch(re"\?+?")
  check "??".isMatch(re"\?*?")
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
  check "aaa".isMatch(re"(a*)*")
  check "aaabbbaaa".isMatch(re"((a*|b*))*")
  check raises(r"a*****")
  check raises(r"a*{,}")
  check "aaa".isMatch(re"(a?)*")
  check "aaaa".isMatch(re"((a)*(a)*)*")
  # Same as PCRE "^(a*)*?$"
  check "aa".matchWithCapt(re"(a*)*?") == @["aa"]
  check "aa".matchWithCapt(re"(a*)*?(a*)*?") == @["", "aa"]
  check "".matchWithCapt(re"(a*)*?(a*)*?") == @["", ""]
  check "aa".matchWithCapt(re"(.*?)") == @["aa"]
  check "aa".matchWithCapt(re"(.*)*") == @[""]
  check "a".matchWithCapt(re"(a*)*") == @[""]
  check "a".matchWithCapt(re"(a*)*(a*)*") == @["", ""]
  check "".matchWithCapt(re"(a*)*") == @[""]
  check "".matchWithCapt(re"(a*)*(a*)*") == @["", ""]
  check "a".matchWithCapt(re"(a*)*?") == @["a"]
  check "a".matchWithCapt(re"(a?)*?") == @["a"]
  check "a".matchWithCapt(re"(a*?)*") == @[""]
  check "a".matchWithCapt(re"(a*?)*?") == @["a"]
  check "a".matchWithCapt(re"(a??)*") == @[""]
  check "ab".matchWithCapt(re"(a??)*b") == @[""]
  check "".matchWithCapt(re"(a??)*") == @[""]
  check "a".matchWithCapt(re"(a?)??") == @["a"]
  check "a".matchWithCapt(re"(a*)??") == @["a"]
  check "a".matchWithCapt(re"(a*)+?") == @["a"]
  check "".matchWithCapt(re"(a?)+") == @[""]
  check "".matchWithCapt(re"(a?)(a?)*") == @["", ""]
  check "a".matchWithCapt(re"(a?)+") == @[""]
  check "".matchWithCapt(re"(a*)+") == @[""]
  check "".matchWithCapt(re"(a*)(a*)*") == @["", ""]
  check "a".matchWithCapt(re"(a*)+") == @[""]
  check "b".matchWithCapt(re"(a*)+b") == @[""]
  check "b".matchWithCapt(re"(a*)+b*") == @[""]
  check "ab".matchWithCapt(re"(a*)+b") == @[""]
  check "".matchWithCapt(re"(a?)*") == @[""]
  check "a".matchWithCapt(re"(a?)*") == @[""]
  check "a".matchWithCapt(re"(a?)*(a?)*") == @["", ""]
  check "ab".matchWithCapt(re"(a?)*b") == @[""]
  check "".matchWithCapt(re"(a+)*") == @[""]
  check "a".matchWithCapt(re"(?:a*)*") == newSeq[string]()
  check "a".matchWithCapt(re"(a?b?)*") == @[""]
  check "".matchWithCapt(re"(a?b?)*") == @[""]

test "talternations":
  check raises(r"a|?")
  check raises(r"a|?b")
  check raises(r"?|?")
  check raises(r"a|*")
  check raises(r"a|*b")
  check raises(r"a|+")
  check raises(r"a|+b")

test "tcaptures":
  check "ab".matchWithCapt(re"(a)b") == @["a"]
  check "aa".matchWithCapt(re"(a)*") == @["a"]
  check "aab".matchWithCapt(re"((a)*b)") == @["aab", "a"]
  check "abbbbccccd".matchWithCapt(re"a(b|c)*d") == @["c"]
  check "abbb".matchWithCapt(re"((a)*(b)*)") == @["abbb", "a", "b"]
  check "abbb".matchWithCapt(re"((a(b)*)*(b)*)") ==
    @["abbb", "abbb", "b", ""]
  check "aa".matchWithCapt(re"(a)+") == @["a"]
  check "abab".matchWithCapt(re"(ab)+") == @["ab"]
  check "a".matchWithCapt(re"(a)?") == @["a"]
  check "ab".matchWithCapt(re"(ab)?") == @["ab"]
  check "aaabbbaaa".matchWithCapt(re"(a*|b*)*") == @[""]
  check "abab".matchWithCapt(re"(a(b))*") == @["ab", "b"]
  check "aaanasdnasd".matchWithCapt(re"((a)*n?(asd)*)*") ==
    @["", "a", "asd"]
  check "aaanasdnasd".matchWithCapt(re"((a)*n?(asd))*") ==
    @["nasd", "a", "asd"]
  check "b".matchWithCapt(re"(a)?b") == @[""]
  check "ฅa".matchWithCapt(re"(\w)(a)") == @["ฅ", "a"]

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
  check "a".matchWithCapt(re"(\w)") == @["a"]

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
  check "a".matchWithCapt(re"([^b])") == @["a"]
  check "asd".matchWithCapt(re"([^b]*)") == @["asd"]
  check "ab".matchWithCapt(re"([^b]*b)") == @["ab"]
  check "asd123".matchWithCapt(re"([^\d]*)(\d*)") == @["asd", "123"]
  check "asd123".matchWithCapt(re"([asd]*)([^asd]*)") == @["asd", "123"]
  check "<asd123!@#>".matchWithCapt(re"(<[^>]*>)") == @["<asd123!@#>"]
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
  check "".isMatch(re"a{0,2}")
  check "a".isMatch(re"a{0}")
  check "a".isMatch(re"a{0,0}")
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
  check "aaaaaaaaaa".isMatch(re"a{0,}")
  check "".isMatch(re"a{0,}")
  check(not "a".isMatch(re"a{2,}"))
  check "a{a,1}".isMatch(re"a{a,1}")
  check(not "a".isMatch(re"a{a,1}"))
  check(not "a1".isMatch(re"a{a,1}"))
  check "a{".isMatch(re"a{")
  check "a{{".isMatch(re"a{{")
  check "a{}".isMatch(re"a{}")
  check raises(r"a*{,}")
  check raises(r"a*{0}")
  check raises(r"a*{1}")
  check "aaa".matchWithCapt(re"(a){0,}") == @["a"]
  check "aaa".matchWithCapt(re"(a{0,}){0,}") == @[""]
  check "aaaaa".matchWithCapt(re"(a){5}") == @["a"]
  check "a".matchWithCapt(re"(a){1,5}") == @["a"]
  check "aaa".matchWithCapt(re"(a){1,5}") == @["a"]
  check "".matchWithCapt(re"(a){0,}") == @[""]
  check "aaa".matchWithCapt(re"(a{0,}){0,}") == @[""]
  check "aaa".matchWithCapt(re"(a{1}){0,}") == @["a"]
  check "aaaa".matchWithCapt(re"(a{2}){0,}") == @["aa"]
  check "aaaa".matchWithCapt(re"(a{0,3}){0,}") == @[""]
  check "".matchWithCapt(re"(a{0,3}){0,}") == @[""]
  check "aaa".matchWithCapt(re"(a{1,}){0,}") == @["aaa"]
  check "".matchWithCapt(re"(a{1,}){0,}") == @[""]
  check(not "".isMatch(re"(a{1,})"))
  check "a".matchWithCapt(re"(a{1,})") == @["a"]
  check "aaa".matchWithCapt(re"(a{1,})") == @["aaa"]
  check "abab".matchWithCapt(re"(a(b)){2}") == @["ab", "b"]
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
  check "abab".matchWithCapt(re"(a(b))*") == @["ab", "b"]
  check "abab".matchWithCapt(re"(?:a(b))*") == @["b"]
  check "abab".matchWithCapt(re"(a(?:b))*") == @["ab"]
  check ")".matchWithCapt(re"(\))") == @[")"]

test "tgreediness":
  check "a".matchWithCapt(re"(a)??") == @["a"]
  check "aaa".matchWithCapt(re"(a)*(a)*(a)*") == @["a", "", ""]
  check "aaa".matchWithCapt(re"(a)*?(a)*(a)*?") == @["", "a", ""]
  check "aaa".matchWithCapt(re"(a)*?(a)*?(a)*") == @["", "", "a"]
  check "aaa".matchWithCapt(re"(a)*?(a)*?(a)*?") == @["", "", "a"]
  check "aaaa".matchWithCapt(re"(a)*?(a)*?(a)*?") == @["", "", "a"]
  check "aa".matchWithCapt(re"(a)?(aa?)") == @["a", "a"]
  check "aa".matchWithCapt(re"(a)??(a)") == @["a", "a"]
  check "aa".matchWithCapt(re"(a)??(aa?)") == @["", "aa"]
  check "aaa".matchWithCapt(re"(a)+(a)+(a)?") == @["a", "a", ""]
  check "aaa".matchWithCapt(re"(a)+?(a)+(a)?") == @["a", "a", ""]
  check "aaa".matchWithCapt(re"(a)+?(a)+?(a)?") == @["a", "a", "a"]
  check "aaa".matchWithCapt(re"(a){0,}(a){0,}(a){0,}") == @["a", "", ""]
  check "aaa".matchWithCapt(re"(a){0,}?(a){0,}(a){0,}?") == @["", "a", ""]
  check "aaa".matchWithCapt(re"(a){0,}?(a){0,}?(a){0,}") == @["", "", "a"]
  check "aaa".matchWithCapt(re"(a){0,}?(a){0,}?(a){0,}?") == @["", "", "a"]
  check "aaa".matchWithCapt(re"(a){1,}(a){1,}(a)?") == @["a", "a", ""]
  check "aaa".matchWithCapt(re"(a){1,}?(a){1,}(a)?") == @["a", "a", ""]
  check "aaa".matchWithCapt(re"(a){1,}?(a){1,}?(a)?") == @["a", "a", "a"]
  block:
    var m: RegexMatch2
    check match2("aaaa", re"(a*?)(a*?)(a*)", m)
    check m.toStrCaptures("aaaa") == @["", "", "aaaa"]
  block:
    var m: RegexMatch2
    check match2("aaaa", re"(a*)(a*?)(a*?)", m)
    check m.toStrCaptures("aaaa") == @["aaaa", "", ""]

test "tassertions":
  check "bbaa aa".matchWithCapt(re"([\w ]*?)(\baa\b)") == @["bbaa ", "aa"]
  check "aa bbaa".matchWithCapt(re"(\baa\b)([\w ]*)") == @["aa", " bbaa"]
  check "This island is great".matchWithCapt(
      re"([\w ]*?)(\bis\b)([\w ]*?)") == @["This island ", "is", " great"]
  check "bbaabb".matchWithCapt(re"([\w ]*?)(\Baa\B)([\w ]*?)") ==
    @["bb", "aa", "bb"]
  check "This is my sister".matchWithCapt(
      re"([\w ]*?)(\Bis\B)([\w ]*?)") == @["This is my s", "is", "ter"]
  check "aa".isMatch(re"\b\b\baa\b\b\b")
  check "bb".isMatch(re"^^^^bb$$$$")
  check "bb".isMatch(re"\A\A\A\Abb\z\z\z\z")

test "tdot_any_matcher":
  check "a".isMatch(re".")
  check "asd123!@#".isMatch(re".*")
  check "| (•□•) | (❍ᴥ❍ʋ)".isMatch(re".*")
  check "ฅ^•ﻌ•^ฅ".matchWithCapt(re"(.*)") == @["ฅ^•ﻌ•^ฅ"]
  check "\t".isMatch(re".")
  check(not "\L".isMatch(re".*"))

