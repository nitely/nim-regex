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
  check "".matchWithBounds(re2"(a*)*?(a*)*?") == @[0 .. -1, 0 .. -1]
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
  check "".matchWithBounds(re2"(a+)*") == @[0 .. -1]
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