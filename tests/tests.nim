from std/sequtils import map

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
  check match(s, pattern, m)
  result = m.toStrCaptures(s)

proc findWithCapt(s: string, pattern: Regex): seq[seq[string]] =
  var m: RegexMatch
  check find(s, pattern, m)
  result = m.toStrCaptures(s)

func findAllCapt(s: string, reg: Regex): seq[seq[seq[Slice[int]]]] =
  result = map(
    findAll(s, reg),
    func (m: RegexMatch): seq[seq[Slice[int]]] =
      for i in 0 .. m.groupsCount-1:
        result.add m.group(i))

when (NimMajor, NimMinor) >= (1, 1):
  template matchMacro(s, r: untyped): untyped =
    (func (): bool =
      result = false
      let exp = s
      match exp, r:
        result = true)()

  template matchMacroCapt(s, r: untyped): untyped =
    (func (): seq[string] =
      var m = false
      let exp = s
      match exp, r:
        m = true
        result = matches
      check m)()

  test "tmatch_macro":
    block hasOwnScope:
      var m = false
      var matches: seq[string]
      match "abc", rex"(\w+)":
        check matches == @["abc"]
        m = true
      check m
      check matches.len == 0
    block:
      var m = false
      match "abc", rex"(\w)+":
        check matches == @["c"]
        m = true
      check m
    block:
      var m = false
      match "abc", rex"(a(b)c)":
        check matches == @["abc", "b"]
        m = true
      check m
    block:
      var m = false
      match "x", rex"y":
        m = true
      check not m
      match "y", rex"y":
        m = true
      check m
    block:
      var m = false
      match "abc", rex"""(?x)
        abc  # verbose mode
      """:
        m = true
      check m
    block:
      template myRegex: untyped =
        rex"""(?x)
          abc  # verbose mode
        """
      var m = false
      match "abc", myRegex:
        m = true
      check m
    block:
      var m = false
      var txt = "abc"
      match txt, rex"(\w)+":
        m = true
      check m
    block:
      (func () = 
        var m = false
        var txt = "abc"
        match txt, rex"(\w)+":
          m = true
        check m)()
    block:
      var m = false
      match "abcdefg", rex"\w+(?<=(ab)(?=(cd)))\w+":
        check matches == @["ab", "cd"]
        m = true
      check m
  
  test "tmatch_macro_captures":
    check matchMacroCapt("ab", rex"(a)b") == @["a"]
    check matchMacroCapt("aa", rex"(a)*") == @["a"]
    check matchMacroCapt("aab", rex"((a)*b)") == @["aab", "a"]
    check matchMacroCapt("abbbbccccd", rex"a(b|c)*d") == @["c"]
    check matchMacroCapt("abbb", rex"((a)*(b)*)") == @["abbb", "a", "b"]
    check matchMacroCapt("abbb", rex"((a(b)*)*(b)*)") ==
      @["abbb", "abbb", "b", ""]
    check matchMacroCapt("aa", rex"(a)+") == @["a"]
    check matchMacroCapt("abab", rex"(ab)+") == @["ab"]
    check matchMacroCapt("a", rex"(a)?") == @["a"]
    check matchMacroCapt("ab", rex"(ab)?") == @["ab"]
    check matchMacroCapt("aaabbbaaa", rex"(a*|b*)*") == @[""]
    check matchMacroCapt("abab", rex"(a(b))*") == @["ab", "b"]
    check matchMacroCapt("aaanasdnasd", rex"((a)*n?(asd)*)*") ==
      @["", "a", "asd"]
    check matchMacroCapt("aaanasdnasd", rex"((a)*n?(asd))*") ==
      @["nasd", "a", "asd"]
    check matchMacroCapt("b", rex"(a)?b") == @[""]
    check matchMacroCapt("ฅa", rex"(\w)(a)") == @["ฅ", "a"]
    check matchMacroCapt("aabcd", rex"(aa)bcd") == @["aa"]
    check matchMacroCapt("aabc", rex"(aa)(bc)") == @["aa", "bc"]
    check matchMacroCapt("ab", rex"a(b|c)") == @["b"]
    check matchMacroCapt("ab", rex"(ab)*") == @["ab"]
    check matchMacroCapt("abab", rex"(ab)*") == @["ab"]
    check matchMacroCapt("ab", rex"((a))b") == @["a", "a"]
    check matchMacroCapt("c", rex"((ab)*)c") == @["", ""]
    check matchMacroCapt("aab", rex"((a)*b)") == @["aab", "a"]
    check matchMacroCapt("abbbbcccc", rex"a(b|c)*") == @["c"]
    check matchMacroCapt("ab", rex"(a*)(b*)") == @["a", "b"]
    check matchMacroCapt("ab", rex"(a)*(b)*") == @["a", "b"]
    check matchMacroCapt("ab", rex"(a)*b*") == @["a"]
    check matchMacroCapt("abd", rex"((ab)c)|((ab)d)") ==
      @["", "", "abd", "ab"]
    check matchMacroCapt("aaa", rex"(a*)") == @["aaa"]
    check matchMacroCapt("aaaa", rex"(a*)(a*)") == @["aaaa", ""]
    check matchMacroCapt("aaaa", rex"(a*?)(a*?)") == @["", "aaaa"]
    check matchMacroCapt("aaaa", rex"(a)*(a)") == @["a", "a"]
    check matchMacroCapt("abcdefg", rex"\w+(?<=(ab))\w+") == @["ab"]
    check matchMacroCapt("abcdefg", rex"\w+(?=(cd))\w+") == @["cd"]
    check matchMacroCapt("abcdefg", rex"\w+(?<=(ab))\w+(?<=(fg))") ==
      @["ab", "fg"]
    check matchMacroCapt("abcdefg", rex"\w+(?=(cd))\w+(?=(ef))\w+") ==
      @["cd", "ef"]
    check matchMacroCapt("abcdefg", rex"\w+(?<=(ab))\w+(?=(ef))\w+") ==
      @["ab", "ef"]
    check matchMacroCapt("abcdefg", rex"\w+(?<=(ab)cd(?<=(cd)))\w+") ==
      @["ab", "cd"]
    check matchMacroCapt("abcdefg", rex"\w+(?<=(ab)(?<=(ab)))\w+") ==
      @["ab", "ab"]
    check matchMacroCapt("abcdefg", rex"\w+(?<=(ab)(?=(cd)))\w+") ==
      @["ab", "cd"]
    check matchMacroCapt("abcdefg", rex"\w+(?<=(ab)(?=(cd)(?<=(cd))))\w+") ==
      @["ab", "cd", "cd"]
    check matchMacroCapt("abcdefg", rex"\w+(?=(cd)(?<=(cd)))\w+") ==
      @["cd", "cd"]

  test "tmatch_macro_misc":
    check matchMacro("abc", rex"\w+")
    check not matchMacro("abc@", rex"\w+")
    check matchMacro("", rex"")
    check matchMacro("a", rex"a")
    check matchMacro("ab", rex"(a)b")
    check matchMacro("aa", rex"(a)*")
    check matchMacro("aab", rex"((a)*b)")
    check matchMacro("abbbbccccd", rex"a(b|c)*d")
    check matchMacro("abbb", rex"((a)*(b)*)")
    check matchMacro("abbb", rex"((a(b)*)*(b)*)")
    check matchMacro("a", rex"a|b")
    check matchMacro("b", rex"a|b")
    check(not matchMacro("ab", rex"a(b|c)*d"))
    check(not matchMacro("a", rex"b"))
    check(not matchMacro("a", rex""))
    check matchMacro(" \"word\" ", rex"\s"".*""\s")
    check matchMacro("aaa", rex"(a*)*")
    check matchMacro("aaabbbaaa", rex"((a*|b*))*")
    check matchMacro("aaa", rex"(a?)*")
    check matchMacro("aaaa", rex"((a)*(a)*)*")
    check matchMacro("aaa", rex"(a*)*")
    check matchMacro("aaabbbaaa", rex"((a*|b*))*")
    check matchMacro("aaa", rex"(a?)*")
    check matchMacro("aaaa", rex"((a)*(a)*)*")
    check matchMacro("<TAG>two</TAG>", rex"<TAG>.*?</TAG>")
    check matchMacro("abc", rex"abc")
    check matchMacro("ab", rex"a(b|c)")
    check matchMacro("ac", rex"a(b|c)")
    check(not matchMacro("ad", rex"a(b|c)"))
    check matchMacro("ab", rex"(ab)*")
    check matchMacro("abab", rex"(ab)*")
    check(not matchMacro("ababc", rex"(ab)*"))
    check(not matchMacro("a", rex"(ab)*"))
    check matchMacro("ab", rex"(ab)+")
    check matchMacro("abab", rex"(ab)+")
    check(not matchMacro("ababc", rex"(ab)+"))
    check(not matchMacro("a", rex"(ab)+"))
  
  test "tmatch_macro_misc2":
    check matchMacro("aa", rex"\b\b\baa\b\b\b")
    check(not matchMacro("cac", rex"c\ba\bc"))
    check matchMacro("abc", rex"[abc]+")
    check matchMacro("abc", rex"[\w]+")
    check matchMacro("弢弢弢", rex"[\w]+")
    check(not matchMacro("abc", rex"[\d]+"))
    check matchMacro("123", rex"[\d]+")
    check matchMacro("abc$%&", rex".+")
    check(not matchMacro("abc$%&\L", rex"(.+)"))
    check(not matchMacro("abc$%&\L", rex".+"))
    check(not matchMacro("弢", rex"\W"))
    check matchMacro("$%&", rex"\W+")
    check matchMacro("abc123", rex"[^\W]+")
    check matchMacro("650-253-0001", rex"[0-9]+-[0-9]+-[0-9]+")
    check(not matchMacro("abc-253-0001", rex"[0-9]+-[0-9]+-[0-9]+"))
    check(not matchMacro("650-253", rex"[0-9]+-[0-9]+-[0-9]+"))
    check(not matchMacro("650-253-0001-abc", rex"[0-9]+-[0-9]+-[0-9]+"))
    check matchMacro("650-253-0001", rex"[0-9]+..*")
    check(not matchMacro("abc-253-0001", rex"[0-9]+..*"))
    check(not matchMacro("6", rex"[0-9]+..*"))
    check matchMacro("", rex"((11)*)+(111)*")
    check matchMacro("11", rex"((11)*)+(111)*")
    check matchMacro("111", rex"((11)*)+(111)*")
    check matchMacro("11111", rex"((11)*)+(111)*")
    check matchMacro("1111111", rex"((11)*)+(111)*")
    check matchMacro("1111111111", rex"((11)*)+(111)*")
    check(not matchMacro("1", rex"((11)*)+(111)*"))
    check(not matchMacro("", rex"(11)+(111)*"))
    check matchMacro("11", rex"(11)+(111)*")
    check(not matchMacro("111", rex"(11)+(111)*"))
    check matchMacro("11111", rex"(11)+(111)*")
    check matchMacro("aabb", rex"(aabb)(ab)*")
    check matchMacro("aabbab", rex"(aabb)(ab)*")
    check matchMacro("aabbabab", rex"(aabb)(ab)*")
    check(not matchMacro("ab", rex"(aabb)(ab)*"))
    check(not matchMacro("aabbaba", rex"(aabb)(ab)*"))
    check matchMacro("0", rex"0(10)*")
    check matchMacro("010", rex"0(10)*")
    check(not matchMacro("", rex"0(10)*"))
    check(not matchMacro("0101", rex"0(10)*"))
    check(not matchMacro("0100", rex"0(10)*"))
    check(not matchMacro("00", rex"0(10)*"))
    check(not matchMacro("000", rex"0(10)*"))
    check matchMacro("", rex"(11)*|(111)*")
    check matchMacro("11", rex"(11)*|(111)*")
    check matchMacro("111", rex"(11)*|(111)*")
    check matchMacro("1111", rex"(11)*|(111)*")
    check matchMacro("111111", rex"(11)*|(111)*")
    check(not matchMacro("1", rex"(11)*|(111)*"))

  test "tmacro_full_lookarounds":
    check matchMacro("ab", rex"a(?=b)\w")
    check(not matchMacro("ab", rex"a(?=x)\w"))
    check(not matchMacro("ab", rex"ab(?=x)"))
    check matchMacro("ab", rex"ab(?=$)")
    check matchMacro("abc", rex"a(?=b)\w+")
    check matchMacroCapt("abcdefg", rex"a(?=(bc))\w+") ==
      @["bc"]
    check matchMacro("ab", rex"\w(?<=a)b")
    check(not matchMacro("ab", rex"\w(?<=x)b"))
    check(not matchMacro("ab", rex"(?<=x)ab"))
    check matchMacro("ab", rex"(?<=^)ab")
    check matchMacro("abc", rex"\w\w(?<=(b))c")
    check matchMacroCapt("abc", rex"\w\w(?<=(b))c") ==
      @["b"]
    check matchMacro("abc", rex"\w\w(?<=(ab))c")
    check matchMacroCapt("abc", rex"\w\w(?<=(ab))c") ==
      @["ab"]
    check matchMacro("a", rex"\w(?<=a)")
    check(not matchMacro("a", rex"\w(?=a)"))
    check matchMacro("ab", rex"\w(?<=a(?<=a))b")
    check matchMacro("ab", rex"\w(?<=a(?<=a(?<=a)))b")
    check matchMacro("ab", rex"\w(?<=a(?=b))b")
    check(not matchMacro("ab", rex"\w(?=b(?=b))b"))
    check(not matchMacro("ab", rex"\w(?<=a(?=b(?=b)))b"))
    check matchMacro("ab", rex"\w(?<=a(?<=a)(?=b))b")
    check matchMacro("ab", rex"\w(?<=a(?<=a(?=b)))b")
    check(not matchMacro("ab", rex"\w(?<=(?<=a)a)b"))
    check matchMacro("aab", rex"\w\w(?<=aa(?=b))b")
    block:
      check matchMacroCapt("aaab", rex"(\w*)(\w*?)") ==
        @["aaab", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*)(\w*?)$)") ==
        @["aaab", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*)(\w*)$)") ==
        @["", "aaab"]
      check matchMacroCapt("aaab", rex"(\w*?)(\w*)(\w*?)") ==
        @["", "aaab", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*?)(\w*)(\w*?)$)") ==
        @["", "aaab", ""]
      check matchMacroCapt("aaab", rex"(\w*)(\w??)") ==
        @["aaab", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*)(\w??)$)") ==
        @["aaab", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*)(\w?)$)") ==
        @["aaa", "b"]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*?)(\w?)(\w*?)$)") ==
        @["aaa", "b", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*?)(\w??)(\w*?)$)") ==
        @["aaab", "", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w??)(\w*?)$)") ==
        @["a", "aab"]
      check matchMacroCapt("aaab", rex".*(?<=^(\w?)(\w*?)$)") ==
        @["a", "aab"]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*?)(\w??)$)") ==
        @["aaab", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*)(\w??)$)") ==
        @["aaab", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*?)(\w?)$)") ==
        @["aaa", "b"]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*)(\w?)$)") ==
        @["aaa", "b"]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*?)(\w*?)(\w??)$)") ==
        @["aaab", "", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*?)(\w*)(\w??)$)") ==
        @["", "aaab", ""]
    block:
      check matchMacroCapt("aaab", rex".*(?<=^(\w)\w+|(\w)\w+$)") ==
        @["a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\d)\w+|(\w)\w+$)") ==
        @["", "a"]
      check matchMacroCapt("aaab", rex".*(?<=^(\w)\w+|(\w)\w+|(\w)\w+$)") ==
        @["a", "", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\d)\w+|(\w)\w+|(\w)\w+$)") ==
        @["", "a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\d)\w+|(\d)\w+|(\w)\w+$)") ==
        @["", "", "a"]
    block:
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\w)\w+|(\w)\w+)+?$)") ==
        @["a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\d)\w+|(\w)\w+)+?$)") ==
        @["", "a"]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\w)\w+|(\w)\w+|(\w)\w+)+?$)") ==
        @["a", "", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\d)\w+|(\w)\w+|(\w)\w+)+?$)") ==
        @["", "a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\d)\w+|(\d)\w+|(\w)\w+)+?$)") ==
        @["", "", "a"]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\w)\w+|(\w)\w+)*?$)") ==
        @["a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\d)\w+|(\w)\w+)*?$)") ==
        @["", "a"]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\w)\w+|(\w)\w+|(\w)\w+)*?$)") ==
        @["a", "", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\d)\w+|(\w)\w+|(\w)\w+)*?$)") ==
        @["", "a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\d)\w+|(\d)\w+|(\w)\w+)*?$)") ==
        @["", "", "a"]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\w)\w+|(\w)\w+)??$)") ==
        @["a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\d)\w+|(\w)\w+)??$)") ==
        @["", "a"]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\w)\w+|(\w)\w+|(\w)\w+)??$)") ==
        @["a", "", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\d)\w+|(\w)\w+|(\w)\w+)??$)") ==
        @["", "a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(?:(\d)\w+|(\d)\w+|(\w)\w+)??$)") ==
        @["", "", "a"]
    block:
      check matchMacroCapt("aaab", rex".*(?<=^(\w)\w+?|(\w)\w+?$)") ==
        @["a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w)\w+|(\w)\w+$)") ==
        @["a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w)\w+?|(\w)\w+$)") ==
        @["a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w)\w+|(\w)\w+?$)") ==
        @["a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w+|\w{4}|\w{4}|\w+)*$)") ==
        @["aaab"]
      check matchMacroCapt("aaab", rex"(\w*|\w{4}|\w{4}|\w*)*") ==
        @[""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*|\w{4}|\w{4}|\w*)*$)") ==
        @["aaab"]
      check matchMacroCapt("aaab", rex".*(?<=^(\w+|\w{4}|\w{4}|\w+)+$)") ==
        @["aaab"]
      check matchMacroCapt("aaab", rex".*(?<=^(\w)\w+|\w{4}|\w{4}|(\w)\w+$)") ==
        @["a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\d)\w+|(\w{4})|(\w{4})|(\w)\w+$)") ==
        @["", "aaab", "", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\d)\w+|(\w)\w{3}|(\w)\w{3}|(\w)\w+$)") ==
        @["", "a", "", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\d)\w+|(\d)\w{3}|(\w)\w{3}|(\w)\w+$)") ==
        @["", "", "a", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\d)\w+|(\d)\w{3}|(\d)\w{3}|(\w)\w+$)") ==
        @["", "", "", "a"]
      check matchMacroCapt("aaab", rex"(\w*|\w{4}|\w{4}|\w*)+") ==
        @[""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*|\w{4}|\w{4}|\w*)+$)") ==
        @["aaab"]
      check matchMacroCapt("aaab", rex".*(?<=^(\w+)|(\w{4})|(\w{10})$)") ==
        @["aaab", "", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w{10})|(\w{4})|(\w+)$)") ==
        @["", "aaab", ""]
      check matchMacroCapt("aaab", rex".*(?<=^((\w{10})|(\w{4})|(\w+))+$)") ==
        @["aaab", "", "aaab", ""]
      check matchMacroCapt("aaab", rex".*(?<=^((\w{10})|(\w{4})|(\w+))*$)") ==
        @["aaab", "", "aaab", ""]
      check matchMacroCapt("aaab", rex".*(?<=^(\w+|\w{4}|\w{4}|\w+)*?$)") ==
        @["aaab"]
      check matchMacroCapt("aaab", rex".*(?<=^(\w*|\w{4}|\w{4}|\w*)*?$)") ==
        @["aaab"]
      check matchMacroCapt("aaab", rex".*(?<=^(\w+|\w{4}|\w{4}|\w+)+?$)") ==
        @["aaab"]
    check(not matchMacro("ab", rex"\w(?<=a(?=b(?<=a)))b"))
    check(not matchMacro("ab", rex"\w(?<=a(?<=a(?=b(?<=a))))b"))
    check matchMacro("ab", rex"\w(?<=a(?=b(?<=b)))b")
    check matchMacro("ab", rex"\w(?<=a(?<=a(?=b(?<=b))))b")
    block:
      template asterisk: untyped = rex".*(?<=(?<!\\)(?:\\\\)*)\*"
      check matchMacro(r"*", asterisk)
      check(not matchMacro(r"\*", asterisk))
      check matchMacro(r"\\*", asterisk)
      check(not matchMacro(r"\\\*", asterisk))
      check matchMacro(r"\\\\*", asterisk)
      check(not matchMacro(r"\\\\\*", asterisk))
    block:
      check matchMacro(r"弢b", rex"弢(?=b)\w")
      check matchMacro(r"a弢", rex"a(?=弢)\w")
      check matchMacro(r"Ⓐb", rex"Ⓐ(?=b)\w")
      check matchMacro(r"aⒶ", rex"a(?=Ⓐ)\w")
      check matchMacro(r"Ⓐb", rex"Ⓐ(?=b)\w")
      check matchMacro(r"aΪ", rex"a(?=Ϊ)\w")
      check matchMacro(r"Ϊb", rex"Ϊ(?=b)\w")
      check matchMacro(r"弢Ⓐ", rex"弢(?=Ⓐ)\w")
    block:
      check matchMacro(r"a弢", rex"\w(?<=a)弢")
      check matchMacro(r"弢b", rex"\w(?<=弢)b")
      check matchMacro(r"aⒶ", rex"\w(?<=a)Ⓐ")
      check matchMacro(r"Ⓐb", rex"\w(?<=Ⓐ)b")
      check matchMacro(r"aΪ", rex"\w(?<=a)Ϊ")
      check matchMacro(r"Ϊb", rex"\w(?<=Ϊ)b")
      check matchMacro(r"弢Ⓐ", rex"\w(?<=弢)Ⓐ")
    block:
      check matchMacroCapt("abcdefg", rex"\w+(?<=(ab)cd(?<=(cd)))\w+") ==
        @["ab", "cd"]
      check matchMacroCapt("abcdefg", rex"\w+(?<=(ab)(?=(cd)))\w+") ==
        @["ab", "cd"]
      check matchMacroCapt("abcdefg", rex"\w+(?<=(ab)(?=(cd)(?<=(cd))))\w+") ==
        @["ab", "cd", "cd"]
      check matchMacroCapt("abcdefg", rex"\w+(?=(cd)(?<=(cd)))\w+") ==
        @["cd", "cd"]

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
  check "aa".matchWithCapt(re"(a*)*?") == @[@["aa"]]
  check "aa".matchWithCapt(re"(a*)*?(a*)*?") ==
    @[newSeq[string](), @["aa"]]
  check "".matchWithCapt(re"(a*)*?(a*)*?") ==
    @[newSeq[string](), newSeq[string]()]
  check "aa".matchWithCapt(re"(.*?)") == @[@["aa"]]
  check "aa".matchWithCapt(re"(.*)*") == @[@["aa", ""]]
  check "a".matchWithCapt(re"(a*)*") == @[@["a", ""]]
  check "a".matchWithCapt(re"(a*)*(a*)*") == @[@["a", ""], @[""]]
  check "".matchWithCapt(re"(a*)*") == @[@[""]]
  check "".matchWithCapt(re"(a*)*(a*)*") == @[@[""], @[""]]
  check "a".matchWithCapt(re"(a*)*?") == @[@["a"]]
  check "a".matchWithCapt(re"(a?)*?") == @[@["a"]]
  check "a".matchWithCapt(re"(a*?)*") == @[@["a", ""]]
  check "a".matchWithCapt(re"(a*?)*?") == @[@["a"]]
  check "a".matchWithCapt(re"(a??)*") == @[@["a", ""]]
  check "ab".matchWithCapt(re"(a??)*b") == @[@["a", ""]]
  check "".matchWithCapt(re"(a??)*") == @[@[""]]
  check "a".matchWithCapt(re"(a?)??") == @[@["a"]]
  check "a".matchWithCapt(re"(a*)??") == @[@["a"]]
  check "a".matchWithCapt(re"(a*)+?") == @[@["a"]]
  check "".matchWithCapt(re"(a?)+") == @[@["", ""]]
  check "".matchWithCapt(re"(a?)(a?)*") == @[@[""], @[""]]
  check "a".matchWithCapt(re"(a?)+") == @[@["a", ""]]
  check "".matchWithCapt(re"(a*)+") == @[@["", ""]]
  check "".matchWithCapt(re"(a*)(a*)*") == @[@[""], @[""]]
  check "a".matchWithCapt(re"(a*)+") == @[@["a", ""]]
  check "b".matchWithCapt(re"(a*)+b") == @[@["", ""]]
  check "b".matchWithCapt(re"(a*)+b*") == @[@["", ""]]
  check "ab".matchWithCapt(re"(a*)+b") == @[@["a", ""]]
  check "".matchWithCapt(re"(a?)*") == @[@[""]]
  check "a".matchWithCapt(re"(a?)*") == @[@["a", ""]]
  check "a".matchWithCapt(re"(a?)*(a?)*") == @[@["a", ""], @[""]]
  check "ab".matchWithCapt(re"(a?)*b") == @[@["a", ""]]
  check "".matchWithCapt(re"(a+)*") == @[newSeq[string]()]
  check "a".matchWithCapt(re"(?:a*)*") == newSeq[seq[string]]()
  check "a".matchWithCapt(re"(a?b?)*") == @[@["a", ""]]
  check "".matchWithCapt(re"(a?b?)*") == @[@[""]]
  # same as nre, but python returns ["", ""]
  check findAllBounds("a", re"(a*)+") == @[0 .. 0, 1 .. 0]
  check findAllBounds("a", re"(a*)(a*)*") == @[0 .. 0, 1 .. 0]
  check findAllBounds("", re"(a*)+") == @[0 .. -1]
  # same as Python finditer
  check findAllCapt("a", re"(a*)+") == @[
    @[@[0 .. 0, 1 .. 0]],
    @[@[1 .. 0, 1 .. 0]]]
  check findAllCapt("", re"(a*)+") == @[@[@[0 .. -1, 0 .. -1]]]
  # same as nre, but python returns ["", ""]
  check findAllBounds("a", re"(a*)*") == @[0 .. 0, 1 .. 0]
  check findAllBounds("", re"(a*)*") == @[0 .. -1]
  check findAllCapt("a", re"(a*)*") == @[
    @[@[0 .. 0, 1 .. 0]],
    @[@[1 .. 0]]]
  check findAllCapt("", re"(a*)*") == @[@[@[0 .. -1]]]
  # litopt
  check findAllBounds("@", re"@(a*)*") == @[0 .. 0]
  check findAllBounds("@", re"@(a*)*?") == @[0 .. 0]
  check findAllBounds("@", re"@(a*?)*") == @[0 .. 0]
  check findAllBounds("@", re"@(a*?)*?") == @[0 .. 0]
  check findAllBounds("@", re"@(a*)+") == @[0 .. 0]
  check findAllBounds("@", re"@(a*)+?") == @[0 .. 0]
  check findAllBounds("@", re"@(a*?)+") == @[0 .. 0]
  check findAllBounds("@", re"@(a*?)+?") == @[0 .. 0]
  check findAllBounds("@", re"(a*)*@") == @[0 .. 0]
  check findAllBounds("@", re"(a*)*?@") == @[0 .. 0]
  check findAllBounds("@", re"(a*?)*@") == @[0 .. 0]
  check findAllBounds("@", re"(a*?)*?@") == @[0 .. 0]
  check findAllBounds("@", re"(a*)+@") == @[0 .. 0]
  check findAllBounds("@", re"(a*)+?@") == @[0 .. 0]
  check findAllBounds("@", re"(a*?)+@") == @[0 .. 0]
  check findAllBounds("@", re"(a*?)+?@") == @[0 .. 0]

test "talternations":
  check raises(r"a|?")
  check raises(r"a|?b")
  check raises(r"?|?")
  check raises(r"a|*")
  check raises(r"a|*b")
  check raises(r"a|+")
  check raises(r"a|+b")

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
    @[@["aaa", "bbb", "aaa", ""]]
  check "abab".matchWithCapt(re"(a(b))*") ==
    @[@["ab", "ab"], @["b", "b"]]
  check "aaanasdnasd".matchWithCapt(re"((a)*n?(asd)*)*") ==
    @[@["aaanasd", "nasd", ""], @["a", "a", "a"], @["asd", "asd"]]
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
  check "aaa".matchWithCapt(re"(a){0,}") ==
    @[@["a", "a", "a"]]
  check "aaa".matchWithCapt(re"(a{0,}){0,}") == @[@["aaa", ""]]
  check "aaaaa".matchWithCapt(re"(a){5}") ==
    @[@["a", "a", "a", "a", "a"]]
  check "a".matchWithCapt(re"(a){1,5}") == @[@["a"]]
  check "aaa".matchWithCapt(re"(a){1,5}") ==
    @[@["a", "a", "a"]]
  check "".matchWithCapt(re"(a){0,}") ==
    @[newSeq[string]()]
  check "aaa".matchWithCapt(re"(a{0,}){0,}") == @[@["aaa", ""]]
  check "aaa".matchWithCapt(re"(a{1}){0,}") ==
    @[@["a", "a", "a"]]
  check "aaaa".matchWithCapt(re"(a{2}){0,}") ==
    @[@["aa", "aa"]]
  check "aaaa".matchWithCapt(re"(a{0,3}){0,}") ==
    @[@["aaa", "a", ""]]
  check "".matchWithCapt(re"(a{0,3}){0,}") ==
    @[@[""]]
  check "aaa".matchWithCapt(re"(a{1,}){0,}") ==
    @[@["aaa"]]
  check "".matchWithCapt(re"(a{1,}){0,}") ==
    @[newSeq[string]()]
  check(not "".isMatch(re"(a{1,})"))
  check "a".matchWithCapt(re"(a{1,})") == @[@["a"]]
  check "aaa".matchWithCapt(re"(a{1,})") == @[@["aaa"]]
  check "abab".matchWithCapt(re"(a(b)){2}") ==
    @[@["ab", "ab"], @["b", "b"]]
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
  check "aaa".matchWithCapt(re"(a){0,}(a){0,}(a){0,}") ==
    @[@["a", "a", "a"], @[], @[]]
  check "aaa".matchWithCapt(re"(a){0,}?(a){0,}(a){0,}?") ==
    @[@[], @["a", "a", "a"], @[]]
  check "aaa".matchWithCapt(re"(a){0,}?(a){0,}?(a){0,}") ==
    @[@[], @[], @["a", "a", "a"]]
  check "aaa".matchWithCapt(re"(a){0,}?(a){0,}?(a){0,}?") ==
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
  check split("AAA :   : BBB :   : CCC", re"\s*:\s*") ==
    @["AAA", "", "BBB", "", "CCC"]
  check split("", re",") == @[""]
  check split(",,", re",") == @["", "", ""]
  # nre's behaviour, differs from python
  check split("abc", re"") == @["a", "b", "c"]
  check split("ab", re"") == @["a", "b"]
  check split("ab", re"\b") == @["ab"]
  check split("a b", re" ") == @["a", "b"]
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
  check split("bar", re"foo") == @["bar"]

  check "12".split(re"\w\b") == @["1", ""]
  check "12".split(re"\w\B") == @["", "2"]

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
  check splitIncl("ab", re"") == @["a", "b"]
  check splitIncl("ab", re"\b") == @["ab"]
  check splitIncl("a b", re" ") == @["a", "b"]

test "tfindall":
  check findAllBounds("abcabc abc", re"abc abc|abc") == @[0 .. 2, 3 .. 9]
  check findAllBounds("abcabc", re"bc") == @[1 .. 2, 4 .. 5]
  check findAllBounds("aa", re"a") == @[0 .. 0, 1 .. 1]
  check findAllBounds("a", re"a") == @[0 .. 0]
  check findAllBounds("a", re"b").len == 0
  check findAllBounds("", re"b").len == 0
  check findAllBounds("abc ab", re"\w+ *") == @[0 .. 3, 4 .. 5]
  check findAllBounds("AAA :   : BBB", re"\s*:\s*") == @[3 .. 7, 8 .. 9]
  check findAllBounds("a\n  b\n c\n  ", re"(?m)^") ==
    @[0 .. -1, 2 .. 1, 6 .. 5, 9 .. 8]
  check findAllBounds("a\n  b\n c\n  ", re"(?m)$") ==
    @[1 .. 0, 5 .. 4, 8 .. 7, 11 .. 10]
  check findAllBounds("\n\n", re"(?m)^") == @[0 .. -1, 1 .. 0, 2 .. 1]
  check findAllBounds("foobarbaz", re"(?<=o)b") == @[3 .. 3]
  check findAllBounds("foobarbaz", re"(?<!o)b") == @[6 .. 6]
  check findAllBounds("aaaabaaaa", re"(?<!a)a") == @[0 .. 0, 5 .. 5]
  check findAllBounds("foobar", re"o(?=b)") == @[2 .. 2]
  check findAllBounds("foobar", re"o(?!b)") == @[1 .. 1]
  check findAllBounds("aaaabaaaa", re"a(?!a)") == @[3 .. 3, 8 .. 8]
  check findAllBounds("aaa", re"\w+b|\w") == @[0 .. 0, 1 .. 1, 2 .. 2]
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
  check findAllBounds("abb", re"a*") == @[0 .. 0, 1 .. 0, 2 .. 1, 3 .. 2]
  check findAllBounds("abbbbccccdXabbbbccccdX", re"a(b|c)*d") ==
    @[0 .. 9, 11 .. 20]
  check findAllBounds("abbbXabbbX", re"((a)*(b)*)") ==
    @[0 .. 3, 4 .. 3, 5 .. 8, 9 .. 8, 10 .. 9]
  check findAllBounds("abbbXabbbX", re"((a)+(b)+)") ==
    @[0 .. 3, 5 .. 8]
  check findAllBounds("abbbXabbbX", re"((a(b)*)+(b)*)") ==
    @[0 .. 3, 5 .. 8]
  check findAllBounds("abXabX", re"a(b|c)*d").len == 0
  check findAllBounds("aaanasdnasdXaaanasdnasd", re"((a)*n?(asd)*)+") ==
    @[0 .. 10, 11 .. 10, 12 .. 22, 23 .. 22]
  check findAllBounds("1xxx", re"\d\w+x") == @[0 .. 3]
  check findAllBounds("xxxx", re"\d\w+x").len == 0

test "tfindandcaptureall":
  check findAndCaptureAll("abcabc", re"bc") == @["bc", "bc"]
  check findAndCaptureAll("a1b2c3a4b5c6", re"\d") ==
    @["1", "2", "3", "4", "5", "6"]
  check findAndCaptureAll("abbbbccccdXabbbbccccdX", re"a(b|c)*d") ==
    @["abbbbccccd", "abbbbccccd"]
  check findAndCaptureAll("abbbXabbbX", re"((a)*(b)*)") ==
    @["abbb", "", "abbb", "", ""]
  check findAndCaptureAll("abbbXabbbX", re"((a)+(b)+)") ==
    @["abbb", "abbb"]
  check findAndCaptureAll("abbbXabbbX", re"((a(b)*)+(b)*)") ==
    @["abbb", "abbb"]
  check findAndCaptureAll("abbbXbbXabbb", re"((a(b)*)*(b)*)") ==
    @["abbb", "", "bb", "", "abbb", ""]

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
  check startsWith("abc", re"b", start = 1)
  check startsWith("abc", re"(?<=a)b", start = 1)
  check(not startsWith("abc", re"(?<=x)b", start = 1))
  check(not startsWith("abc", re"^b", start = 1))

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
  check "ab".isMatch(re"\w(?<=a)b")
  check(not "ab".isMatch(re"\w(?<=a)"))
  check(not "ab".isMatch(re"\w(?<=c)b"))
  check "ab".matchWithCapt(re"(a(?<=a))b") == @[@["a"]]

test "tnegative_look_around":
  check "ab".isMatch(re"a(?!c)\w")
  check(not "ab".isMatch(re"a(?!c)"))
  check(not "ab".isMatch(re"a(?!b)\w"))
  check "ab".matchWithCapt(re"(a(?!c))\w") == @[@["a"]]
  check "ab".isMatch(re"\w(?<!c)b")
  check(not "ab".isMatch(re"\w(?<!c)"))
  check(not "ab".isMatch(re"\w(?<!a)b"))
  check "ab".matchWithCapt(re"(\w(?<!c))b") == @[@["a"]]

test "tfull_lookarounds":
  var m: RegexMatch
  check match("ab", re"a(?=b)\w")
  check(not match("ab", re"a(?=x)\w"))
  check(not match("ab", re"ab(?=x)"))
  check match("ab", re"ab(?=$)")
  check match("abc", re"a(?=b)\w+")
  check match("abc", re"a(?=(bc))\w+", m) and
    m.captures == @[@[1 .. 2]]
  check match("ab", re"\w(?<=a)b")
  check(not match("ab", re"\w(?<=x)b"))
  check(not match("ab", re"(?<=x)ab"))
  check match("ab", re"(?<=^)ab")
  check match("abc", re"\w\w(?<=(b))c")
  check match("abc", re"\w\w(?<=(b))c", m) and
    m.captures == @[@[1 .. 1]]
  check match("abc", re"\w\w(?<=(ab))c")
  check match("abc", re"\w\w(?<=(ab))c", m) and
    m.captures == @[@[0 .. 1]]
  check match("a", re"\w(?<=a)")
  check(not match("a", re"\w(?=a)"))
  check match("ab", re"\w(?<=a(?<=a))b")
  check match("ab", re"\w(?<=a(?<=a(?<=a)))b")
  check match("ab", re"\w(?<=a(?=b))b")
  check(not match("ab", re"\w(?=b(?=b))b"))  # JS, D
  check(not match("ab", re"\w(?<=a(?=b(?=b)))b"))  # JS, D
  check match("ab", re"\w(?<=a(?<=a)(?=b))b")
  check match("ab", re"\w(?<=a(?<=a(?=b)))b")
  check(not match("ab", re"\w(?<=(?<=a)a)b"))  # JS, D
  check match("aab", re"\w\w(?<=aa(?=b))b")
  block:
    check match("aaab", re"(\w*)(\w*?)", m) and
      m.captures == @[@[0 .. 3], @[4 .. 3]]
    check match("aaab", re".*(?<=^(\w*)(\w*?)$)", m) and
      m.captures == @[@[0 .. 3], @[4 .. 3]]
    check match("aaab", re".*(?<=^(\w*)(\w*)$)", m) and
      m.captures == @[@[0 .. -1], @[0 .. 3]]
    check match("aaab", re"(\w*?)(\w*)(\w*?)", m) and
      m.captures == @[@[0 .. -1], @[0 .. 3], @[4 .. 3]]
    check match("aaab", re".*(?<=^(\w*?)(\w*)(\w*?)$)", m) and
      m.captures == @[@[0 .. -1], @[0 .. 3], @[4 .. 3]]
    check match("aaab", re"(\w*)(\w??)", m) and
      m.captures == @[@[0 .. 3], @[4 .. 3]]
    check match("aaab", re".*(?<=^(\w*)(\w??)$)", m) and
      m.captures == @[@[0 .. 3], @[4 .. 3]]
    check match("aaab", re".*(?<=^(\w*)(\w?)$)", m) and
      m.captures == @[@[0 .. 2], @[3 .. 3]]
    check match("aaab", re".*(?<=^(\w*?)(\w?)(\w*?)$)", m) and
      m.captures == @[@[0 .. 2], @[3 .. 3], @[4 .. 3]]
    check match("aaab", re".*(?<=^(\w*?)(\w??)(\w*?)$)", m) and
      m.captures == @[@[0 .. 3], @[4 .. 3], @[4 .. 3]]
    check match("aaab", re".*(?<=^(\w??)(\w*?)$)", m) and
      m.captures == @[@[0 .. 0], @[1 .. 3]]
    check match("aaab", re".*(?<=^(\w?)(\w*?)$)", m) and
      m.captures == @[@[0 .. 0], @[1 .. 3]]
    check match("aaab", re".*(?<=^(\w*?)(\w??)$)", m) and
      m.captures == @[@[0 .. 3], @[4 .. 3]]
    check match("aaab", re".*(?<=^(\w*)(\w??)$)", m) and
      m.captures == @[@[0 .. 3], @[4 .. 3]]
    check match("aaab", re".*(?<=^(\w*?)(\w?)$)", m) and
      m.captures == @[@[0 .. 2], @[3 .. 3]]
    check match("aaab", re".*(?<=^(\w*)(\w?)$)", m) and
      m.captures == @[@[0 .. 2], @[3 .. 3]]
    check match("aaab", re".*(?<=^(\w*?)(\w*?)(\w??)$)", m) and
      m.captures == @[@[0 .. 3], @[4 .. 3], @[4 .. 3]]
    check match("aaab", re".*(?<=^(\w*?)(\w*)(\w??)$)", m) and
      m.captures == @[@[0 .. -1], @[0 .. 3], @[4 .. 3]]
  block:
    check match("aaab", re".*(?<=^(\w)\w+|(\w)\w+$)", m) and
      m.captures == @[@[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(\d)\w+|(\w)\w+$)", m) and
      m.captures == @[@[], @[0 .. 0]]
    check match("aaab", re".*(?<=^(\w)\w+|(\w)\w+|(\w)\w+$)", m) and
      m.captures == @[@[0 .. 0], @[], @[]]
    check match("aaab", re".*(?<=^(\d)\w+|(\w)\w+|(\w)\w+$)", m) and
      m.captures == @[@[], @[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(\d)\w+|(\d)\w+|(\w)\w+$)", m) and
      m.captures == @[@[], @[], @[0 .. 0]]
  block:
    check match("aaab", re".*(?<=^(?:(\w)\w+|(\w)\w+)+?$)", m) and
      m.captures == @[@[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(?:(\d)\w+|(\w)\w+)+?$)", m) and
      m.captures == @[@[], @[0 .. 0]]
    check match("aaab", re".*(?<=^(?:(\w)\w+|(\w)\w+|(\w)\w+)+?$)", m) and
      m.captures == @[@[0 .. 0], @[], @[]]
    check match("aaab", re".*(?<=^(?:(\d)\w+|(\w)\w+|(\w)\w+)+?$)", m) and
      m.captures == @[@[], @[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(?:(\d)\w+|(\d)\w+|(\w)\w+)+?$)", m) and
      m.captures == @[@[], @[], @[0 .. 0]]
    check match("aaab", re".*(?<=^(?:(\w)\w+|(\w)\w+)*?$)", m) and
      m.captures == @[@[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(?:(\d)\w+|(\w)\w+)*?$)", m) and
      m.captures == @[@[], @[0 .. 0]]
    check match("aaab", re".*(?<=^(?:(\w)\w+|(\w)\w+|(\w)\w+)*?$)", m) and
      m.captures == @[@[0 .. 0], @[], @[]]
    check match("aaab", re".*(?<=^(?:(\d)\w+|(\w)\w+|(\w)\w+)*?$)", m) and
      m.captures == @[@[], @[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(?:(\d)\w+|(\d)\w+|(\w)\w+)*?$)", m) and
      m.captures == @[@[], @[], @[0 .. 0]]
    check match("aaab", re".*(?<=^(?:(\w)\w+|(\w)\w+)??$)", m) and
      m.captures == @[@[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(?:(\d)\w+|(\w)\w+)??$)", m) and
      m.captures == @[@[], @[0 .. 0]]
    check match("aaab", re".*(?<=^(?:(\w)\w+|(\w)\w+|(\w)\w+)??$)", m) and
      m.captures == @[@[0 .. 0], @[], @[]]
    check match("aaab", re".*(?<=^(?:(\d)\w+|(\w)\w+|(\w)\w+)??$)", m) and
      m.captures == @[@[], @[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(?:(\d)\w+|(\d)\w+|(\w)\w+)??$)", m) and
      m.captures == @[@[], @[], @[0 .. 0]]
  block:
    check match("aaab", re".*(?<=^(\w)\w+?|(\w)\w+?$)", m) and
      m.captures == @[@[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(\w)\w+|(\w)\w+$)", m) and
      m.captures == @[@[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(\w)\w+?|(\w)\w+$)", m) and
      m.captures == @[@[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(\w)\w+|(\w)\w+?$)", m) and
      m.captures == @[@[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(\w+|\w{4}|\w{4}|\w+)*$)", m) and
      m.captures == @[@[0 .. 3]]
    check match("aaab", re"(\w*|\w{4}|\w{4}|\w*)*", m) and
      m.captures == @[@[0 .. 3, 4 .. 3]]
    check match("aaab", re".*(?<=^(\w*|\w{4}|\w{4}|\w*)*$)", m) and
      m.captures == @[@[0 .. -1, 0 .. 3]]
    check match("aaab", re".*(?<=^(\w+|\w{4}|\w{4}|\w+)+$)", m) and
      m.captures == @[@[0 .. 3]]
    check match("aaab", re".*(?<=^(\w)\w+|\w{4}|\w{4}|(\w)\w+$)", m) and
      m.captures == @[@[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(\d)\w+|(\w{4})|(\w{4})|(\w)\w+$)", m) and
      m.captures == @[@[], @[0 .. 3], @[], @[]]
    check match("aaab", re".*(?<=^(\d)\w+|(\w)\w{3}|(\w)\w{3}|(\w)\w+$)", m) and
      m.captures == @[@[], @[0 .. 0], @[], @[]]
    check match("aaab", re".*(?<=^(\d)\w+|(\d)\w{3}|(\w)\w{3}|(\w)\w+$)", m) and
      m.captures == @[@[], @[], @[0 .. 0], @[]]
    check match("aaab", re".*(?<=^(\d)\w+|(\d)\w{3}|(\d)\w{3}|(\w)\w+$)", m) and
      m.captures == @[@[], @[], @[], @[0 .. 0]]
    check match("aaab", re"(\w*|\w{4}|\w{4}|\w*)+", m) and
      m.captures == @[@[0 .. 3, 4 .. 3]]
    check match("aaab", re".*(?<=^(\w*|\w{4}|\w{4}|\w*)+$)", m) and
      m.captures == @[@[0 .. -1, 0 .. 3]]
    check match("aaab", re".*(?<=^(\w+)|(\w{4})|(\w{10})$)", m) and
      m.captures == @[@[0 .. 3], @[], @[]]
    check match("aaab", re".*(?<=^(\w{10})|(\w{4})|(\w+)$)", m) and
      m.captures == @[@[], @[0 .. 3], @[]]
    check match("aaab", re".*(?<=^((\w{10})|(\w{4})|(\w+))+$)", m) and
      m.captures == @[@[0 .. 3], @[], @[0 .. 3], @[]]
    check match("aaab", re".*(?<=^((\w{10})|(\w{4})|(\w+))*$)", m) and
      m.captures == @[@[0 .. 3], @[], @[0 .. 3], @[]]
    check match("aaab", re".*(?<=^(\w+|\w{4}|\w{4}|\w+)*?$)", m) and
      m.captures == @[@[0 .. 3]]
    check match("aaab", re".*(?<=^(\w*|\w{4}|\w{4}|\w*)*?$)", m) and
      m.captures == @[@[0 .. 3]]
    check match("aaab", re".*(?<=^(\w+|\w{4}|\w{4}|\w+)+?$)", m) and
      m.captures == @[@[0 .. 3]]
  check findAllBounds(r"1abab", re"(?<=\d\w*)ab") ==
    @[1 .. 2, 3 .. 4]
  check findAllBounds(r"abab", re"(?<=\d\w*)ab").len == 0
  check findAllBounds(r"abab1", re"ab(?=\w*\d)") ==
    @[0 .. 1, 2 .. 3]
  check findAllBounds(r"abab", re"ab(?=\w*\d)").len == 0
  check findAllBounds("foo\nbar\nbar", re"bar(?=$)") ==
    @[8 .. 10]
  check findAllBounds("foo\nbar\nbar", re"(?m)bar(?=$)") ==
    @[4 .. 6, 8 .. 10]
  check findAllBounds("bar\nfoo\nbar", re"(?<=^)bar") ==
    @[0 .. 2]
  check findAllBounds("bar\nfoo\nbar", re"(?m)(?<=^)bar") ==
    @[0 .. 2, 8 .. 10]
  block:
    # There is a difference in how nesting is
    # handled by JS vs D; in D all of them start
    # from the outermost one, while in JS they
    # start from the outer one/parent. I think the JS way
    # is less mind blending, and it makes more sense to me
    #
    # These 2 match in D, but not in JS
    check(not match("ab", re"\w(?<=a(?=b(?<=a)))b"))  # JS, !D
    check(not match("ab", re"\w(?<=a(?<=a(?=b(?<=a))))b"))  # JS, !D
    # These 2 match in JS, but not in D
    check match("ab", re"\w(?<=a(?=b(?<=b)))b")  # JS, !D
    check match("ab", re"\w(?<=a(?<=a(?=b(?<=b))))b")  # JS, !D
  block:
    let asterisk = re"(?<=(?<!\\)(?:\\\\)*)\*"
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
    let asterisk = re".*(?<=(?<!\\)(?:\\\\)*)\*"
    check match(r"*", asterisk)
    check(not match(r"\*", asterisk))
    check match(r"\\*", asterisk)
    check(not match(r"\\\*", asterisk))
    check match(r"\\\\*", asterisk)
    check(not match(r"\\\\\*", asterisk))
  block:
    check findAllBounds("foobarbaz", re"(?<=o)b") == @[3 .. 3]
    check findAllBounds("foobar", re"o(?=b)") == @[2 .. 2]
    check findAllBounds("100 in USD100", re"(?<=USD)\d{3}") ==
      @[10 .. 12]
    check findAllBounds("100 in USD100", re"\d{3}(?<=USD\d{3})") ==
      @[10 .. 12]
  block:
    check match("弢b", re"弢(?=b)\w")
    check match("a弢", re"a(?=弢)\w")
    check match("Ⓐb", re"Ⓐ(?=b)\w")
    check match("aⒶ", re"a(?=Ⓐ)\w")
    check match("Ⓐb", re"Ⓐ(?=b)\w")
    check match("aΪ", re"a(?=Ϊ)\w")
    check match("Ϊb", re"Ϊ(?=b)\w")
    check match("弢Ⓐ", re"弢(?=Ⓐ)\w")
  block:
    check match("a弢", re"\w(?<=a)弢")
    check match("弢b", re"\w(?<=弢)b")
    check match("aⒶ", re"\w(?<=a)Ⓐ")
    check match("Ⓐb", re"\w(?<=Ⓐ)b")
    check match("aΪ", re"\w(?<=a)Ϊ")
    check match("Ϊb", re"\w(?<=Ϊ)b")
    check match("弢Ⓐ", re"\w(?<=弢)Ⓐ")
  block:  # Follows Nim re's behaviour
    check(not match("abc", re"^bc", m, start = 1))
    check match("abc", re"(?<=a)bc", m, start = 1)
    check(not match("abc", re"(?<=x)bc", m, start = 1))
  block:
    check match("abcdefg", re"\w+(?<=(ab)cd(?<=(cd)))\w+", m) and
      m.captures == @[@[0 .. 1], @[2 .. 3]]
    check match("abcdefg", re"\w+(?<=(ab)(?=(cd)))\w+", m) and
      m.captures == @[@[0 .. 1], @[2 .. 3]]
    check match("abcdefg", re"\w+(?<=(ab)(?=(cd)(?<=(cd))))\w+", m) and
      m.captures == @[@[0 .. 1], @[2 .. 3], @[2 .. 3]]
    check match("abcdefg", re"\w+(?=(cd)(?<=(cd)))\w+", m) and
      m.captures == @[@[2 .. 3], @[2 .. 3]]

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
    check text.match(re"(?P<greet>hello) (?P<who>world)", m)
    check m.groupsCount == 2
    for name in @["greet", "who"]:
      check m.groupNames.contains(name)
  block:
    let text = "hello world"
    var m: RegexMatch
    check text.match(re"(?P<greet>hello) (?P<who>world)", m)
    check m.group("greet", text) == @["hello"]
    check m.group("who", text) == @["world"]
  block:
    let text = "hello world foo bar"
    var m: RegexMatch
    check text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    check m.group("greet", text) == @["hello"]
    let whoGroups = m.group("who", text)
    for w in @["foo", "bar", "world"]:
      check whoGroups.contains(w)
  block:
    let text = "hello world"
    var m: RegexMatch
    check text.match(re"(?P<greet>hello) (?P<who>world)", m)
    check m.groupFirstCapture("greet", text) == "hello"
    check m.groupFirstCapture("who", text) == "world"
  block:
    let text = "hello world her"
    var m: RegexMatch
    check text.match(re"(?P<greet>hello) (?P<who>world) (?P<who>her)", m)
    check m.groupFirstCapture("greet", text) == "hello"
  block:
    let text = "hello world foo bar"
    var m: RegexMatch
    check text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    # "who" captures @["world", "foo", "bar"]
    check m.groupFirstCapture("who", text) == "world"
  block:
    let text = "hello"
    var m: RegexMatch
    check text.match(re"(?P<greet>hello)\s?(?P<who>world)?", m)
    check m.groupFirstCapture("greet", text) == "hello"
    check m.groupFirstCapture("who", text) == ""
  block:
    let text = "hello world her"
    var m: RegexMatch
    check text.match(re"(?P<greet>hello) (?P<who>world) (?P<who>her)", m)
    check m.groupLastCapture("who", text) == "her"
  block:
    let text = "hello world foo bar"
    var m: RegexMatch
    check text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    # "who" captures @["world", "foo", "bar"]
    check m.groupLastCapture("who", text) == "bar"
  block:
    let text = "hello"
    var m: RegexMatch
    check text.match(re"(?P<greet>hello)\s?(?P<who>world)?", m)
    check m.groupLastCapture("greet", text) == "hello"
    check m.groupLastCapture("who", text) == ""
  block:
    let text = "hello"
    var m: RegexMatch
    check text.match(re"(hello)\s?(world)?", m)
    check m.groupLastCapture(0, text) == "hello"
    check m.groupLastCapture(1, text) == ""
  block:
    let text = ""
    var m: RegexMatch
    check text.match(re"(hello)?\s?(world)?", m)
    check m.groupLastCapture(0, text) == ""
    check m.groupLastCapture(1, text) == ""
  block:
    let text = "hello world foo bar"
    var m: RegexMatch
    check text.match(re"(hello) (?:([^\s]+)\s?)+", m)
    # "who" captures @["world", "foo", "bar"]
    check m.groupLastCapture(1, text) == "bar"
  block:
    let text = "hello"
    var m: RegexMatch
    check text.match(re"(hello)\s?(world)?", m)
    check m.groupFirstCapture(0, text) == "hello"
    check m.groupFirstCapture(1, text) == ""
  block:
    let text = ""
    var m: RegexMatch
    check text.match(re"(hello)?\s?(world)?", m)
    check m.groupFirstCapture(0, text) == ""
    check m.groupFirstCapture(1, text) == ""
  block:
    let text = "hello world foo bar"
    var m: RegexMatch
    check text.match(re"(hello) (?:([^\s]+)\s?)+", m)
    # "who" captures @["world", "foo", "bar"]
    check m.groupFirstCapture(1, text) == "world"

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

test "tfindopt":
  var m: RegexMatch
  check(not find("bar", re"foo", m))
  check(not find("bar", re"baz", m))
  check "abcd".find(re"bc", m)
  check m.boundaries == 1 .. 2
  check "bcd".find(re"bc", m)
  check m.boundaries == 0 .. 1
  check "bc".find(re"bc", m)
  check m.boundaries == 0 .. 1
  check "ababcd".find(re"bc", m)
  check m.boundaries == 3 .. 4
  check "abc@xyz".find(re"\w@", m)
  check m.boundaries == 2 .. 3
  check "ab1c@xyz".find(re"\d\w@", m)
  check m.boundaries == 2 .. 4
  check "##axyz##".find(re"(a|b)xyz", m)
  check m.boundaries == 2 .. 5
  check "##bxyz##".find(re"(a|b)xyz", m)
  check m.boundaries == 2 .. 5
  check "##x#ax#axy#bxyz##".find(re"(a|b)xyz", m)
  check m.boundaries == 11 .. 14
  check "##z#xyz#yz#bxyz##".find(re"(a|b)xyz", m)
  check m.boundaries == 11 .. 14
  check "#xabcx#abc#".find(re"\babc\b", m)
  check m.boundaries == 7 .. 9
  check "#foo://#".find(re"[\w]+://", m)
  check m.boundaries == 1 .. 6
  check "x#foo://#".find(re"[\w]+://", m)
  check m.boundaries == 2 .. 7

test "tfindallopt":
  check findAllBounds("bar", re"foo").len == 0
  check findAllBounds("bar", re"baz").len == 0
  check findAllBounds("abcd", re"bc") ==
    @[1 .. 2]
  check findAllBounds("bcd", re"bc") ==
    @[0 .. 1]
  check findAllBounds("bc", re"bc") ==
    @[0 .. 1]
  check findAllBounds("ababcd", re"bc") ==
    @[3 .. 4]
  check findAllBounds("abcdbcbc", re"bc") ==
    @[1 .. 2, 4 .. 5, 6 .. 7]
  check findAllBounds("abc@xyz", re"\w@") ==
    @[2 .. 3]
  check findAllBounds("abc@xyz@@", re"\w@") ==
    @[2 .. 3, 6 .. 7]
  check findAllBounds("ab1c@xyz", re"\d\w@") ==
    @[2 .. 4]
  check findAllBounds("ab1c@xy1z@z@", re"\d\w@") ==
    @[2 .. 4, 7 .. 9]
  check findAllBounds("##axyz##", re"(a|b)xyz") ==
    @[2 .. 5]
  check findAllBounds("##bxyz##", re"(a|b)xyz") ==
    @[2 .. 5]
  check findAllBounds("##axyz##bxyz", re"(a|b)xyz") ==
    @[2 .. 5, 8 .. 11]
  check findAllBounds("##x#ax#axy#bxyz##", re"(a|b)xyz") ==
    @[11 .. 14]
  check findAllBounds("##z#xyz#yz#bxyz##", re"(a|b)xyz") ==
    @[11 .. 14]
  check findAllBounds("#xabcx#abc#", re"\babc\b") ==
    @[7 .. 9]
  check findAllBounds("#xabcx#abc#abc", re"\babc\b") ==
    @[7 .. 9, 11 .. 13]
  check findAllBounds("#foo://#", re"[\w]+://") ==
    @[1 .. 6]
  check findAllBounds("x#foo://#", re"[\w]+://") ==
    @[2 .. 7]
  check findAllBounds("foobarbaz", re"(?<=o)b") ==
    @[3 .. 3]
  check findAllBounds("foobarbaz", re"(?<=r)b") ==
    @[6 .. 6]
  check findAllBounds("foobarbaz", re"(?<!o)b") ==
    @[6 .. 6]
  check findAllBounds("foobarbaz", re"(?<!r)b") ==
    @[3 .. 3]
  check findAllBounds("x@x@y@y@y@x@xy@yx@@", re"(?<!x)@\w+") ==
    @[5 .. 6, 7 .. 8, 9 .. 10, 14 .. 16]
  check findAllBounds("foobar", re"o(?=b)") ==
    @[2 .. 2]
  check findAllBounds("foobarbaz", re"a(?=r)") ==
    @[4 .. 4]
  check findAllBounds("foobar", re"o(?!b)") ==
    @[1 .. 1]
  check findAllBounds("foobarbaz", re"a(?!r)") ==
    @[7 .. 7]
  check findAllBounds("x@x@y@y@y@x@xy@yx@@", re"\w+@(?!x)") ==
    @[2 .. 3, 4 .. 5, 6 .. 7, 12 .. 14, 15 .. 17]
  check findAllBounds("abcdef", re"^abcd") ==
    @[0 .. 3]
  check findAllBounds("abcdef", re"cdef$") ==
    @[2 .. 5]
  check findAllBounds("abcdef", re"^abcdef$") ==
    @[0 .. 5]
  check findAllBounds("abcdef", re"^abcx").len == 0
  check findAllBounds("abcdef", re"xcdef$").len == 0
  check findAllBounds("abc\nabc\na", re"(?m)^a") ==
    @[0 .. 0, 4 .. 4, 8 .. 8]
  check findAllBounds("x\nabcxx\nabcxx", re"(?m)x$\n?[^x]*") ==
    @[0 .. 4, 6 .. 10, 12 .. 12]
  check findAllBounds("#f1o2o3@bar#", re"(\w\d)*?@\w+") ==
    @[1 .. 10]
  check findAllBounds("foo@bar@baz", re"\w+@\w+") ==
    @[0 .. 6]
  check findAllBounds("foo@111@111", re"\w+@\d+") ==
    @[0 .. 6]
  check findAllBounds("foo@111@111", re"\w*@\d+") ==
    @[0 .. 6, 7 .. 10]
  check findAllBounds("foo@111@@111", re"\w+@\d+") ==
    @[0 .. 6]
  check findAllBounds("foo@111foo@111", re"\w+@\d+") ==
    @[0 .. 6, 7 .. 13]
  check findAllBounds("111@111@111", re"\d+@\w+") ==
    @[0 .. 6]
  check findAllBounds("abbbbccccd@abbbbccccd@", re"\w(b|c)*d@") ==
    @[0 .. 10, 11 .. 21]
  check findAllBounds("abXabX", re"\w(b|c)*@").len == 0
  check findAllBounds("abbcXabbcX", re"((a(b)*)+(c))") ==
    @[0 .. 3, 5 .. 8]
  check findAllBounds("aaanasdnasd@aaanasdnasd@", re"((a)*n?(asd)*)+@") ==
    @[0 .. 11, 12 .. 23]
  check findAllBounds("a1@a1a1@", re"(\w\d)+@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@a@a", re"(\w\d)+@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@a@a1a@1@a@a1@", re"(\w\d)+@") ==
    @[0 .. 2, 3 .. 7, 18 .. 20]
  check findAllBounds("a1@a1a1@", re"(\w\d)*@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@a@a", re"(\w\d)*@") ==
    @[0 .. 2, 3 .. 7, 9 .. 9]
  check findAllBounds("a1@a1a1@", re"(\w\d)+?@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@", re"(\w\d)*?@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@", re"\w+\d+?@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@", re"\w+?\d+@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("a1@a1a1@", re"\w+?\d+?@") ==
    @[0 .. 2, 3 .. 7]
  check findAllBounds("ab@&%", re"(a|ab)\w@&%") ==
    @[0 .. 4]
  check findAllBounds("abc@&%", re"(a|ab)\w@&%") ==
    @[0 .. 5]
  check findAllBounds("ab@&%abc@&%", re"(a|ab)\w@&%") ==
    @[0 .. 4, 5 .. 10]
  check findAllCapt("ab@&%", re"(?:(a)|(ab))\w@&%") ==
    @[@[@[0 .. 0], @[]]]
  check findAllCapt("a#b@&%", re"(?:(a)|(a#))\w@&%") ==
    @[@[@[], @[0 .. 1]]]
  check findAllCapt("abc@&%", re"(?:(a)|(ab))\w@&%") ==
    @[@[@[], @[0 .. 1]]]
  check findAllCapt("aba@&%", re"(?:(ab)|(a))\w@&%") ==
    @[@[@[0 .. 1], @[]]]
  check findAllCapt("ab@&%abc@&%", re"(?:(a)|(ab))\w@&%") ==
    @[@[@[0 .. 0], @[]], @[@[], @[5 .. 6]]]
  check findAllCapt("abc@&%ab@&%", re"(?:(a)|(ab))\w@&%") ==
    @[@[@[], @[0 .. 1]], @[@[6 .. 6], @[]]]
  check findAllBounds("x@xxzx@xxz", re"\w+@\w+(?=z)") == @[0 .. 3, 4 .. 8]
  check findAllBounds("1x@11zx@xxz", re"\d+\w+@\w+(?=z)") == @[0 .. 4]
  check findAllBounds("1x@11zx@xxz", re"\d+\w+@\w+") == @[0 .. 6]
  check findAllBounds("1x@1x@1x", re"\d+\w+@\w+") == @[0 .. 4]
  check findAllBounds("1x@1x@1x", re"\d+\w+@(\d\w)+") == @[0 .. 4]
  check findAllBounds("2222", re"22") == @[0 .. 1, 2 .. 3]
  block overlapTests:
    check findAllBounds("1x@1xx@1x", re"\d+\w+@(1\w)+") == @[0 .. 4]
    check findAllBounds("1x@1x1xx@1x", re"\d+\w+@(1\w)+") == @[0 .. 6]
    check findAllBounds("1x@1xx1x@1x", re"\d+\w+@(1\w)+") == @[0 .. 4, 6 .. 10]
    check findAllBounds("1x@1xx@1x", re"\d\w+@(\d\w)+") == @[0 .. 4]
    check findAllBounds("2x1x@xx", re"(1\w)+@\w+") == @[2 .. 6]
    check findAllBounds("2x1x1x@xx", re"(1\w)+@\w+") == @[2 .. 8]
  check findAllBounds("bcbc#bc", re"\bbc\B") == @[0 .. 1]
  check findAllBounds("bcbc#bca", re"\bbc\B") == @[0 .. 1, 5 .. 6]
  check findAllBounds("bcbc#bc#xbcx#bcbcbc#bc", re"\bbc\B") ==
    @[0 .. 1, 13 .. 14]
  check findAllBounds("bcbc#bc#xbcx#bcbcbc#bc", re"\Bbc\b") ==
    @[2 .. 3, 17 .. 18]
  check findAllBounds("bcbc", re"\Bbc") == @[2 .. 3]
  check findAllBounds("bcbcbc", re"\Bbc") == @[2 .. 3, 4 .. 5]
  check findAllBounds("bc#bc#xbcx", re"\Bbc\B") == @[7 .. 8]
  check findAllBounds("bcbc#bca", re"\Bbc\b") == @[2 .. 3]
  check findAllBounds("bcbc#bc", re"\Bbc\b") == @[2 .. 3]
  check findAllBounds("abcabc", re"bc") == @[1 .. 2, 4 .. 5]
  check findAllBounds("bcbc#bc", re"\bbc\b") == @[5 .. 6]
  check findAllBounds("bcbc", re"\bbc") == @[0 .. 1]
  check findAllBounds("bc#bc", re"\bbc\b") == @[0 .. 1, 3 .. 4]
  check findAllBounds("bcabc", re"^bc") == @[0 .. 1]
  check findAllBounds("bcbc", re"^bc") == @[0 .. 1]
  check findAllBounds("bcabc\nbc\nabc\nbcbc", re"(?m)^bc") ==
    @[0 .. 1, 6 .. 7, 13 .. 14]
  check findAllBounds("弢@弢@弢", re"\w+@\w+") == @[0 .. 8]
  check findAllBounds("弢ⒶΪ@弢ⒶΪ@弢ⒶΪ", re"\w+@\w+") == @[0 .. 18]
  check findAllBounds("۲@弢۲⅕@弢", re"\d+@\w+") == @[0 .. 8]
  check findAllBounds("۲۲@弢ⒶΪ11@弢ⒶΪ", re"\d+@弢ⒶΪ") ==
    @[0 .. 13, 14 .. 25]
  check findAllBounds("#xa弢ⒶΪx#a弢ⒶΪ#", re"\ba弢ⒶΪ\b") == @[14 .. 23]
  check findAllBounds("#xa弢ⒶΪx#a弢ⒶΪ#a弢ⒶΪ", re"\ba弢ⒶΪ\b") ==
    @[14 .. 23, 25 .. 34]
  check findAllBounds("۲Ⓐ@۲弢Ϊ@۲弢", re"\d+\w+@(۲\w)+") == @[0 .. 11]
  check findAllBounds("۲弢@۲弢۲ΪΪ@۲弢", re"\d+\w+@(۲\w)+") == @[0 .. 16]
  check findAllBounds("۲Ϊ@۲弢Ⓐ۲Ⓐ@۲弢", re"\d+\w+@(۲\w)+") ==
    @[0 .. 10, 14 .. 25]

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
    m.captures == @[@[0 .. 2, 3 .. 5, 6 .. 8, 9 .. 8]]
  check match("abab", re"(a(b))*", m) and
    m.captures == @[@[0 .. 1, 2 .. 3], @[1 .. 1, 3 .. 3]]
  check match("aaanasdnasd", re"((a)*n?(asd)*)*", m) and
    m.captures == @[
      @[0 .. 6, 7 .. 10, 11 .. 10],
      @[0 .. 0, 1 .. 1, 2 .. 2],
      @[4 .. 6, 8 .. 10]]
  check match("aaanasdnasd", re"((a)*n?(asd))*", m) and
    m.captures == @[@[0 .. 6, 7 .. 10], @[0 .. 0, 1 .. 1, 2 .. 2], @[4 .. 6, 8 .. 10]]

test "tmisc2_5":
  var m: RegexMatch
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
  # VM registry error on Nim < 1.1 (devel)
  when not defined(runTestAtCT) or (NimMajor, NimMinor) > (1, 0):
    block:
      const re1 = re"((11)*)+(111)*"
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

test "tmisc2_6":
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

test "tmisc3":
  var m: RegexMatch
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
  check(not find("foobarbar", re"^bar", m, start=3))
  check find("foobar\nbar", re"(?m)^bar", m, start=3) and
    m.boundaries == 7 .. 9
  check find("foo\nbar\nbar", re"(?m)^bar", m, start=3) and
    m.boundaries == 4 .. 6
  check find("foo\nbar\nbar", re"(?m)^bar", m, start=4) and
    m.boundaries == 4 .. 6
  block:
    # The bounds must contain the empty match index
    check find("foo\nbar\nbar", re"(?m)^", m) and
      m.boundaries == 0 .. -1
    check find("foo\nbar\nbar", re"(?m)^", m, start=1) and
      m.boundaries == 4 .. 3
    check find("foo\nbar\nbar", re"(?m)^", m, start=4) and
      m.boundaries == 4 .. 3
    check find("foo\nbar\nbar", re"(?m)^", m, start=5) and
      m.boundaries == 8 .. 7
    check find("foo\nbar\nbar", re"(?m)^", m, start=8) and
      m.boundaries == 8 .. 7
    check(not find("foo\nbar\nbar", re"(?m)^", m, start=9))
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
    #check find("foo\nbar\nbar", re"(?m)$", m, start=12) and
    #  m.boundaries == 12 .. 11
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
  check split("foobar", re"(?<=o)b") == @["foo", "ar"]
  check split("foobarbaz", re"(?<!o)b") == @["foobar", "az"]
  check split("foobar", re"o(?=b)") == @["fo", "bar"]
  check split("foobar", re"o(?!b)") == @["f", "obar"]
  block:
    var m: RegexMatch
    check find("abcxyz", re"(abc)|\w+", m)
    check m.boundaries == 0 .. 2
    check find("xyzabc", re"(abc)|\w+", m)
    check m.boundaries == 0 .. 5
  check findAndCaptureAll(
    "He was carefully disguised but captured quickly by police.",
    re"\w+ly") == @["carefully", "quickly"]

test "fix#83":
  block:
    let pattern = "^src/(?:[^\\/]*(?:\\/|$))*[^/]*\\.nim$"
    check "src/dir/foo.nim".match(pattern.re)
  check match("foo", re"(?:$)*\w*")
  check match("foo", re"($)*\w*")
  check match("foo", re"^*\w*")
  check match("foo", re"([^/]*$)*\w*")
  check match("src/dir/foo.nim", re"^src/(?:[^/]*(?:/|$))*[^/]*\.nim$")
  check(not match("foo", re"$($*)*(\w*)"))
  check(not match("foo", re"$($$*$*)*(\w*)"))
  check(not match("foo", re"($|$)($*)*(\w*)"))
  check(not match("foo", re"($|$$)($*)*(\w*)"))
  check(not match("foo", re"($$|$)($*)*(\w*)"))
  check(not match("foo", re"($|$)(\w*)"))
  check(not match("foo", re"($|$$)(\w*)"))
  check(not match("foo", re"($$|$)(\w*)"))
  check(not match("foo", re"(^$|$)(\w*)"))
  check(not match("foo", re"($|^$)(\w*)"))
  check match("", re"$*\w*")
  check match("foo", re"($*?)(\w*)")
  check match("foo", re"($*)(\w*)")
  check match("foox", re"($*)(\w*)x")
  check match("foo", re"(a*?$*?)*?(\w*)")
  check match("foox", re"(a*$*)*(\w*)x")
  check match("aaa", re"($|^)(a*)")
  check match("aaa", re"(^|$)(a*)")
  block:
    check findAllBounds("aaaxaaa", re"^(a*)") == @[0 .. 2]
    check findAllBounds("aaaxaaa", re"$(a*)") == @[7 .. 6]
    check findAllBounds("aaaxaaa", re"($|^)(a*)") == @[0 .. 2, 7 .. 6]
    check findAllBounds("aaaxaaa", re"(^|$)(a*)") == @[0 .. 2, 7 .. 6]
  when (NimMajor, NimMinor) >= (1, 1):
    check matchMacroCapt("foo", rex"($*?)(\w*)") == @["", "foo"]
    check matchMacroCapt("foo", rex"($*)(\w*)") == @["", "foo"]
    check matchMacroCapt("foox", rex"($*)(\w*)x") == @["", "foo"]
    check matchMacroCapt("foo", rex"(a*?$*?)*?(\w*)") == @["", "foo"]
    check matchMacroCapt("foox", rex"(a*$*)*(\w*)x") == @["", "foo"]
    check matchMacroCapt("aaa", rex"($|^)(a*)") == @["", "aaa"]
    check matchMacroCapt("aaa", rex"(^|$)(a*)") == @["", "aaa"]

test "escapeRe":
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
  check match("", re"|")
  check match("a", re"a|")
  check match("", re"a|")
  check(not match("b", re"a|"))
  check match("b", re"|b")
  check match("", re"|b")
  check(not match("a", re"|b"))
  check match("", re"(|)")
  check match("a", re"(a|)")
  check match("", re"(a|)")
  check(not match("b", re"(a|)"))
  check match("b", re"(|b)")
  check match("", re"(|b)")
  check(not match("a", re"(|b)"))
  check match("", re"||")
  check match("a", re"a||")
  check match("", re"a||")
  check match("b", re"||b")
  check match("", re"||b")
  check match("", re"a||b")
  check match("a", re"a||b")
  check match("b", re"a||b")
  check match("", re"|||")
  check match("a", re"a|||")
  check match("", re"a|||")
  check match("b", re"|||b")
  check match("", re"|||b")
  check match("a", re"a|||b")
  check match("b", re"a|||b")
  check match("", re"(||)")
  check match("a", re"(a||)")
  check match("", re"(a||)")
  check match("b", re"(||b)")
  check match("", re"(||b)")
  check match("1.1.1.1", re"(\d+)\.(\d+)(\.(\d+)|)(\.(\d+)|)")
  check match("1.1.1", re"(\d+)\.(\d+)(\.(\d+)|)(\.(\d+)|)")
  check match("1.1", re"(\d+)\.(\d+)(\.(\d+)|)(\.(\d+)|)")

test "issue_101":
  var m: RegexMatch
  check match("TXT1/TXT2.1", re"(TXT1)/TXT2()\.(\d+)")
  check match("TXT1/TXT2.1", re"(TXT1)/TXT2(?:)\.(\d+)")
  check match("TXT1/TXT2.1", re"(TXT1)/TXT2(?i:)\.(\d+)")
  check match("TXT1/TXT2.1", re"(TXT1)/TXT2(?P<foo>)\.(\d+)")
  check match("TXT1/TXT2.1", re"(TXT1)/TXT2()\.(\d+)", m) and
    m.captures == @[@[0 .. 3], @[9 .. 8], @[10 .. 10]]
  check match(" ", re"(?x)     (?-x) ")
  check match("aa", re"((?x)a    )a")
  check match("aa", re"((?x)   a    )a")
  check match(" ", re"((?x)) ")
  check match(" ", re"((?x)     ) ")
  check match(" ", re"(?x:(?x)     ) ")
  check match(" ", re"(?x:) ")
  check match(" ", re"(?x:   ) ")
  check match(" ", re"((?x:)) ")
  check match("A", re"(?xi)     a")
  check(not match("A", re"((?xi))     a"))
  check(not match("A", re"(?xi:(?xi)     )a"))
