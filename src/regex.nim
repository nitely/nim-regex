import std/tables
import std/sequtils
import std/unicode
from std/strutils import addf

import pkg/regex/common
import pkg/regex/parser
import pkg/regex/exptransformation
import pkg/regex/nfa
import pkg/regex/nfamatch

export
  Regex,
  RegexMatch,
  RegexFlag,
  RegexError

template reImpl(s, flags: untyped): Regex =
  var groups: GroupsCapture
  var transitions: Transitions
  let nfa = s
    .parse
    .transformExp(groups)
    .nfa(transitions)
  Regex(
    nfa: nfa,
    transitions: transitions,
    groupsCount: groups.count,
    namedGroups: groups.names,
    flags: flags)

func re*(
  s: string,
  flags: set[RegexFlag] = {}
): Regex {.inline.} =
  reImpl(s, flags)

iterator group*(m: RegexMatch, i: int): Slice[int] =
  ## return slices for a given group.
  ## Slices of start > end are empty
  ## matches (i.e.: ``re"(\d?)"``)
  ## and they are included same as in PCRE.
  runnableExamples:
    let text = "abc"
    var m: RegexMatch
    doAssert text.match(re"(\w)+", m)
    var captures = newSeq[string]()
    for bounds in m.group(0):
      captures.add(text[bounds])
    doAssert captures == @["a", "b", "c"]

  for capt in m.captures[i]:
    yield capt

func group*(m: RegexMatch, i: int): seq[Slice[int]] =
  ## return slices for a given group.
  ## Use the iterator version if you care about performance
  m.captures[i]

iterator group*(m: RegexMatch, s: string): Slice[int] =
  ## return slices for a given named group
  runnableExamples:
    let text = "abc"
    var m: RegexMatch
    doAssert text.match(re"(?P<foo>\w)+", m)
    var captures = newSeq[string]()
    for bounds in m.group("foo"):
      captures.add(text[bounds])
    doAssert captures == @["a", "b", "c"]

  for bounds in m.group(m.namedGroups[s]):
    yield bounds

func group*(m: RegexMatch, s: string): seq[Slice[int]] =
  ## return slices for a given named group.
  ## Use the iterator version if you care about performance
  m.group(m.namedGroups[s])

func groupsCount*(m: RegexMatch): int =
  ## return the number of capturing groups
  runnableExamples:
    var m: RegexMatch
    doAssert "ab".match(re"(a)(b)", m)
    doAssert m.groupsCount == 2

  m.captures.len

func groupNames*(m: RegexMatch): seq[string] =
  ## return the names of capturing groups.
  runnableExamples:
    let text = "hello world"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?P<who>world)", m)
    doAssert m.groupNames() == @["greet", "who"]

  result = toSeq(m.namedGroups.keys)

func group*(
  m: RegexMatch,
  groupName: string,
  text: string
): seq[string] = 
  ## return seq of captured text by group `groupName`
  runnableExamples:
    let text = "hello beautiful world"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    doAssert m.group("greet", text) == @["hello"]
    doAssert m.group("who", text) == @["beautiful", "world"]

  result = newSeq[string]()
  for bounds in m.group(groupName):
    result.add text[bounds]

func groupFirstCapture*(
  m: RegexMatch,
  groupName: string,
  text: string
): string =
  ## return first capture for a given capturing group
  runnableExamples:
    let text = "hello beautiful world"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    doAssert m.groupFirstCapture("greet", text) == "hello"
    doAssert m.groupFirstCapture("who", text) == "beautiful"

  let captures = m.group(groupName, text)
  if captures.len > 0:
    return captures[0]
  else:
    return "" 

func groupLastCapture*(
  m: RegexMatch,
  groupName: string,
  text: string
): string =
  ## return last capture for a given capturing group
  runnableExamples:
    let text = "hello beautiful world"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    doAssert m.groupLastCapture("greet", text) == "hello"
    doAssert m.groupLastCapture("who", text) == "world"

  let captures = m.group(groupName, text)
  if captures.len > 0:
    return captures[captures.len-1]
  else:
    return ""

func match*(
  s: string,
  pattern: Regex,
  m: var RegexMatch,
  start = 0
): bool {.inline.} =
  const f: MatchFlags = {}
  result = matchImpl(s, pattern, m, f, start)

func match*(s: string, pattern: Regex): bool {.inline.} =
  var m: RegexMatch
  result = matchImpl(s, pattern, m, {mfNoCaptures})

func contains*(s: string, pattern: Regex): bool =
  ## search for the pattern anywhere
  ## in the string. It returns as soon
  ## as there is a match, even when the
  ## expression has repetitions
  result = false
  var m: RegexMatch
  var i = 0
  var c: Rune
  while i < len(s):
    result = matchImpl(s, pattern, m, {mfShortestMatch, mfNoCaptures}, i)
    if result:
      break
    fastRuneAt(s, i, c, true)

func find*(
  s: string,
  pattern: Regex,
  m: var RegexMatch,
  start = 0
): bool =
  result = false
  var i = start
  var c: Rune
  while i < len(s):
    result = matchImpl(s, pattern, m, {mfShortestMatch, mfNoCaptures}, i)
    if result:
      result = matchImpl(s, pattern, m, {mfLongestMatch}, i)
      doAssert result
      break
    fastRuneAt(s, i, c, true)

iterator findAll*(
  s: string,
  pattern: Regex,
  start = 0
): RegexMatch {.inline.} =
  var i = start
  var c: Rune
  var m: RegexMatch
  while i < len(s):
    if find(s, pattern, m, i):
      if i < m.boundaries.b+1:
        i = m.boundaries.b+1
      else:
        fastRuneAt(s, i, c, true)
      yield m
    else:
      fastRuneAt(s, i, c, true)

func findAll*(
  s: string,
  pattern: Regex,
  start = 0
): seq[RegexMatch] =
  for m in findAll(s, pattern, start):
    result.add(m)

template runeIncAt(s: string, n: var int) =
  ## increment ``n`` up to
  ## next rune's index
  if n >= s.len:
    inc n
  else:
    inc(n, runeLenAt(s, n))

iterator split*(s: string, sep: Regex): string {.inline.} =
  ## return not matched substrings
  var
    first = 0
    last = 0
    m: RegexMatch
  while last <= s.len:
    first = last
    while last <= s.len:
      discard matchImpl(s, sep, m, {mfLongestMatch, mfNoCaptures}, last)
      if m.boundaries.a <= m.boundaries.b:
        break
      s.runeIncAt last
    yield substr(s, first, last-1)
    if m.boundaries.a <= m.boundaries.b:
      assert last < m.boundaries.b+1
      last = m.boundaries.b+1

func split*(s: string, sep: Regex): seq[string] =
  for w in split(s, sep):
    result.add w

func splitIncl*(s: string, sep: Regex): seq[string] =
  var
    first = 0
    last = 0
    m: RegexMatch
  while last <= s.len:
    first = last
    while last <= s.len:
      discard matchImpl(s, sep, m, {mfLongestMatch, mfNoCaptures}, last)
      if m.boundaries.a <= m.boundaries.b:
        break
      s.runeIncAt last
    result.add substr(s, first, last-1)
    for g in 0 ..< m.groupsCount:
      for sl in m.group(g):
        result.add substr(s, sl.a, sl.b)
    if m.boundaries.a <= m.boundaries.b:
      assert last < m.boundaries.b+1
      last = m.boundaries.b+1

func startsWith*(s: string, pattern: Regex, start = 0): bool =
  ## return whether the string
  ## starts with the pattern or not
  var m: RegexMatch
  result = matchImpl(s, pattern, m, {mfShortestMatch, mfNoCaptures}, start)

func endsWith*(s: string, pattern: Regex): bool =
  ## return whether the string
  ## ends with the pattern or not
  result = false
  var
    m: RegexMatch
    i = 0
  while i < s.len:
    result = matchImpl(s, pattern, m, {mfNoCaptures}, i)
    if result: return
    s.runeIncAt(i)

func flatCaptures(
  result: var seq[string],
  m: RegexMatch,
  s: string
) {.inline.} =
  ## Concat capture repetitions
  var i, n = 0
  for g in 0 ..< m.groupsCount:
    n = 0
    for sl in m.group(g):
      if sl.a <= sl.b:
        n += sl.b - sl.a + 1
    i = 0
    result[g].setLen(n)
    for sl in m.group(g):
      for c in sl:
        result[g][i] = s[c]
        inc i
    assert i == n

func addsubstr(result: var string, s: string, first, last: int) =
  let
    first = max(first, 0)
    last = min(last, s.high)
  if first > last: return
  let n = result.len
  result.setLen(result.len + last - first + 1)
  # XXX copyMem
  var j = 0
  for i in first .. last:
    result[n + j] = s[i]
    inc j

func addsubstr(result: var string, s: string, first: int) {.inline.} =
  addsubstr(result, s, first, s.high)

func replace*(
  s: string,
  pattern: Regex,
  by: string,
  limit = 0
): string =
  ## Replace matched substrings.
  ##
  ## Matched groups can be accessed with ``$N``
  ## notation, where ``N`` is the group's index,
  ## starting at 1 (1-indexed). ``$$`` means
  ## literal ``$``.
  ##
  ## If ``limit`` is given, at most ``limit``
  ## replacements are done. ``limit`` of 0
  ## means there is no limit
  result = ""
  var
    i, j = 0
    capts = newSeq[string](pattern.groupsCount)
  for m in findAll(s, pattern):
    result.addsubstr(s, i, m.boundaries.a - 1)
    flatCaptures(capts, m, s)
    if capts.len > 0:
      result.addf(by, capts)
    else:
      result.add(by)
    i = m.boundaries.b + 1
    inc j
    if limit > 0 and j == limit: break
  result.addsubstr(s, i)

func replace*(
  s: string,
  pattern: Regex,
  by: proc (m: RegexMatch, s: string): string,
  limit = 0
): string =
  ## Replace matched substrings.
  ##
  ## If ``limit`` is given, at most ``limit``
  ## replacements are done. ``limit`` of 0
  ## means there is no limit
  result = ""
  var i, j = 0
  for m in findAll(s, pattern):
    result.addsubstr(s, i, m.boundaries.a - 1)
    result.add(by(m, s))
    i = m.boundaries.b + 1
    inc j
    if limit > 0 and j == limit: break
  result.addsubstr(s, i)

when isMainModule:
  var m: RegexMatch

  doAssert match("abc", re(r"abc", {reAscii}), m)
  doAssert match("abc", re"abc", m)
  doAssert match("ab", re"a(b|c)", m)
  doAssert match("ac", re"a(b|c)", m)
  doAssert not match("ad", re"a(b|c)", m)
  doAssert match("ab", re"(ab)*", m)
  doAssert match("abab", re"(ab)*", m)
  doAssert not match("ababc", re"(ab)*", m)
  doAssert not match("a", re"(ab)*", m)
  doAssert match("ab", re"(ab)+", m)
  doAssert match("abab", re"(ab)+", m)
  doAssert not match("ababc", re"(ab)+", m)
  doAssert not match("a", re"(ab)+", m)
  doAssert match("aa", re"\b\b\baa\b\b\b", m)
  doAssert not match("cac", re"c\ba\bc", m)
  doAssert match("abc", re"[abc]+", m)
  doAssert match("abc", re"[\w]+", m)
  doAssert match("弢弢弢", re"[\w]+", m)
  doAssert not match("abc", re"[\d]+", m)
  doAssert match("123", re"[\d]+", m)
  doAssert match("abc$%&", re".+", m)
  doAssert not match("abc$%&\L", re"(.+)", m)
  doAssert not match("abc$%&\L", re".+", m)
  doAssert not match("弢", re"\W", m)
  doAssert match("$%&", re"\W+", m)
  doAssert match("abc123", re"[^\W]+", m)

  doAssert match("aabcd", re"(aa)bcd", m) and
    m.captures == @[@[0 .. 1]]
  doAssert match("aabc", re"(aa)(bc)", m) and
    m.captures == @[@[0 .. 1], @[2 .. 3]]
  doAssert match("ab", re"a(b|c)", m) and
    m.captures == @[@[1 .. 1]]
  doAssert match("ab", re"(ab)*", m) and
    m.captures == @[@[0 .. 1]]
  doAssert match("abab", re"(ab)*", m) and
    m.captures == @[@[0 .. 1, 2 .. 3]]
  doAssert match("ab", re"((a))b", m) and
    m.captures == @[@[0 .. 0], @[0 .. 0]]
  doAssert match("c", re"((ab)*)c", m) and
    m.captures == @[@[0 .. -1], @[]]
  doAssert match("aab", re"((a)*b)", m) and
    m.captures == @[@[0 .. 2], @[0 .. 0, 1 .. 1]]
  doAssert match("abbbbcccc", re"a(b|c)*", m) and
    m.captures == @[@[1 .. 1, 2 .. 2, 3 .. 3, 4 .. 4, 5 .. 5, 6 .. 6, 7 .. 7, 8 .. 8]]
  doAssert match("ab", re"(a*)(b*)", m) and
    m.captures == @[@[0 .. 0], @[1 .. 1]]
  doAssert match("ab", re"(a)*(b)*", m) and
    m.captures == @[@[0 .. 0], @[1 .. 1]]
  doAssert match("ab", re"(a)*b*", m) and
    m.captures == @[@[0 .. 0]]
  doAssert match("abbb", re"((a(b)*)*(b)*)", m) and
    m.captures == @[@[0 .. 3], @[0 .. 3], @[1 .. 1, 2 .. 2, 3 .. 3], @[]]
  doAssert match("aa", re"(a)+", m) and
    m.captures == @[@[0 .. 0, 1 .. 1]]
  doAssert match("abab", re"(ab)+", m) and
    m.captures == @[@[0 .. 1, 2 .. 3]]
  doAssert match("a", re"(a)?", m) and
    m.captures == @[@[0 .. 0]]
  doAssert match("ab", re"(ab)?", m) and
    m.captures == @[@[0 .. 1]]
  doAssert match("aaabbbaaa", re"(a*|b*)*", m) and
    m.captures == @[@[0 .. 2, 3 .. 5, 6 .. 8]]
  doAssert match("abab", re"(a(b))*", m) and
    m.captures == @[@[0 .. 1, 2 .. 3], @[1 .. 1, 3 .. 3]]
  doAssert match("aaanasdnasd", re"((a)*n?(asd)*)*", m) and
    m.captures == @[@[0 .. 6, 7 .. 10], @[0 .. 0, 1 .. 1, 2 .. 2], @[4 .. 6, 8 .. 10]]
  doAssert match("aaanasdnasd", re"((a)*n?(asd))*", m) and
    m.captures == @[@[0 .. 6, 7 .. 10], @[0 .. 0, 1 .. 1, 2 .. 2], @[4 .. 6, 8 .. 10]]
  doAssert match("abd", re"((ab)c)|((ab)d)", m) and
    m.captures == @[@[], @[], @[0 .. 2], @[0 .. 1]]
  doAssert match("aaa", re"(a*)", m) and
    m.captures == @[@[0 .. 2]]
  doAssert match("aaaa", re"(a*)(a*)", m) and
    m.captures == @[@[0 .. 3], @[4 .. 3]]
  doAssert match("aaaa", re"(a*?)(a*?)", m) and
    m.captures == @[@[0 .. -1], @[0 .. 3]]
  doAssert match("aaaa", re"(a)*(a)", m) and
    m.captures == @[@[0 .. 0, 1 .. 1, 2 .. 2], @[3 .. 3]]
  doAssert match("aa", re"\baa\b", m) and
    m.boundaries == 0 .. 1

  doAssert match("abc", re"abc")
  doAssert not match("abc", re"abd")
  doAssert not match("abc", re"ab")
  doAssert not match("abc", re"b")
  doAssert not match("abc", re"c")

  doAssert match("650-253-0001", re"[0-9]+-[0-9]+-[0-9]+", m)
  doAssert not match("abc-253-0001", re"[0-9]+-[0-9]+-[0-9]+", m)
  doAssert not match("650-253", re"[0-9]+-[0-9]+-[0-9]+", m)
  doAssert not match("650-253-0001-abc", re"[0-9]+-[0-9]+-[0-9]+", m)
  doAssert match("650-253-0001", re"[0-9]+..*", m)
  doAssert not match("abc-253-0001", re"[0-9]+..*", m)
  doAssert not match("6", re"[0-9]+..*", m)

  doAssert match("abcabcabc", re"(?:(?:abc)){3}")
  doAssert match("abcabcabc", re"((abc)){3}")

  doAssert re"bc" in "abcd"
  doAssert re"(23)+" in "23232"
  doAssert re"^(23)+$" notin "23232"

  doAssert "abcd".find(re"bc", m)
  doAssert not "abcd".find(re"de", m)
  doAssert "%弢弢%".find(re"\w{2}", m)
  doAssert "2222".find(re"(22)*", m) and
    m.group(0) == @[0 .. 1, 2 .. 3]
  doAssert "11222211".find(re"(22)+", m) and
    m.group(0) == @[2 .. 3, 4 .. 5]

  func findAllBounds(s: string, reg: Regex): seq[Slice[int]] =
    result = map(
      findAll(s, reg),
      func (m: RegexMatch): Slice[int] =
        m.boundaries)

  doAssert findAllBounds("abcabc", re"bc") == @[1 .. 2, 4 .. 5]
  doAssert findAllBounds("aa", re"a") == @[0 .. 0, 1 .. 1]
  doAssert findAllBounds("a", re"a") == @[0 .. 0]
  doAssert findAllBounds("a", re"b") == newSeq[Slice[int]]()
  doAssert findAllBounds("", re"b") == newSeq[Slice[int]]()
  doAssert findAllBounds("a", re"") == @[0 .. -1]
  doAssert findAllBounds("ab", re"") == @[0 .. -1, 1 .. 0]
  doAssert findAllBounds("a", re"\b") == @[0 .. -1]
  doAssert findAllBounds("ab", re"\b") == @[0 .. -1, 1 .. 0]
  doAssert findAllBounds("aΪⒶ弢", re"Ϊ") == @[1 .. 2]
  doAssert findAllBounds("aΪⒶ弢", re"Ⓐ") == @[3 .. 5]
  doAssert findAllBounds("aΪⒶ弢", re"弢") == @[6 .. 9]
  doAssert findAllBounds("aΪⒶ弢aΪⒶ弢", re"Ⓐ") == @[3 .. 5, 13 .. 15]
  doAssert findAllBounds("aaa", re"a*") == @[0 .. 2]

  doAssert split("a,b,c", re",") == @["a", "b", "c"]
  doAssert split("00232this02939is39an22example111", re"\d+") ==
    @["", "this", "is", "an", "example", ""]
  doAssert split("AAA :   : BBB", re"\s*:\s*") == @["AAA", "", "BBB"]
  doAssert split("", re",") == @[""]
  doAssert split(",,", re",") == @["", "", ""]
  doAssert split("abc", re"") == @["abc"]
  doAssert split(",a,Ϊ,Ⓐ,弢,", re",") ==
    @["", "a", "Ϊ", "Ⓐ", "弢", ""]
  doAssert split("弢", re"\xAF") == @["弢"]  # "弢" == "\xF0\xAF\xA2\x94"
  doAssert split("Words, words, words.", re"\W+") ==
    @["Words", "words", "words", ""]
  doAssert split("0a3B9", re"[a-fA-F]+") ==
    @["0", "3", "9"]
  doAssert split("1 2 3 4 5 6 ", re" ") ==
    @["1", "2", "3", "4", "5", "6", ""]
  doAssert split("1  2  ", re" ") == @["1", "", "2", "", ""]
  doAssert split("1 2", re" ") == @["1", "2"]
  doAssert split("foo", re"foo") == @["", ""]
  doAssert split("", re"foo") == @[""]

  doAssert "a,b".splitIncl(re"(,)") == @["a", ",", "b"]
  doAssert "12".splitIncl(re"(\d)") == @["", "1", "", "2", ""]
  doAssert splitIncl("aΪⒶ弢", re"(\w)") ==
    @["", "a", "", "Ϊ", "", "Ⓐ", "", "弢", ""]
  doAssert splitIncl("aΪⒶ弢", re"") == @["aΪⒶ弢"]
  doAssert splitIncl("...words, words...", re"(\W+)") ==
    @["", "...", "words", ", ", "words", "...", ""]
  doAssert splitIncl("Words, words, words.", re"(\W+)") ==
    @["Words", ", ", "words", ", ", "words", ".", ""]

  # regular split stuff
  doAssert splitIncl("a,b,c", re",") == @["a", "b", "c"]
  doAssert splitIncl("00232this02939is39an22example111", re"\d+") ==
    @["", "this", "is", "an", "example", ""]
  doAssert splitIncl("AAA :   : BBB", re"\s*:\s*") ==
    @["AAA", "", "BBB"]
  doAssert splitIncl("", re",") == @[""]
  doAssert splitIncl(",,", re",") == @["", "", ""]
  doAssert splitIncl("abc", re"") == @["abc"]
  doAssert splitIncl(",a,Ϊ,Ⓐ,弢,", re",") ==
    @["", "a", "Ϊ", "Ⓐ", "弢", ""]
  doAssert splitIncl("弢", re"\xAF") == @["弢"]  # "弢" == "\xF0\xAF\xA2\x94"
  doAssert splitIncl("Words, words, words.", re"\W+") ==
    @["Words", "words", "words", ""]
  doAssert splitIncl("0a3B9", re"[a-fA-F]+") ==
    @["0", "3", "9"]
  doAssert splitIncl("1 2 3 4 5 6 ", re" ") ==
    @["1", "2", "3", "4", "5", "6", ""]
  doAssert splitIncl("1  2  ", re" ") == @["1", "", "2", "", ""]
  doAssert splitIncl("1 2", re" ") == @["1", "2"]
  doAssert splitIncl("foo", re"foo") == @["", ""]
  doAssert splitIncl("", re"foo") == @[""]

  doAssert "abc".startsWith(re"ab")
  doAssert not "abc".startsWith(re"bc")
  doAssert startsWith("弢ⒶΪ", re"弢Ⓐ")
  doAssert startsWith("弢", re("\xF0\xAF\xA2\x94"))
  doAssert not startsWith("弢", re("\xF0\xAF\xA2"))
  doAssert "abc".startsWith(re"\w")
  doAssert not "abc".startsWith(re"\d")
  doAssert "abc".startsWith(re"(a|b)")
  doAssert "bc".startsWith(re"(a|b)")
  doAssert not "c".startsWith(re"(a|b)")

  doAssert "abc".endsWith(re"bc")
  doAssert not "abc".endsWith(re"ab")
  doAssert endsWith("弢ⒶΪ", re"ⒶΪ")
  doAssert endsWith("弢", re("\xF0\xAF\xA2\x94"))
  doAssert not endsWith("弢", re("\xAF\xA2\x94"))
  doAssert "abc".endsWith(re"(b|c)")
  doAssert "ab".endsWith(re"(b|c)")
  doAssert not "a".endsWith(re"(b|c)")

  doAssert "a".replace(re"(a)", "m($1)") ==
    "m(a)"
  doAssert "a".replace(re"(a)", "m($1) m($1)") ==
    "m(a) m(a)"
  doAssert "aaa".replace(re"(a*)", "m($1)") ==
    "m(aaa)"
  doAssert "abc".replace(re"(a(b)c)", "m($1) m($2)") ==
    "m(abc) m(b)"
  doAssert "abc".replace(re"(a(b))(c)", "m($1) m($2) m($3)") ==
    "m(ab) m(b) m(c)"
  doAssert "abcabc".replace(re"(abc)*", "m($1)") ==
    "m(abcabc)"
  doAssert "abcabc".replace(re"(abc)", "m($1)") ==
    "m(abc)m(abc)"
  doAssert "abcabc".replace(re"(abc)", "m($1)") ==
    "m(abc)m(abc)"
  doAssert "abcab".replace(re"(abc)", "m($1)") ==
    "m(abc)ab"
  doAssert "abcabc".replace(re"((abc)*)", "m($1) m($2)") ==
    "m(abcabc) m(abcabc)"
  doAssert "abcabc".replace(re"((a)bc)*", "m($1) m($2)") ==
    "m(abcabc) m(aa)"
  doAssert "abc".replace(re"(b)", "m($1)") == "am(b)c"
  doAssert "abc".replace(re"d", "m($1)") == "abc"
  doAssert "abc".replace(re"(d)", "m($1)") == "abc"
  doAssert "aaa".replace(re"a", "b") == "bbb"
  doAssert "aaa".replace(re"a", "b", 1) == "baa"
  doAssert "Nim is awesome!".replace(re"(\w\B)", "$1_") ==
    "N_i_m i_s a_w_e_s_o_m_e!"

  doAssert "12".split(re"\w\b") == @["1", ""]
  doAssert "12".split(re"\w\B") == @["", "2"]

  block:
    proc by(m: RegexMatch, s: string): string =
      result = "m("
      for g in 0 ..< m.groupsCount:
        for sl in m.group(g):
          result.add(s[sl])
          result.add(',')
      result.add(')')

    doAssert "abc".replace(re"(b)", by) == "am(b,)c"
    doAssert "aaa".replace(re"(a*)", by) == "m(aaa,)"
    doAssert "aaa".replace(re"(a)*", by) == "m(a,a,a,)"

  block:
    proc removeEvenWords(m: RegexMatch, s: string): string =
      if m.group(1).len mod 2 != 0:
        result = s[m.group(0)[0]]
      else:
        result = ""

    let
      text = "Es macht Spaß, alle geraden Wörter zu entfernen!"
      expected = "macht , geraden entfernen!"
    doAssert text.replace(re"((\w)+\s*)", removeEvenWords) == expected
