import std/tables
import std/sequtils
import std/unicode
from std/strutils import addf

import pkg/regex/nodetype
import pkg/regex/common
import pkg/regex/parser
import pkg/regex/exptransformation
import pkg/regex/nfatype
import pkg/regex/nfa
import pkg/regex/nfamatch
import pkg/regex/nfamacro

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
    .nfa2(transitions)
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

when not defined(forceRegexAtRuntime):
  func re*(
    s: static string,
    flags: static[set[RegexFlag]] = {}
  ): static[Regex] {.inline.} =
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

when (NimMajor, NimMinor) >= (1, 1):
  func match*(
    s: string,
    pattern: static Regex,
    m: var RegexMatch,
    start = 0
  ): bool {.inline.} =
    const f: MatchFlags = {}
    result = matchImpl(s, pattern, m, f, start)

func match*(s: string, pattern: Regex): bool {.inline.} =
  var m: RegexMatch
  result = matchImpl(s, pattern, m, {mfNoCaptures})

when (NimMajor, NimMinor) >= (1, 1):
  func match*(s: string, pattern: static Regex): bool {.inline.} =
    var m: RegexMatch
    result = matchImpl(s, pattern, m, {mfNoCaptures})

template containsImpl(): untyped {.dirty.} =
  const f = {mfShortestMatch, mfFindMatch, mfNoCaptures}
  var m: RegexMatch
  result = matchImpl(s, pattern, m, f)

func contains*(s: string, pattern: Regex): bool =
  ## search for the pattern anywhere
  ## in the string. It returns as soon
  ## as there is a match, even when the
  ## expression has repetitions
  containsImpl()

when (NimMajor, NimMinor) >= (1, 1):
  func contains*(s: string, pattern: static Regex): bool =
    containsImpl()

template findImpl(): untyped {.dirty.} =
  matchImpl(s, pattern, m, {mfLongestMatch, mfFindMatch}, start)

func find*(
  s: string,
  pattern: Regex,
  m: var RegexMatch,
  start = 0
): bool =
  findImpl()

when (NimMajor, NimMinor) >= (1, 1):
  func find*(
    s: string,
    pattern: static Regex,
    m: var RegexMatch,
    start = 0
  ): bool =
    findImpl()

iterator findAll*(
  s: string,
  pattern: Regex,
  start = 0
): RegexMatch {.inline.} =
  var i = start
  var c: Rune
  var m: RegexMatch
  while i < len(s):
    if not find(s, pattern, m, i):
      break
    if i < m.boundaries.b+1:
      i = m.boundaries.b+1
    else:  # empty match
      fastRuneAt(s, i, c, true)
    yield m

func findAll*(
  s: string,
  pattern: Regex,
  start = 0
): seq[RegexMatch] =
  for m in findAll(s, pattern, start):
    result.add(m)

func findAndCaptureAll*(s: string, pattern: Regex): seq[string] =
  ## search through the string and
  ## return a seq with captures.
  runnableExamples:
    let
      captured = findAndCaptureAll("a1b2c3d4e5", re"\d")
      expected = @["1", "2", "3", "4", "5"]
    doAssert captured == expected

  for m in s.findAll(pattern):
    result.add(s[m.boundaries])

template runeIncAt(s: string, n: var int) =
  ## increment ``n`` up to
  ## next rune's index
  if n >= s.len:
    inc n
  else:
    inc(n, runeLenAt(s, n))

# XXX there is no static version because of Nim/issues/13791
iterator split*(s: string, sep: Regex): string {.inline.} =
  ## return not matched substrings
  var
    first, last = 0
    m: RegexMatch
  while last <= s.len:
    first = last
    while last <= s.len:
      if not find(s, sep, m, last):
        last = s.len + 1
        break
      if m.boundaries.a <= m.boundaries.b:
        last = m.boundaries.a
        break
      # empty match
      runeIncAt(s, last)
    yield substr(s, first, last-1)
    if m.boundaries.a <= m.boundaries.b:
      doAssert last < m.boundaries.b+1
      last = m.boundaries.b+1

func split*(s: string, sep: Regex): seq[string] =
  for w in split(s, sep):
    result.add w

func splitIncl*(s: string, sep: Regex): seq[string] =
  var
    first, last = 0
    m: RegexMatch
  while last <= s.len:
    first = last
    while last <= s.len:
      if not find(s, sep, m, last):
        last = s.len + 1
        break
      if m.boundaries.a <= m.boundaries.b:
        last = m.boundaries.a
        break
      # empty match
      runeIncAt(s, last)
    result.add substr(s, first, last-1)
    for g in 0 ..< m.groupsCount:
      for sl in m.group(g):
        result.add substr(s, sl.a, sl.b)
    if m.boundaries.a <= m.boundaries.b:
      doAssert last < m.boundaries.b+1
      last = m.boundaries.b+1

func startsWith*(s: string, pattern: Regex, start = 0): bool =
  ## return whether the string
  ## starts with the pattern or not
  var m: RegexMatch
  result = matchImpl(s, pattern, m, {mfShortestMatch, mfNoCaptures}, start)

when (NimMajor, NimMinor) >= (1, 1):
  func startsWith*(s: string, pattern: static Regex, start = 0): bool =
    var m: RegexMatch
    result = matchImpl(s, pattern, m, {mfShortestMatch, mfNoCaptures}, start)

template endsWithImpl(): untyped {.dirty.} =
  result = false
  var
    m: RegexMatch
    i = 0
  while i < s.len:
    result = matchImpl(s, pattern, m, {mfNoCaptures}, i)
    if result: return
    s.runeIncAt(i)

func endsWith*(s: string, pattern: Regex): bool =
  ## return whether the string
  ## ends with the pattern or not
  endsWithImpl()

when (NimMajor, NimMinor) >= (1, 1):
  func endsWith*(s: string, pattern: static Regex): bool =
    endsWithImpl()

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

# XXX there is no static version because of Nim/issues/13791
#     this func uses findAll iterator
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

proc isInitialized*(re: Regex): bool {.inline.} =
  ## Check whether the regex has been initialized
  runnableExamples:
    var re: Regex
    doAssert(not re.isInitialized)
    re = re"foo"
    doAssert re.isInitialized

  re.nfa.len > 0

proc toString(
  pattern: Regex,
  nIdx: int16,
  visited: var set[int16]
): string {.used.} =
  ## NFA to string representation.
  ## For debugging purposes
  # XXX zero-match transitions are missing
  if nIdx in visited:
    result = "[...]"
    return
  visited.incl(nIdx)
  let n = pattern.nfa[nIdx]
  result = "["
  result.add($n)
  for nn in n.next:
    result.add(", ")
    result.add(pattern.toString(nn, visited))
  result.add("]")

proc toString(pattern: Regex): string {.used.} =
  ## NFA to string representation.
  ## For debugging purposes
  var visited: set[int16]
  result = pattern.toString(0, visited)

when isMainModule:
  func toAtoms(s: string): string =
    var groups: GroupsCapture
    let atoms = s
      .parse
      .toAtoms(groups)
    result = atoms.toString

  func toNfaStr(s: string): string =
    result = re(s).toString

  doAssert toAtoms(r"a(b|c)*d") == r"a~(b|c)*~d"
  doAssert toAtoms(r"abc") == r"a~b~c"
  doAssert toAtoms(r"(abc|def)") == r"(a~b~c|d~e~f)"
  doAssert toAtoms(r"(abc|def)*xyz") == r"(a~b~c|d~e~f)*~x~y~z"
  doAssert toAtoms(r"a*b") == r"a*~b"
  doAssert toAtoms(r"(a)b") == r"(a)~b"
  doAssert toAtoms(r"(a)(b)") == r"(a)~(b)"
  doAssert toAtoms(r"\y") == r"y"
  doAssert toAtoms(r"a\*b") == r"a~*~b"
  doAssert toAtoms(r"\(a\)") == r"(~a~)"
  doAssert toAtoms(r"\w") == r"\w"
  doAssert toAtoms(r"\d") == r"\d"
  doAssert toAtoms(r"[a-z]") == r"[a-z]"
  doAssert toAtoms(r"[aa-zz]") == r"[aza-z]"
  doAssert toAtoms(r"[aa\-zz]") == r"[-az]"
  doAssert toAtoms(r"[^a]") == r"[^a]"
  doAssert toAtoms(r"(a*)*") != toAtoms(r"a*")
  doAssert toAtoms(r"(a*|b*)*") != toAtoms(r"(a|b)*")
  doAssert toAtoms(r"(a*b*)*") != toAtoms(r"(a|b)*")
  doAssert toAtoms(r"(a*|b*)") != toAtoms(r"(a|b)*")
  doAssert toAtoms(r"(a(b)){2}") == r"(a~(b))~(a~(b))"

  # trepetition_range_expand
  doAssert r"a{0}".toNfaStr == r"a".toNfaStr
  doAssert r"a{0}b".toNfaStr == r"ab".toNfaStr
  doAssert r"a{1}".toNfaStr == r"a".toNfaStr
  doAssert r"a{10}".toNfaStr == r"aaaaaaaaaa".toNfaStr
  doAssert r"a{1,}".toNfaStr == r"aa*".toNfaStr
  doAssert r"a{10,}".toNfaStr == r"aaaaaaaaaaa*".toNfaStr
  doAssert r"a{10,10}".toNfaStr == r"aaaaaaaaaa".toNfaStr
  doAssert r"a{0,0}".toNfaStr == r"a".toNfaStr
  doAssert r"a{1,2}".toNfaStr == r"aa?".toNfaStr
  doAssert r"a{2,4}".toNfaStr == r"aaa?a?".toNfaStr
  doAssert r"a{,10}".toNfaStr == r"a?a?a?a?a?a?a?a?a?a?".toNfaStr
  doAssert r"a{0,10}".toNfaStr == r"a?a?a?a?a?a?a?a?a?a?".toNfaStr
  doAssert r"a{,}".toNfaStr == r"a*".toNfaStr
  doAssert r"(a(b)){2}".toNfaStr == r"(a(b))(a(b))".toNfaStr

  # tascii_set
  doAssert r"[[:alnum:]]".toAtoms == "[[0-9a-zA-Z]]"
  doAssert r"[[:^alnum:]]".toAtoms == "[[^0-9a-zA-Z]]"
  doAssert r"[[:alpha:]]".toAtoms == "[[a-zA-Z]]"
  doAssert r"[[:ascii:]]".toAtoms == "[[\x00-\x7F]]"
  doAssert r"[[:blank:]]".toAtoms == "[[\t ]]"
  doAssert r"[[:cntrl:]]".toAtoms == "[[\x7F\x00-\x1F]]"
  doAssert r"[[:digit:]]".toAtoms == "[[0-9]]"
  doAssert r"[[:graph:]]".toAtoms == "[[!-~]]"
  doAssert r"[[:lower:]]".toAtoms == "[[a-z]]"
  doAssert r"[[:print:]]".toAtoms == "[[ -~]]"
  doAssert r"[[:punct:]]".toAtoms == "[[!-/:-@[-`{-~]]"
  doAssert r"[[:space:]]".toAtoms == "[[\t\n\v\f\r ]]"
  doAssert r"[[:upper:]]".toAtoms == "[[A-Z]]"
  doAssert r"[[:word:]]".toAtoms == "[[_0-9a-zA-Z]]"
  doAssert r"[[:xdigit:]]".toAtoms == "[[0-9a-fA-F]]"
  doAssert r"[[:alpha:][:digit:]]".toAtoms == "[[a-zA-Z][0-9]]"

  var m: RegexMatch

  #doAssert match("abc", re(r"abc", {reAscii}), m)
  doAssert match("abc", re"abc", m)
  doAssert match("ab", re"a(b|c)", m)
  doAssert match("ac", re"a(b|c)", m)
  doAssert(not match("ad", re"a(b|c)", m))
  doAssert match("ab", re"(ab)*", m)
  doAssert match("abab", re"(ab)*", m)
  doAssert(not match("ababc", re"(ab)*", m))
  doAssert(not match("a", re"(ab)*", m))
  doAssert match("ab", re"(ab)+", m)
  doAssert match("abab", re"(ab)+", m)
  doAssert(not match("ababc", re"(ab)+", m))
  doAssert(not match("a", re"(ab)+", m))
  doAssert match("aa", re"\b\b\baa\b\b\b", m)
  doAssert(not match("cac", re"c\ba\bc", m))
  doAssert match("abc", re"[abc]+", m)
  doAssert match("abc", re"[\w]+", m)
  doAssert match("弢弢弢", re"[\w]+", m)
  doAssert(not match("abc", re"[\d]+", m))
  doAssert match("123", re"[\d]+", m)
  doAssert match("abc$%&", re".+", m)
  doAssert(not match("abc$%&\L", re"(.+)", m))
  doAssert(not match("abc$%&\L", re".+", m))
  doAssert(not match("弢", re"\W", m))
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
  
  doAssert match("abc", re"abc")
  doAssert(not match("abc", re"abd"))
  doAssert(not match("abc", re"ab"))
  doAssert(not match("abc", re"b"))
  doAssert(not match("abc", re"c"))

  doAssert re"bc" in "abcd"
  doAssert re"(23)+" in "23232"
  doAssert re"^(23)+$" notin "23232"
  doAssert re"\w" in "弢"
  #doAssert re(r"\w", {reAscii}) notin "弢"
  #doAssert re(r"\w", {reAscii}) in "a"

  doAssert "abcd".find(re"bc", m)
  doAssert(not "abcd".find(re"de", m))
  #doAssert "%ab%".find(re(r"\w{2}", {reAscii}), m)
  doAssert "%弢弢%".find(re"\w{2}", m)
  #doAssert(not "%弢弢%".find(re(r"\w{2}", {reAscii}), m)
  doAssert(
    "2222".find(re"(22)*", m) and
    m.group(0) == @[0 .. 1, 2 .. 3])
  doAssert(
    "11222211".find(re"(22)+", m) and
    m.group(0) == @[2 .. 3, 4 .. 5])
  
  doAssert match("650-253-0001", re"[0-9]+-[0-9]+-[0-9]+", m)
  doAssert(not match("abc-253-0001", re"[0-9]+-[0-9]+-[0-9]+", m))
  doAssert(not match("650-253", re"[0-9]+-[0-9]+-[0-9]+", m))
  doAssert(not match("650-253-0001-abc", re"[0-9]+-[0-9]+-[0-9]+", m))
  doAssert match("650-253-0001", re"[0-9]+..*", m)
  doAssert(not match("abc-253-0001", re"[0-9]+..*", m))
  doAssert(not match("6", re"[0-9]+..*", m))

  doAssert match("abcabcabc", re"(?:(?:abc)){3}")
  doAssert match("abcabcabc", re"((abc)){3}")
