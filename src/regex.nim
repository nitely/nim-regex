import std/tables
import std/sequtils
import std/unicode
from std/strutils import addf

import pkg/regex/nodetype
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

when not defined(forceRegexAtRuntime):
  func re*(
    s: static string,
    flags: static set[RegexFlag] = {}
  ): static Regex {.inline.} =
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

# XXX reset each result[i] ?
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

proc isInitialized*(re: Regex): bool =
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
