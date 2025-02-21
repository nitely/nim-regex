# Deprecated regex.nim API

import std/tables
import std/sequtils
import std/unicode
from std/strutils import addf

import ./types
import ./common
import ./compiler
import ./nfatype
import ./nfafindall
import ./nfamatch
when not defined(noRegexOpt):
  import ./litopt

export
  Regex,
  RegexMatch,
  RegexError

template debugCheckUtf8(s: untyped): untyped =
  when not defined(release):
    assert(verifyUtf8(s) == -1, "Invalid utf-8 input")

when defined(noRegexOpt):
  template findSomeOptTpl(s, pattern, ms, i): untyped =
    findSomeImpl(s, pattern, ms, i)
else:
  template findSomeOptTpl(s, pattern, ms, i): untyped =
    if pattern.litOpt.canOpt:
      findSomeOptImpl(s, pattern, ms, i)
    else:
      findSomeImpl(s, pattern, ms, i)

# below deprecated funcs call each other,
# so turn warnings
{.push warning[Deprecated]: off.}

func re*(
  s: string
): Regex {.raises: [RegexError], deprecated: "use re2(string) instead".} =
  reImpl(s)

when not defined(forceRegexAtRuntime):
  func re*(
    s: static string
  ): static[Regex] {.deprecated: "use re2(static string) instead".} =
    reCt(s)

func toPattern*(
  s: string
): Regex {.raises: [RegexError], deprecated: "Use `re2(string)` instead".} =
  re(s)

iterator group*(m: RegexMatch, i: int): Slice[int] {.inline, raises: [], deprecated.} =
  for capt in m.captures[i]:
    yield capt

func group*(m: RegexMatch, i: int): seq[Slice[int]] {.inline, raises: [], deprecated: "use group(RegexMatch2, int)".} =
  m.captures[i]

func group*(
  m: RegexMatch, i: int, text: string
): seq[string] {.inline, raises: [], deprecated.} =
  result = newSeq[string]()
  for bounds in m.group i:
    result.add text[bounds]

func groupFirstCapture*(
  m: RegexMatch, i: int, text: string
): string {.inline, raises: [], deprecated.} =
  result = ""
  for bounds in m.group i:
    return text[bounds]

func groupLastCapture*(
  m: RegexMatch, i: int, text: string
): string {.inline, raises: [], deprecated: "use group(RegexMatch2, int) instead".} =
  var b = 0 .. -1
  for bounds in m.group i:
    b = bounds
  result = text[b]

iterator group*(
  m: RegexMatch, s: string
): Slice[int] {.inline, raises: [KeyError], deprecated.} =
  for bounds in m.group(m.namedGroups[s]):
    yield bounds

func group*(
  m: RegexMatch, s: string
): seq[Slice[int]] {.inline, raises: [KeyError], deprecated: "use group(RegexMatch2, string)".} =
  m.group m.namedGroups[s]

func group*(
  m: RegexMatch,
  groupName: string,
  text: string
): seq[string] {.inline, raises: [KeyError], deprecated.} =
  result = newSeq[string]()
  for bounds in m.group(groupName):
    result.add text[bounds]

func groupFirstCapture*(
  m: RegexMatch,
  groupName: string,
  text: string
): string {.inline, raises: [KeyError], deprecated.} =
  let captures = m.group(groupName, text)
  if captures.len > 0:
    return captures[0]
  else:
    return "" 

func groupLastCapture*(
  m: RegexMatch,
  groupName: string,
  text: string
): string {.inline, raises: [KeyError], deprecated: "use group(RegexMatch2, string) instead".} =
  let captures = m.group(groupName, text)
  if captures.len > 0:
    return captures[captures.len-1]
  else:
    return ""

func groupsCount*(m: RegexMatch): int {.inline, raises: [], deprecated: "use groupsCount(RegexMatch2)".} =
  m.captures.len

func groupNames*(m: RegexMatch): seq[string] {.inline, raises: [], deprecated: "use groupNames(RegexMatch2)".} =
  result = toSeq(m.namedGroups.keys)

func match*(
  s: string,
  pattern: Regex,
  m: var RegexMatch,
  start = 0
): bool {.raises: [], deprecated: "use match(string, Regex2, var RegexMatch2) instead".} =
  debugCheckUtf8 s
  result = matchImpl(s, pattern, m, start)

func match*(s: string, pattern: Regex): bool {.raises: [], deprecated: "use match(string, Regex2) instead".} =
  debugCheckUtf8 s
  var m = RegexMatch()
  result = matchImpl(s, pattern, m)

iterator findAll*(
  s: string,
  pattern: Regex,
  start = 0
): RegexMatch {.inline, raises: [], deprecated: "use findAll(string, Regex2) instead".} =
  debugCheckUtf8 s
  var i = start
  var i2 = start-1
  var m = RegexMatch()
  var ms = RegexMatches()
  while i <= len(s):
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, pattern, ms, i)
    #debugEcho i
    if i < 0: break
    for mi in ms:
      fillMatchImpl(m, mi, ms, pattern)
      yield m
    if i == len(s):
      break

func findAll*(
  s: string,
  pattern: Regex,
  start = 0
): seq[RegexMatch] {.raises: [], deprecated: "use findAll(string, Regex2) instead".} =
  result = newSeq[RegexMatch]()
  for m in findAll(s, pattern, start):
    result.add m

iterator findAllBounds*(
  s: string,
  pattern: Regex,
  start = 0
): Slice[int] {.inline, raises: [], deprecated: "use findAllBounds(string, Regex2) instead".} =
  debugCheckUtf8 s
  var i = start
  var i2 = start-1
  var ms = RegexMatches()
  while i <= len(s):
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, pattern, ms, i)
    #debugEcho i
    if i < 0: break
    for ab in ms.bounds:
      yield ab
    if i == len(s):
      break

func findAllBounds*(
  s: string,
  pattern: Regex,
  start = 0
): seq[Slice[int]] {.raises: [], deprecated: "use findAllBounds(string, Regex2) instead".} =
  result = newSeq[Slice[int]]()
  for m in findAllBounds(s, pattern, start):
    result.add m

func findAndCaptureAll*(
  s: string, pattern: Regex
): seq[string] {.raises: [], deprecated: "use findAll(string, Regex2) instead".} =
  result = newSeq[string]()
  for m in s.findAll(pattern):
    result.add s[m.boundaries]

func contains*(s: string, pattern: Regex): bool {.raises: [], deprecated: "use contains(string, Regex2) instead".} =
  for _ in findAllBounds(s, pattern):
    return true
  return false

func find*(
  s: string,
  pattern: Regex,
  m: var RegexMatch,
  start = 0
): bool {.raises: [], deprecated: "use find(string, Regex2, var RegexMatch2) instead".} =
  m.clear()
  for m2 in findAll(s, pattern, start):
    m.captures.add m2.captures
    m.namedGroups = m2.namedGroups
    m.boundaries = m2.boundaries
    return true
  return false

iterator split*(s: string, sep: Regex): string {.inline, raises: [], deprecated: "use split(string, Regex2) instead".} =
  debugCheckUtf8 s
  var
    first, last, i = 0
    i2 = -1
    done = false
    ms = RegexMatches()
  while not done:
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, sep, ms, i)
    done = i < 0 or i >= len(s)
    if done: ms.dummyMatch(s.len)
    for ab in ms.bounds:
      last = ab.a
      if ab.a > 0 or ab.a <= ab.b:  # skip first empty match
        yield substr(s, first, last-1)
      first = ab.b+1

func split*(s: string, sep: Regex): seq[string] {.raises: [], deprecated: "use split(string, Regex2) instead".} =
  result = newSeq[string]()
  for w in split(s, sep):
    result.add w

func splitIncl*(s: string, sep: Regex): seq[string] {.raises: [], deprecated: "use splitIncl(string, Regex2) instead".} =
  template ab: untyped = m.boundaries
  debugCheckUtf8 s
  result = newSeq[string]()
  var
    first, last, i = 0
    i2 = -1
    done = false
    m = RegexMatch()
    ms = RegexMatches()
  while not done:
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, sep, ms, i)
    done = i < 0 or i >= len(s)
    if done: ms.dummyMatch(s.len)
    for mi in ms:
      fillMatchImpl(m, mi, ms, sep)
      last = ab.a
      if ab.a > 0 or ab.a <= ab.b:  # skip first empty match
        result.add substr(s, first, last-1)
        for g in 0 ..< m.groupsCount:
          for sl in m.group(g):
            result.add substr(s, sl.a, sl.b)
      first = ab.b+1

func startsWith*(
  s: string, pattern: Regex, start = 0
): bool {.raises: [], deprecated: "use startsWith(string, Regex2) instead".} =
  debugCheckUtf8 s
  startsWithImpl(s, pattern, start)

template runeIncAt(s: string, n: var int) =
  ## increment ``n`` up to
  ## next rune's index
  if n < s.len:
    inc(n, runeLenAt(s, n))
  else:
    n = s.len+1

func endsWith*(s: string, pattern: Regex): bool {.raises: [], deprecated: "use endsWith(string, Regex2) instead".} =
  debugCheckUtf8 s
  result = false
  var
    m = RegexMatch()
    i = 0
  while i < s.len:
    result = match(s, pattern, m, i)
    if result: return
    s.runeIncAt(i)

func flatCaptures(
  result: var seq[string],
  m: RegexMatch,
  s: string
) {.inline, raises: [].} =
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

func addsubstr(
  result: var string, s: string, first, last: int
) {.inline, raises: [].} =
  let
    first = max(first, 0)
    last = min(last, s.high)
  if first > last: return
  let n = result.len
  result.setLen(result.len + last-first+1)
  # XXX copyMem
  var j = 0
  for i in first .. last:
    result[n + j] = s[i]
    inc j

func addsubstr(
  result: var string, s: string, first: int
) {.inline, raises: [].} =
  addsubstr(result, s, first, s.high)

func replace*(
  s: string,
  pattern: Regex,
  by: string,
  limit = 0
): string {.raises: [ValueError], deprecated: "use replace(string, Regex2, string) instead".} =
  debugCheckUtf8 s
  result = ""
  var
    i, j = 0
    capts = newSeq[string](pattern.groupsCount)
  for m in findAll(s, pattern):
    result.addsubstr(s, i, m.boundaries.a-1)
    flatCaptures(capts, m, s)
    if capts.len > 0:
      result.addf(by, capts)
    else:
      result.add(by)
    i = m.boundaries.b+1
    inc j
    if limit > 0 and j == limit: break
  result.addsubstr(s, i)

when not defined(nimHasEffectsOf):
  {.pragma: effectsOf.}

func replace*(
  s: string,
  pattern: Regex,
  by: proc (m: RegexMatch, s: string): string,
  limit = 0
): string {.raises: [], effectsOf: by, deprecated: "use replace(string, Regex2, proc(RegexMatch2, string): string) instead".} =
  debugCheckUtf8 s
  result = ""
  var i, j = 0
  for m in findAll(s, pattern):
    result.addsubstr(s, i, m.boundaries.a-1)
    result.add by(m, s)
    i = m.boundaries.b+1
    inc j
    if limit > 0 and j == limit: break
  result.addsubstr(s, i)

func isInitialized*(re: Regex): bool {.inline, raises: [], deprecated: "use isInitialized(Regex2) instead".} =
  re.nfa.s.len > 0

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
  let n = pattern.nfa.s[nIdx]
  result = "["
  result.add($n)
  for nn in n.next:
    if isEpsilonTransition(pattern.nfa.s[nn]):
      continue
    result.add(", ")
    result.add(pattern.toString(nn, visited))
  result.add("]")

proc toString(pattern: Regex): string {.used.} =
  ## NFA to string representation.
  ## For debugging purposes
  var visited: set[int16] = {}
  result = pattern.toString(0, visited)

{.pop.}  # {.push warning[Deprecated]: off.}
