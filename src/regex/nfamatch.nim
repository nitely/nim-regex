## NFA matcher for non-static regexes

# Pro-tip: try modifications here and
# run tests passing `-d:forceRegexAtRuntime`.
# Then port the changes to nfamacro. Hacking
# with nfamatch is a lot easier.

import unicode
import tables

import nodematch
import nodetype
import nfatype

template findMatchBailOut: untyped {.dirty.} =
  if nfa[n].kind == reEoe:
    if not smB.hasState n:
      smB.add (n, capt, bounds)
    # first Eoe in SmA wins, it's pointless to
    # keep matching further than the last Eoe
    break

template notEoe: untyped {.dirty.} =
  when mfFindMatch in flags:
    nfa[nt].kind != reEoe
  else:
    true

func submatch(
  smA, smB: var Submatches,
  capts: var Capts,
  regex: Regex,
  i: int,
  cPrev, c, c2: int32,
  flags: static MatchFlags,
) {.inline.} =
  template t: untyped = regex.transitions
  template nfa: untyped = regex.nfa
  smB.clear()
  var captx: int32
  var matched = true
  for n, capt, bounds in smA.items:
    when mfFindMatch in flags:
      findMatchBailOut()
    for nti, nt in nfa[n].next.pairs:
      if smB.hasState nt:
        continue
      if notEoe() and not match(nfa[nt], c2.Rune):
        continue
      if t.allZ[n][nti] == -1'i16:
        smB.add (nt, capt, bounds.a .. i-1)
        continue
      matched = true
      captx = capt
      for z in t.z[t.allZ[n][nti]]:
        if not matched:
          break
        case z.kind
        of groupKind:
          capts.add CaptNode(
            parent: captx,
            bound: i,
            idx: z.idx)
          captx = (capts.len-1).int32
        of assertionKind:
          matched = match(z, cPrev.Rune, c.Rune)
        else:
          assert false
          discard
      if matched:
        smB.add (nt, captx, bounds.a .. i-1)
  swap smA, smB

template shortestMatch: untyped {.dirty.} =
  submatch(smA, smB, capts, regex, iPrev, cPrev, c.int32, -1'i32, flags)
  if smA.len > 0:
    return true
  swap smA, smB

template findMatch: untyped {.dirty.} =
  smA.add (0'i16, -1'i32, i .. i-1)
  if regex.nfa[smA[0].ni].kind == reEoe:
    constructSubmatches(m.captures, capts, smA[0].ci, regex.groupsCount)
    if regex.namedGroups.len > 0:
      m.namedGroups = regex.namedGroups
    m.boundaries = smA[0].bounds
    return true

func bwRuneAt(s: string, n: int): Rune =
  ## Take rune ending at ``n``
  doAssert n >= 0
  doAssert n <= s.len-1
  var n = n
  while n > 0 and s[n].ord shr 6 == 0b10:
    dec n
  fastRuneAt(s, n, result, false)

func matchImpl*(
  text: string,
  regex: Regex,
  m: var RegexMatch,
  flags: static MatchFlags,
  start = 0
): bool {.inline.} =
  m.clear()
  var
    smA, smB: Submatches
    capts: Capts
    c = Rune(-1)
    cPrev = -1'i32
    i = start
    iPrev = start
  smA = newSubmatches(regex.nfa.len)
  smB = newSubmatches(regex.nfa.len)
  smA.add (0'i16, -1'i32, start .. start-1)
  when mfFindMatch in flags:
    if 0 <= start-1 and start-1 <= len(text)-1:
      cPrev = bwRuneAt(text, start-1).int32
  while i < len(text):
    fastRuneAt(text, i, c, true)
    when mfShortestMatch in flags:
      shortestMatch()
    submatch(smA, smB, capts, regex, iPrev, cPrev, c.int32, c.int32, flags)
    when mfFindMatch in flags:
      findMatch()
    if smA.len == 0:
      return false
    iPrev = i
    cPrev = c.int32
  submatch(smA, smB, capts, regex, iPrev, cPrev, -1'i32, -1'i32, flags)
  if smA.len == 0:
    return false
  constructSubmatches(m.captures, capts, smA[0].ci, regex.groupsCount)
  if regex.namedGroups.len > 0:
    m.namedGroups = regex.namedGroups
  m.boundaries = smA[0].bounds
  return true

type
  MatchItemIdx = int
  MatchItem = tuple
    capt: CaptIdx
    bounds: Bounds
  Matches = seq[MatchItem]
  RegexMatches* = object
    a, b: Submatches
    m: Matches
    c: Capts

# XXX make it log(n)?
func add(ms: var Matches, m: MatchItem) {.inline.} =
  var size = 0
  for i in countdown(ms.len-1, 0):
    if max(ms[i].bounds.b, ms[i].bounds.a) < m.bounds.a:
      size = i+1
      break
  ms.setLen size
  system.add(ms, m)

func clear(ms: var Matches) {.inline.} =
  ms.setLen 0

template initMaybeImpl(
  ms: var RegexMatches,
  regex: Regex
) =
  if ms.a == nil:
    ms = RegexMatches(
      a: newSubmatches(regex.nfa.len),
      b: newSubmatches(regex.nfa.len))
  doAssert ms.a.cap == regex.nfa.len and
    ms.b.cap == regex.nfa.len

func hasMatches(ms: RegexMatches): bool {.inline.} =
  return ms.m.len > 0

func clear(ms: var RegexMatches) {.inline.} =
  ms.a.clear()
  ms.b.clear()
  ms.m.clear()
  ms.c.setLen 0

iterator items*(ms: RegexMatches): MatchItemIdx {.inline.} =
  for i in 0 .. ms.m.len-1:
    yield i

func fillMatchImpl*(
  m: var RegexMatch,
  mi: MatchItemIdx,
  ms: RegexMatches,
  regex: Regex
) {.inline.} =
  if m.namedGroups.len != regex.namedGroups.len:
    m.namedGroups = regex.namedGroups
  constructSubmatches(
    m.captures, ms.c, ms.m[mi].capt, regex.groupsCount)
  m.boundaries = ms.m[mi].bounds

func submatch(
  ms: var RegexMatches,
  regex: Regex,
  i: int,
  cPrev, c: int32
) {.inline.} =
  template tns: untyped = regex.transitions
  template nfa: untyped = regex.nfa
  template smA: untyped = ms.a
  template smB: untyped = ms.b
  template capts: untyped = ms.c
  template n: untyped = ms.a[smi].ni
  template capt: untyped = ms.a[smi].ci
  template bounds: untyped = ms.a[smi].bounds
  smB.clear()
  var captx: int32
  var matched = true
  var eoeFound = false
  var smi = 0
  while smi < smA.len:
    for nti, nt in nfa[n].next.pairs:
      if smB.hasState nt:
        continue
      if nfa[nt].kind != reEoe and not match(nfa[nt], c.Rune):
        continue
      matched = true
      captx = capt
      if tns.allZ[n][nti] > -1:
        for z in tns.z[tns.allZ[n][nti]]:
          if not matched:
            break
          case z.kind
          of groupKind:
            capts.add CaptNode(
              parent: captx,
              bound: i,
              idx: z.idx)
            captx = (capts.len-1).int32
          of assertionKind:
            matched = match(z, cPrev.Rune, c.Rune)
          else:
            assert false
            discard
      if matched:
        if nfa[nt].kind == reEoe:
          #debugEcho "eoe ", bounds, " ", ms.m
          ms.m.add (captx, bounds.a .. i-1)
          smA.clear()
          if not eoeFound:
            eoeFound = true
            smA.add (0'i16, -1'i32, i .. i-1)
          smi = -1
          break
        smB.add (nt, captx, bounds.a .. i-1)
    inc smi
  swap smA, smB

func findSomeImpl*(
  text: string,
  regex: Regex,
  ms: var RegexMatches,
  start = 0
): int =
  template smA: untyped = ms.a
  initMaybeImpl(ms, regex)
  ms.clear()
  var
    c = Rune(-1)
    cPrev = -1'i32
    i = start
    iPrev = start
  smA.add (0'i16, -1'i32, start .. start-1)
  if 0 <= start-1 and start-1 <= len(text)-1:
    cPrev = bwRuneAt(text, start-1).int32
  while i < len(text):
    #debugEcho "it= ", i, " ", cPrev
    fastRuneAt(text, i, c, true)
    submatch(ms, regex, iPrev, cPrev, c.int32)
    when false:  # early return
      if smA.len == 0:
        debugEcho "smA 0"
        if ms.hasMatches() and i < len(text):
          #debugEcho "m= ", ms.m.s
          #debugEcho "sma=0=", i
          return i
    smA.add (0'i16, -1'i32, i .. i-1)
    iPrev = i
    cPrev = c.int32
  submatch(ms, regex, iPrev, cPrev, -1'i32)
  doAssert smA.len == 0
  if ms.hasMatches():
    #debugEcho "m= ", ms.m.s
    return i
  #debugEcho "noMatch"
  return -1
