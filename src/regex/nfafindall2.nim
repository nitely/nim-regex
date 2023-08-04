import std/unicode
import std/tables
from std/strutils import find

import ./common
import ./nodematch
import ./types
import ./nfatype
import ./nfamatch2

# XXX remove
const nonCapture = -1 .. -2

type
  MatchItemIdx = int
  MatchItem = tuple
    capt: CaptIdx
    bounds: Bounds
  Matches = object
    s: seq[MatchItem]
    i: int
  RegexMatches2* = object
    a, b: Submatches
    m: Matches
    c: Capts2
    look: Lookaround

func len(ms: Matches): int {.inline.} =
  ms.i

# XXX make it log(n)? altough this does
#     not add to the time complexity
func add(ms: var Matches, m: MatchItem) {.inline.} =
  ## Add `m` to `ms`. Remove all overlapped matches.
  var size = 0
  for i in countdown(ms.len-1, 0):
    if max(ms.s[i].bounds.b, ms.s[i].bounds.a) < m.bounds.a:
      size = i+1
      break
  ms.i = size
  if ms.i <= ms.s.len-1:
    ms.s[ms.i] = m
  else:
    ms.s.add m
  ms.i += 1

func clear(ms: var Matches) {.inline.} =
  ms.i = 0

template initMaybeImpl(
  ms: var RegexMatches2,
  size, groupsLen: int
) =
  if ms.a == nil:
    assert ms.b == nil
    ms.a = newSubmatches size
    ms.b = newSubmatches size
    ms.c = initCapts2 groupsLen
    ms.look = initLook()
  doAssert ms.a.cap >= size and
    ms.b.cap >= size

template initMaybeImpl(
  ms: var RegexMatches2,
  regex: Regex
) =
  initMaybeImpl(ms, regex.nfa.s.len, regex.groupsCount)

func hasMatches(ms: RegexMatches2): bool {.inline.} =
  return ms.m.len > 0

func clear(ms: var RegexMatches2) {.inline.} =
  ms.a.clear()
  ms.b.clear()
  ms.m.clear()
  ms.c.clear()

iterator bounds*(ms: RegexMatches2): Slice[int] {.inline.} =
  for i in 0 .. ms.m.len-1:
    yield ms.m.s[i].bounds

iterator items*(ms: RegexMatches2): MatchItemIdx {.inline.} =
  for i in 0 .. ms.m.len-1:
    yield i

func fillMatchImpl*(
  m: var RegexMatch2,
  mi: MatchItemIdx,
  ms: RegexMatches2,
  regex: Regex
) {.inline.} =
  if m.namedGroups.len != regex.namedGroups.len:
    m.namedGroups = regex.namedGroups
  if ms.m.s[mi].capt >= 0:
    m.captures.setLen 0
    m.captures.add ms.c[ms.m.s[mi].capt]
  else:
    m.captures.setLen regex.groupsCount
    for i in 0 .. m.captures.len-1:
      m.captures[i] = nonCapture
  m.boundaries = ms.m.s[mi].bounds

func dummyMatch*(ms: var RegexMatches2, i: int) {.inline.} =
  ## hack to support `split` last value.
  ## we need to add the end boundary if
  ## it has not matched the end
  ## (no match implies this too)
  template ab: untyped = ms.m.s[^1].bounds
  if ms.m.len == 0 or max(ab.a, ab.b) < i:
    ms.m.add (-1'i32, i+1 .. i)

func submatch(
  ms: var RegexMatches2,
  text: string,
  regex: Regex,
  i: int,
  cPrev, c: int32
) {.inline.} =
  template tns: untyped = regex.nfa.t
  template nfa: untyped = regex.nfa.s
  template smA: untyped = ms.a
  template smB: untyped = ms.b
  template capts: untyped = ms.c
  template n: untyped = ms.a[smi].ni
  template capt: untyped = ms.a[smi].ci
  template bounds: untyped = ms.a[smi].bounds
  template look: untyped = ms.look
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
            if captx < 0:
              capts.inc()
              captx = (capts.len-1).int32
            else:
              capts.add capts[captx]
              captx = (capts.len-1).int32
            if z.kind == reGroupStart:
              capts[captx][z.idx].a = i  # XXX fix
            else:
              capts[captx][z.idx].b = i-1
          of assertionKind - lookaroundKind:
            matched = match(z, cPrev.Rune, c.Rune)
          of lookaroundKind:
            lookAroundTpl()
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
  ms: var RegexMatches2,
  start: Natural = 0,
  flags: set[MatchFlag] = {}
): int =
  template smA: untyped = ms.a
  initMaybeImpl(ms, regex)
  ms.clear()
  var
    c = Rune(-1)
    cPrev = -1'i32
    i = start.int
    iPrev = start.int
    optFlag = mfFindMatchOpt in flags
  smA.add (0'i16, -1'i32, i .. i-1)
  if start-1 in 0 .. text.len-1:
    cPrev = bwRuneAt(text, start-1).int32
  while i < text.len:
    #debugEcho "it= ", i, " ", cPrev
    fastRuneAt(text, i, c, true)
    #c = text[i].Rune
    #i += 1
    submatch(ms, text, regex, iPrev, cPrev, c.int32)
    if smA.len == 0:
      # avoid returning right before final zero-match
      if i < len(text):
        if ms.hasMatches():
          #debugEcho "m= ", ms.m
          #debugEcho "sma=0=", i
          return i
        # else:  # XXX clear captures
        if optFlag:
          return i
    smA.add (0'i16, -1'i32, i .. i-1)
    iPrev = i
    cPrev = c.int32
  submatch(ms, text, regex, iPrev, cPrev, -1'i32)
  doAssert smA.len == 0
  if ms.hasMatches():
    #debugEcho "m= ", ms.m.s
    return i
  #debugEcho "noMatch"
  return -1

# findAll with literal optimization below,
# there is an explanation of how this work
# in litopt.nim

func findSomeOptImpl*(
  text: string,
  regex: Regex,
  ms: var RegexMatches2,
  start: Natural
): int =
  template regexSize: untyped =
    max(regex.litOpt.nfa.s.len, regex.nfa.s.len)
  template opt: untyped = regex.litOpt
  template groupsLen: untyped = regex.groupsCount
  template smA: untyped = ms.a
  template smB: untyped = ms.b
  template look: untyped = ms.look
  doAssert opt.nfa.s.len > 0
  initMaybeImpl(ms, regexSize, groupsLen)
  ms.clear()
  var limit = start.int
  var i = start.int
  var i2 = -1
  while i < len(text):
    doAssert i > i2; i2 = i
    #debugEcho "lit=", opt.lit
    #debugEcho "i=", i
    let litIdx = text.find(opt.lit.char, i)
    if litIdx == -1:
      return -1
    #debugEcho "litIdx=", litIdx
    doAssert litIdx >= i
    i = litIdx
    i = reversedMatchImpl(smA, smB, text, opt.nfa, look, groupsLen, i, limit)
    if i == -1:
      #debugEcho "not.Match=", i
      i = litIdx+1
    else:
      doAssert i <= litIdx
      #debugEcho "bounds.a=", i
      i = findSomeImpl(text, regex, ms, i, {mfFindMatchOpt})
      #debugEcho "bounds.b=", i
      if ms.hasMatches:
        return i
      if i == -1:
        return -1
  return -1
