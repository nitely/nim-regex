## DEPRECATED

import std/unicode
import std/tables
from std/strutils import find

import ./common
import ./nodematch
import ./types
import ./nfatype
import ./nfamatch

type
  MatchItemIdx = int
  MatchItem = tuple
    capt: CaptIdx
    bounds: Bounds
  Matches = object
    s: seq[MatchItem]
    i: int
  RegexMatches* = object
    a, b: Pstates
    m: Matches
    c: Capts
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

func initMaybeImpl(
  ms: var RegexMatches,
  size: int
) {.inline.} =
  ms.a.reset size
  ms.b.reset size
  ms.look = initLook()

func initMaybeImpl(
  ms: var RegexMatches,
  regex: Regex
) {.inline.} =
  initMaybeImpl(ms, regex.nfa.s.len)

func hasMatches(ms: RegexMatches): bool {.inline.} =
  return ms.m.len > 0

func clear(ms: var RegexMatches) {.inline.} =
  ms.a.clear()
  ms.b.clear()
  ms.m.clear()
  ms.c.setLen 0

iterator bounds*(ms: RegexMatches): Slice[int] {.inline.} =
  for i in 0 .. ms.m.len-1:
    yield ms.m.s[i].bounds

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
    m.captures, ms.c, ms.m.s[mi].capt, regex.groupsCount)
  m.boundaries = ms.m.s[mi].bounds

func dummyMatch*(ms: var RegexMatches, i: int) {.inline.} =
  ## hack to support `split` last value.
  ## we need to add the end boundary if
  ## it has not matched the end
  ## (no match implies this too)
  template ab: untyped = ms.m.s[^1].bounds
  if ms.m.len == 0 or max(ab.a, ab.b) < i:
    ms.m.add (-1.CaptIdx, i+1 .. i)

func submatch(
  ms: var RegexMatches,
  text: string,
  regex: Regex,
  i: int,
  cPrev, c: int32
) {.inline.} =
  template nfa: untyped = regex.nfa.s
  template smA: untyped = ms.a
  template smB: untyped = ms.b
  template capts: untyped = ms.c
  template n: untyped = ms.a[smi].ni
  template capt: untyped = ms.a[smi].ci
  template bounds: untyped = ms.a[smi].bounds
  template look: untyped = ms.look
  template nt: untyped = nfa[n].next[nti]
  template ntn: untyped = nfa[nt]
  smB.clear()
  var captx = 0.CaptIdx
  var matched = true
  var eoeFound = false
  var smi = 0
  while smi < smA.len:
    let L = nfa[n].next.len
    var nti = 0
    while nti < L:
      let isEoe = ntn.kind == reEoe
      let nt0 = nt
      matched = nt notin smB and
        (ntn.match(c.Rune) or ntn.kind == reEoe)
      inc nti
      captx = capt
      while nti < L and isEpsilonTransition(ntn):
        if matched:
          case ntn.kind
          of groupKind:
            capts.add CaptNode(
              parent: captx,
              bound: i,
              idx: ntn.idx)
            captx = (capts.len-1).CaptIdx
          of assertionKind - lookaroundKind:
            matched = match(ntn, cPrev.Rune, c.Rune)
          of lookaroundKind:
            lookAroundTpl()
          else:
            doAssert false
            discard
        inc nti
      if matched:
        if isEoe:
          #debugEcho "eoe ", bounds, " ", ms.m
          ms.m.add (captx, bounds.a .. i-1)
          smA.clear()
          if not eoeFound:
            eoeFound = true
            smA.add initPstate(0'i16, -1.CaptIdx, i .. i-1)
          smi = -1
          break
        smB.add initPstate(nt0, captx, bounds.a .. i-1)
    inc smi
  swap smA, smB

func findSomeImpl*(
  text: string,
  regex: Regex,
  ms: var RegexMatches,
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
  smA.add initPstate(0'i16, -1.CaptIdx, i .. i-1)
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
    smA.add initPstate(0'i16, -1.CaptIdx, i .. i-1)
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
# in litopt.nim, move this there?

func findSomeOptImpl*(
  text: string,
  regex: Regex,
  ms: var RegexMatches,
  start: Natural
): int =
  template regexSize: untyped =
    max(regex.litOpt.nfa.s.len, regex.nfa.s.len)
  template opt: untyped = regex.litOpt
  template smA: untyped = ms.a
  template smB: untyped = ms.b
  template look: untyped = ms.look
  doAssert opt.nfa.s.len > 0
  initMaybeImpl(ms, regexSize)
  ms.clear()
  let hasLits = opt.lits.len > 0
  let step = max(1, opt.lits.len)
  var limit = start.int
  var i = start.int
  var i2 = -1
  while i < len(text):
    doAssert i > i2; i2 = i
    #debugEcho "lit=", opt.lit
    #debugEcho "i=", i
    let litIdx = if hasLits:
      text.find(opt.lits, i)
    else:
      text.find(opt.lit.char, i)
    if litIdx == -1:
      return -1
    #debugEcho "litIdx=", litIdx
    doAssert litIdx >= i
    i = litIdx
    i = reversedMatchImpl(smA, smB, text, opt.nfa, look, i, limit)
    if i == -1:
      #debugEcho "not.Match=", i
      i = litIdx+step
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
