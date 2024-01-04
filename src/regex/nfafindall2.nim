## Linear time NFA findAll

#[
The nfamatch find algorithm can be used
to search all matches within a string,
however doing this may take quadratic time
in some cases. Ex: findAll("aaaa", re"\w+\d|\w")
this will call find 4 times and consume the
entire string every time.

The way to avoid this is to store all possible
matches until we know whether they are valid
and should be returned or they must be replaced by an overlapping
match taking priority according to PCRE rules
(longest-left match wins).

This algorithm works the same way as calling
nfamatch find repeatedly, except it keeps
all possible matches and returns them as soon
as the current character cannot match the regex,
i.e: it's a safe point to return. This is just
to avoid consuming too much memory if possible.

The downside is it takes linear space in the length
of the text to match + the regex. In practice it almost always
takes little space, since the matches are index ranges.

The tricky part is to replace all overlapped
temporary matches every time an Eoe is found,
then prune the next states (as they're overlapped),
then try to match the initial state to the
current character (next possible match). Other than
that is the same algorithm as nfamatch.

The "literals optimization" is also implemented here,
see https://nitely.github.io/2020/11/30/regex-literals-optimization.html
for the algorithm description
]#

import std/unicode
import std/tables
from std/strutils import find

import ./common
import ./nodematch
import ./types
import ./nfatype
import ./nfamatch2

type
  MatchItemIdx = int
  MatchItem = tuple
    capt: CaptIdx
    bounds: Bounds
  Matches = seq[MatchItem]
  RegexMatches2* = object
    a, b: Submatches
    m: Matches
    c: Capts3
    look: Lookaround

template initMaybeImpl(
  ms: var RegexMatches2,
  size, groupsLen: int
) =
  if ms.a == nil:
    assert ms.b == nil
    ms.a = newSubmatches size
    ms.b = newSubmatches size
    ms.c = initCapts3 groupsLen
    ms.look = initLook()
  doAssert ms.a.cap >= size and
    ms.b.cap >= size

template initMaybeImpl(
  ms: var RegexMatches2,
  regex: Regex
) =
  initMaybeImpl(ms, regex.nfa.s.len, regex.groupsCount)

func add(ms: var RegexMatches2, m: MatchItem) {.inline.} =
  ## Add `m` to `ms.m`. Remove all overlapped matches.
  template msm: untyped = ms.m
  template capts: untyped = ms.c
  var size = 0
  for i in countdown(msm.len-1, 0):
    if max(msm[i].bounds.b, msm[i].bounds.a) < m.bounds.a:
      size = i+1
      break
  #for i in size .. msm.len-1:
    if msm[i].capt != -1:
      capts.recyclable msm[i].capt
  msm.setLen size
  msm.add m
  if m.capt != -1:
    capts.notRecyclable m.capt

func hasMatches(ms: RegexMatches2): bool {.inline.} =
  return ms.m.len > 0

func clear(ms: var RegexMatches2) {.inline.} =
  ms.a.clear()
  ms.b.clear()
  ms.m.setLen 0
  ms.c.clear()

iterator bounds*(ms: RegexMatches2): Slice[int] {.inline.} =
  for i in 0 .. ms.m.len-1:
    yield ms.m[i].bounds

iterator items*(ms: RegexMatches2): MatchItemIdx {.inline.} =
  for i in 0 .. ms.m.len-1:
    yield i

func fillMatchImpl*(
  m: var RegexMatch2,
  mi: MatchItemIdx,
  ms: RegexMatches2,
  regex: Regex
) {.inline.} =
  template capt: untyped = ms.m[mi].capt
  if m.namedGroups.len != regex.namedGroups.len:
    m.namedGroups = regex.namedGroups
  m.captures.setLen regex.groupsCount
  if capt != -1:
    for i in 0 .. m.captures.len-1:
      m.captures[i] = ms.c[capt, i]
  else:
    for i in 0 .. m.captures.len-1:
      m.captures[i] = nonCapture
  m.boundaries = ms.m[mi].bounds

func dummyMatch*(ms: var RegexMatches2, i: int) {.inline.} =
  ## hack to support `split` last value.
  ## we need to add the end boundary if
  ## it has not matched the end
  ## (no match implies this too)
  template ab: untyped = ms.m[^1].bounds
  if ms.m.len == 0 or max(ab.a, ab.b) < i:
    ms.add (-1'i32, i+1 .. i)

func submatch(
  ms: var RegexMatches2,
  text: string,
  regex: Regex,
  i: int,
  cPrev, c: int32,
  flags: MatchFlags
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
  var captx: int32
  var matched = true
  var eoeFound = false
  var smi = 0
  while smi < smA.len:
    if capt != -1:
      capts.keepAlive capt
    var nti = 0
    while nti <= nfa[n].next.len-1:
      matched = true
      captx = capt
      while isEpsilonTransition(ntn):
        if matched:
          case ntn.kind
          of reGroupStart:
            captx = capts.diverge captx
            capts[captx, ntn.idx].a = i
          of reGroupEnd:
            captx = capts.diverge captx
            capts[captx, ntn.idx].b = i-1
          of assertionKind - lookaroundKind:
            matched = match(ntn, cPrev.Rune, c.Rune)
          of lookaroundKind:
            let freezed = capts.freeze()
            lookAroundTpl()
            capts.unfreeze freezed
            if captx != -1:
              capts.keepAlive captx
          else:
            doAssert false
            discard
        inc nti
      if matched and
          not smB.hasState(nt) and
          (ntn.match(c.Rune) or ntn.kind == reEoe):
        if ntn.kind == reEoe:
          #debugEcho "eoe ", bounds, " ", ms.m
          ms.add (captx, bounds.a .. i-1)
          smA.clear()
          if not eoeFound:
            eoeFound = true
            smA.add (0'i16, -1'i32, i .. i-1)
          smi = -1
          break
        smB.add (nt, captx, bounds.a .. i-1)
      inc nti
    inc smi
  swap smA, smB
  capts.recycle()

func findSomeImpl*(
  text: string,
  regex: Regex,
  ms: var RegexMatches2,
  start: Natural = 0,
  flags: MatchFlags = {}
): int =
  template smA: untyped = ms.a
  initMaybeImpl(ms, regex)
  ms.clear()
  var
    c = Rune(-1)
    cPrev = -1'i32
    i = start.int
    iPrev = start.int
  let
    flags = regex.flags.toMatchFlags + flags
    optFlag = mfFindMatchOpt in flags
    binFlag = mfBytesInput in flags
  smA.add (0'i16, -1'i32, i .. i-1)
  if start-1 in 0 .. text.len-1:
    cPrev = if binFlag:
      text[start-1].int32
    else:
      bwRuneAt(text, start-1).int32
  while i < text.len:
    if binFlag:
      c = text[i].Rune
      inc i
    else:
      fastRuneAt(text, i, c, true)
    submatch(ms, text, regex, iPrev, cPrev, c.int32, flags)
    if smA.len == 0:
      # avoid returning right before final zero-match
      if i < len(text):
        if ms.hasMatches():
          #debugEcho "m= ", ms.m
          #debugEcho "sma=0=", i
          return i
        if optFlag:
          return i
    smA.add (0'i16, -1'i32, i .. i-1)
    iPrev = i
    cPrev = c.int32
  submatch(ms, text, regex, iPrev, cPrev, -1'i32, flags)
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
  let flags = regex.flags.toMatchFlags + {mfFindMatchOpt}
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
    i = reversedMatchImpl(smA, smB, text, opt.nfa, look, groupsLen, i, limit, flags)
    if i == -1:
      #debugEcho "not.Match=", i
      i = litIdx+step
    else:
      doAssert i <= litIdx
      #debugEcho "bounds.a=", i
      i = findSomeImpl(text, regex, ms, i, flags)
      #debugEcho "bounds.b=", i
      if ms.hasMatches:
        return i
      if i == -1:
        return -1
  return -1
