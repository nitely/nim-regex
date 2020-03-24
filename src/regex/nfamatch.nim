## NFA matcher for non-static regexes

import unicode
import sets
import tables
import deques
import algorithm

import unicodeplus except isUpper, isLower

import nodematch
import nodetype
import nfa

type
  CaptNode* = object
    parent*: int
    bound*: int
    idx*: int
  Capts* = seq[CaptNode]
  Captures* = seq[seq[Slice[int]]]

func constructSubmatches*(
  captures: var Captures,
  capts: Capts,
  capt, size: int
) {.inline.} =
  template currGroup: untyped = captures[capts[capt].idx]
  captures.setLen(size)
  for i in 0 .. captures.len-1:
    captures[i].setLen(0)
  if capts.len == 0:
    return
  var capt = capt
  while capt != -1:
    if currGroup.len == 0:
      currGroup.add(-2 .. -2)
    if currGroup[^1].a != -2:
      currGroup.add(-2 .. -2)
    if currGroup[^1].b == -2:
      currGroup[^1].b = capts[capt].bound-1
    else:
      currGroup[^1].a = capts[capt].bound
    capt = capts[capt].parent
  for c in captures.mitems:
    c.reverse()

type
  NodeIdx = int16
  CaptIdx = int32
  Submatches* = ref object
    ## Parallel states would be a better name
    sx: seq[(NodeIdx, CaptIdx)]
    # use custom len because setLen(0) is slower,
    # and {.noInit.} makes no difference
    si: int
    ss: set[int16]

func newSubmatches*(): Submatches {.inline.} =
  result = new Submatches
  result.sx = newSeq[(NodeIdx, CaptIdx)](8)
  result.si = 0

func `[]`*(sm: Submatches, i: int): (NodeIdx, CaptIdx) {.inline.} =
  assert i < sm.si
  sm.sx[i]

func add*(sm: var Submatches, item: (NodeIdx, CaptIdx)) {.inline.} =
  assert item[0] notin sm.ss
  assert sm.si <= sm.sx.len
  if (sm.si == sm.sx.len).unlikely:
    sm.sx.setLen(sm.sx.len * 2)
  sm.sx[sm.si] = item
  sm.si += 1 
  sm.ss.incl(item[0])

func len*(sm: Submatches): int {.inline.} =
  sm.si

func hasState*(sm: Submatches, n: int16): bool {.inline.} =
  n in sm.ss

func clear*(sm: var Submatches) {.inline.} =
  for i in 0 .. sm.len-1:
    assert sm.sx[i][0] in sm.ss
    sm.ss.excl sm.sx[i][0]
  sm.si = 0

iterator items*(sm: Submatches): (NodeIdx, CaptIdx) {.inline.} =
  for i in 0 .. sm.len-1:
    yield sm.sx[i]

type
  RegexFlag* = enum
    reAscii
  Regex* = object
    ## a compiled regular expression
    nfa*: Nfa
    transitions*: Transitions
    groupsCount*: int16
    namedGroups*: OrderedTable[string, int16]
    flags*: set[RegexFlag]
  MatchFlag* = enum
    mfShortestMatch
    mfLongestMatch
    mfNoCaptures
    mfFindMatch
  MatchFlags* = set[MatchFlag]
  RegexMatch* = object
    ## result from matching operations
    captures*: Captures
    namedGroups*: OrderedTable[string, int16]
    boundaries*: Slice[int]

func submatch(
  smA, smB: var Submatches,
  capts: var Capts,
  regex: Regex,
  i: int,
  cPrev, c, c2: int32
) {.inline.} =
  template t: untyped {.dirty.} = regex.transitions
  template nfa: untyped {.dirty.} = regex.nfa
  smB.clear()
  var captx: int32
  var matched = true
  for n, capt in smA.items:
    for nti, nt in nfa[n].next.pairs:
      if smB.hasState(nt):
        continue
      if not match(nfa[nt], c2.Rune):
        continue
      if t.allZ[n][nti] == -1'i16:
        smB.add((nt, capt))
        continue
      matched = true
      captx = capt
      for z in t.z[t.allZ[n][nti]]:
        if not matched:
          break
        case z.kind
        of groupKind:
          capts.add(CaptNode(
            parent: captx,
            bound: i,
            idx: z.idx))
          captx = (capts.len-1'i32).int32
        of assertionKind:
          matched = match(z, cPrev.Rune, c.Rune)
        else:
          assert false
          discard
      if matched:
        smB.add((nt, captx))
  swap smA, smB

func clear*(m: var RegexMatch) {.inline.} =
  if m.captures.len > 0:
    m.captures.setLen(0)
  if m.namedGroups.len > 0:
    m.namedGroups.clear()
  m.boundaries = 0 .. -1

template shortestMatch: untyped {.dirty.} =
  submatch(smA, smB, capts, regex, iPrev, cPrev, c.int32, -1'i32)
  if smA.len > 0:
    return true
  swap smA, smB

template longestMatchInit: untyped {.dirty.} =
  var
    matchedLong = false
    captLong = -1'i32
    iPrevLong = start

template longestMatchEnter: untyped {.dirty.} =
  submatch(smA, smB, capts, regex, iPrev, cPrev, c.int32, -1'i32)
  if smA.len > 0:
    matchedLong = true
    captLong = smA[0][1]
    iPrevLong = iPrev
  swap smA, smB

template longestMatchExit: untyped {.dirty.} =
  if not matchedLong:
    return false
  assert smA.len == 0
  constructSubmatches(m.captures, capts, captLong, regex.groupsCount)
  if regex.namedGroups.len > 0:
    m.namedGroups = regex.namedGroups
  m.boundaries = start .. iPrevLong-1
  return true

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
  when mfLongestMatch in flags:
    longestMatchInit()
  smA = newSubmatches()
  smB = newSubmatches()
  smA.add((0'i16, -1'i32))
  while i < len(text):
    fastRuneAt(text, i, c, true)
    when mfShortestMatch in flags:
      shortestMatch()
    when mfLongestMatch in flags:
      longestMatchEnter()
    submatch(smA, smB, capts, regex, iPrev, cPrev, c.int32, c.int32)
    if smA.len == 0:
      when mfLongestMatch in flags:
        longestMatchExit()
      return false
    iPrev = i
    cPrev = c.int32
  submatch(smA, smB, capts, regex, iPrev, cPrev, -1'i32, -1'i32)
  if smA.len == 0:
    when mfLongestMatch in flags:
      longestMatchExit()
    return false
  constructSubmatches(m.captures, capts, smA[0][1], regex.groupsCount)
  if regex.namedGroups.len > 0:
    m.namedGroups = regex.namedGroups
  m.boundaries = start .. iPrev-1
  return true

type
  PState = ref object
    ## Binary tree of DFA states
    s: seq[int16]
    next: seq[(int32, PState)]  # XXX use binary tree or a table
    left: PState
    right: PState

func newPState(s: seq[int16]): PState {.inline.} =
  result = new PState
  result.s = s

func `[]`(ps: PState, c: int32): PState {.inline.} =
  for i in 0 .. ps.next.len-1:
    if c == ps.next[i][0]:
      return ps.next[i][1]

func `<`(a, b: seq[int16]): bool {.inline.} =
  let ln = min(a.len, b.len)
  var i = 0
  while i < ln:
    if b[i] < a[i]:
      return false
    i += 1
  return a.len < b.len

func getOrAdd(ps: PState, s: var seq[int16]): PState {.inline.} =
  assert not ps.isNil
  s.sort()
  var ps = ps
  var ps2 = ps
  var i = 1
  while not ps.isNil:
    i = cmp(ps.s, s)
    if i < 0:
      ps2 = ps
      ps = ps.left
    elif i > 0:
      ps2 = ps
      ps = ps.right
    else:
      return ps
  result = newPState(s)
  if i < 0:
    ps2.left = result
  else:
    assert i > 0
    ps2.right = result

func nextStates(
  smA, smB: var seq[int16],
  smC: var set[int16],
  regex: Regex,
  i: int,
  c: int32
) {.inline.} =
  template nfa: untyped {.dirty.} = regex.nfa
  for n in smA:
    for nt in nfa[n].next:
      if nt in smC:
        continue
      if not match(nfa[nt], c.Rune):
        continue
      smB.add nt
      smC.incl nt
  for n in smB:
    smC.excl n
  swap smA, smB
  smB.setLen(0)

# benchmarks show this is as slow as matchImpl for
# short text, and 5x faster for long enough texts
# XXX constructs an unbounded DFA, do not use this for now
func fastMatchImpl*(
  text: string,
  regex: Regex,
  m: var RegexMatch,
  start = 0
): bool {.inline.} =
  m.clear()
  var
    smA, smB: seq[int16]
    smC: set[int16]
    c = Rune(-1)
    i = start
    iPrev = start
  smA.add 0'i16
  let t = newPState(smA)
  var q = t
  while i < len(text):
    fastRuneAt(text, i, c, true)
    let qn = q[c.int32]
    if not qn.isNil:
      q = qn
    else:
      smA.setLen(0)
      smA.add q.s
      nextStates(smA, smB, smC, regex, iPrev, c.int32)
      if smA.len == 0:
        return false
      let q2 = getOrAdd(t, smA)
      q.next.add (c.int32, q2)
      q = q2
    iPrev = i
  smA.setLen(0)
  smA.add q.s
  nextStates(smA, smB, smC, regex, iPrev, -1'i32)
  if smA.len == 0:
    return false
  m.boundaries = start .. iPrev-1
  return true
