## Types used by the NFA

import std/tables
import std/sets
import std/algorithm

import ./types
import ./litopt

type
  CaptIdx* = int32
  CaptNode* = object
    parent*: CaptIdx
    bound*: int
    idx*: int16  # XXX rename to group or groupNum
  Capts* = seq[CaptNode]
  Captures* = seq[seq[Slice[int]]]

func constructSubmatches*(
  captures: var Captures,
  capts: Capts,
  capt, size: int
) {.inline.} =
  template currGroup: untyped = captures[capts[capt].idx]
  captures.setLen size
  for i in 0 .. captures.len-1:
    captures[i].setLen 0
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

func reverse*(capts: var Capts, a, b: int32): int32 =
  ## reverse capture indices from a to b; return head
  doAssert a >= b
  var capt = a
  var parent = b
  while capt != b:
    let p = capts[capt].parent
    capts[capt].parent = parent
    parent = capt
    capt = p
  return parent

type
  RegexLit* = distinct string
    ## raw regex literal string
  Regex* = object
    ## a compiled regular expression
    nfa*: Nfa
    groupsCount*: int16
    namedGroups*: OrderedTable[string, int16]
    #flags*: set[RegexFlag]
    litOpt*: LitOpt
  MatchFlag* = enum
    mfShortestMatch
    mfNoCaptures
    mfFindMatch
    mfFindMatchOpt
    mfAnchored
    mfBwMatch
    mfReverseCapts
  MatchFlags* = set[MatchFlag]
  RegexMatch* = object
    ## result from matching operations
    captures*: Captures
    namedGroups*: OrderedTable[string, int16]
    boundaries*: Slice[int]

func clear*(m: var RegexMatch) {.inline.} =
  if m.captures.len > 0:
    m.captures.setLen(0)
  if m.namedGroups.len > 0:
    m.namedGroups.clear()
  m.boundaries = 0 .. -1

type
  NodeIdx* = int16
  Bounds* = Slice[int]
  PState* = tuple
    ni: NodeIdx
    ci: CaptIdx
    bounds: Bounds
  Submatches* = ref object
    ## Parallel states would be a better name.
    ## This is a sparse set
    sx: seq[PState]
    ss: seq[int16]
    si: int16

func newSubmatches*(size: int): Submatches {.inline.} =
  result = new Submatches
  result.sx = newSeq[PState](8)
  result.ss = newSeq[int16](size)
  result.si = 0

when defined(release):
  {.push checks: off.}

func `[]`*(sm: Submatches, i: int): PState {.inline.} =
  assert i < sm.si
  sm.sx[i]

func hasState*(sm: Submatches, n: int16): bool {.inline.} =
  sm.ss[n] < sm.si and sm.sx[sm.ss[n]].ni == n

func add*(sm: var Submatches, item: PState) {.inline.} =
  assert(not sm.hasState(item.ni))
  assert sm.si <= sm.sx.len
  if (sm.si == sm.sx.len).unlikely:
    sm.sx.setLen(sm.sx.len * 2)
  sm.sx[sm.si] = item
  sm.ss[item.ni] = sm.si
  sm.si += 1'i16

func len*(sm: Submatches): int {.inline.} =
  sm.si

func clear*(sm: var Submatches) {.inline.} =
  sm.si = 0

iterator items*(sm: Submatches): PState {.inline.} =
  for i in 0 .. sm.len-1:
    yield sm.sx[i]

# does not work in Nim <= 0.20
#iterator mitems*(sm: Submatches): var PState {.inline.} =
#  for i in 0 .. sm.len-1:
#    yield sm.sx[i]

func cap*(sm: Submatches): int {.inline.} =
  sm.ss.len

func setLen*(sm: var Submatches, size: int) {.inline.} =
  sm.ss.setLen size

when defined(release):
  {.pop.}

# XXX maybe store the lookaround number + count, and use a fixed
#     size seq to reduce allocations
type
  SmLookaroundItem* = object
    a, b: Submatches
  SmLookaround* = object
    s: seq[SmLookaroundItem]
    i: int

func setLen*(item: var SmLookaroundItem, size: int) {.inline.} =
  if item.a == nil:
    doAssert item.b == nil
    item.a = newSubmatches size
    item.b = newSubmatches size
  else:
    doAssert item.b != nil
    item.a.setLen size
    item.b.setLen size

template last*(sm: var SmLookaround): untyped =
  sm.s[sm.i-1]

template lastA*(sm: var SmLookaround): untyped =
  last(sm).a

template lastB*(sm: var SmLookaround): untyped =
  last(sm).b

func grow*(sm: var SmLookaround) {.inline.} =
  doAssert sm.i <= sm.s.len
  if sm.i == sm.s.len:
    sm.s.setLen(max(1, sm.s.len) * 2)
  sm.i += 1

func removeLast*(sm: var SmLookaround) {.inline.} =
  doAssert sm.i > 0
  sm.i -= 1
