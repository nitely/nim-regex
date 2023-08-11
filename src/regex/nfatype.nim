## Types used by the NFA

import std/tables
import std/sets
import std/algorithm
import std/math
import std/bitops

import ./types
import ./litopt

const nonCapture* = -1 .. -2

type TouchOpt* = uint8
const
  touNo = 0.TouchOpt
  touYes = 1.TouchOpt
  touKeep = 2.TouchOpt
  touFrozen = 3.TouchOpt .. TouchOpt.high

type
  CaptIdx* = int32
  Capts3* = object
    ## Seq of captures divided into blocks
    ## of power of 2 len. One block per parallel state.
    ## A seq/set to keep track of used blocks.
    ## A seq of free blocks for reusing
    s: seq[Slice[int]]
    groupsLen: Natural
    blockSize: Natural
    blockSizeL2: Natural
    touched: seq[TouchOpt]
    free: seq[int16]
    freezeId: TouchOpt
    freezeCount: Natural

func len(capts: Capts3): int {.inline.} =
  capts.s.len shr capts.blockSizeL2  # s.len / blockSize

func `[]`*(capts: Capts3, i, j: Natural): Slice[int] {.inline.} =
  #doAssert i <= capts.len-1
  #doAssert j <= capts.groupsLen-1
  capts.s[(i shl capts.blockSizeL2) + j]  # i * blockSize

func `[]`*(capts: var Capts3, i, j: Natural): var Slice[int] {.inline.} =
  #doAssert i <= capts.len-1
  #doAssert j <= capts.groupsLen-1
  capts.s[(i shl capts.blockSizeL2) + j]  # i * blockSize

func `[]=`(capts: var Capts3, i, j: Natural, x: Slice[int]) {.inline.} =
  #doAssert i <= capts.len-1
  #doAssert j <= capts.groupsLen-1
  capts.s[(i shl capts.blockSizeL2) + j] = x

func initCapts3*(groupsLen: int): Capts3 =
  result.groupsLen = groupsLen
  result.blockSize = max(2, nextPowerOfTwo groupsLen)
  result.blockSizeL2 = fastLog2 result.blockSize

func touch*(capts: var Capts3, captIdx: CaptIdx) {.inline.} =
  capts.touched[captIdx] = touYes

func freeze*(capts: var Capts3): TouchOpt =
  ## Freeze all touched capts.
  ## Return freezeId
  doAssert capts.freezeId <= touFrozen.b
  if capts.freezeCount == 0:
    capts.freezeId = touFrozen.a
  result = capts.freezeId
  for i in 0 .. capts.touched.len-1:
    if capts.touched[i] == touYes:
      capts.touched[i] = result
      inc capts.freezeCount
  inc capts.freezeId

func unfreeze*(capts: var Capts3, freezeId: TouchOpt) =
  doAssert freezeId in touFrozen
  for i in 0 .. capts.touched.len-1:
    if capts.touched[i] == freezeId:
      capts.touched[i] = touYes
      dec capts.freezeCount

func diverge*(capts: var Capts3, captIdx: CaptIdx): CaptIdx =
  if capts.free.len > 0:
    result = capts.free.pop
    for i in 0 .. capts.blockSize-1:
      capts[result, i] = nonCapture
    capts.touched[result] = touYes
  else:
    result = capts.len.CaptIdx
    for _ in 0 .. capts.blockSize-1:
      capts.s.add nonCapture
    capts.touched.add touYes
    doAssert result == capts.touched.len-1
  if captIdx != -1:
    for i in 0 .. capts.blockSize-1:
      capts[result, i] = capts[captIdx, i]

func recycle*(capts: var Capts3) =
  ## Free untouched entries, and untouch all entries
  capts.free.setLen 0
  for i in 0 .. capts.touched.len-1:
    if capts.touched[i] == touNo:
      capts.free.add i.int16
    if capts.touched[i] == touYes:
      capts.touched[i] = touNo

func notRecyclable*(capts: var Capts3, captIdx: CaptIdx) =
  #doAssert capts.touched[captIdx] == touYes, $capts.touched[captIdx]
  capts.touched[captIdx] = touKeep

func recyclable*(capts: var Capts3, captIdx: CaptIdx) {.inline.} =
  capts.touched[captIdx] = touYes

func clear*(capts: var Capts3) =
  capts.s.setLen 0
  capts.touched.setLen 0
  capts.free.setLen 0

type
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
  Regex2* = distinct Regex
    ## a compiled regular expression
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
  RegexMatch2* = object
    ## result from matching operations
    captures*: seq[Slice[int]]
    namedGroups*: OrderedTable[string, int16]
    boundaries*: Slice[int]

func clear*(m: var RegexMatch) {.inline.} =
  if m.captures.len > 0:
    m.captures.setLen(0)
  if m.namedGroups.len > 0:
    m.namedGroups.clear()
  m.boundaries = 0 .. -1

func clear*(m: var RegexMatch2) {.inline.} =
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

when isMainModule:
  block:
    var capts = initCapts3(2)
    doAssert capts.len == 0
    discard capts.diverge -1
    doAssert capts.len == 1
    discard capts.diverge -1
    doAssert capts.len == 2
    discard capts.diverge -1
    doAssert capts.len == 3
  block:
    var groupsLen = 1
    var capts = initCapts3(groupsLen)
    var captx = capts.diverge -1
    doAssert captx == 0
    doAssert capts[captx, 0] == nonCapture
    captx = capts.diverge -1
    doAssert captx == 1
    doAssert capts[captx, 0] == nonCapture
  block:
    var groupsLen = 1
    var capts = initCapts3(groupsLen)
    for i in 0 .. 20:
      var captx = capts.diverge -1
      doAssert captx == i
      doAssert capts[captx, 0] == nonCapture
  block:
    var capts = initCapts3(1)
    var captx1 = capts.diverge -1
    capts[captx1, 0] = 1..1
    var captx2 = capts.diverge -1
    capts[captx2, 0] = 2..2
    doAssert capts[captx1, 0] == 1..1
    doAssert capts[captx2, 0] == 2..2
  block:
    var capts = initCapts3(2)
    var captx1 = capts.diverge -1
    capts[captx1, 0] = 1..1
    capts[captx1, 1] = 2..2
    var captx2 = capts.diverge -1
    capts[captx2, 0] = 3..3
    capts[captx2, 1] = 4..4
    doAssert capts[captx1, 0] == 1..1
    doAssert capts[captx1, 1] == 2..2
    doAssert capts[captx2, 0] == 3..3
    doAssert capts[captx2, 1] == 4..4
    doAssert captx1 == 0
    doAssert captx2 == 1
  block:
    var groupsLen = 2
    var capts = initCapts3(groupsLen)
    for i in 0 .. 20:
      var captx = capts.diverge -1
      doAssert captx == i
      for j in 0 .. 1:
        doAssert capts[i, j] == nonCapture
        capts[i,j] = (i+j)..(i+j)
    # check for overwrites
    for i in 0 .. 20:
      for j in 0 .. 1:
        doAssert capts[i,j] == (i+j)..(i+j)
  block:
    var capts = initCapts3(2)
    var captx1 = capts.diverge -1
    capts[captx1, 0] = 1..1
    capts[captx1, 1] = 2..2
    var captx2 = capts.diverge captx1
    doAssert capts[captx1, 0] == 1..1
    doAssert capts[captx1, 1] == 2..2
    doAssert capts[captx2, 0] == 1..1
    doAssert capts[captx2, 1] == 2..2
    doAssert captx1 == 0
    doAssert captx2 == 1
  block:
    var capts = initCapts3(2)
    var captx1 = capts.diverge -1
    capts[captx1, 0] = 1..1
    capts[captx1, 1] = 2..2
    capts.recycle()
    var captx2 = capts.diverge -1
    capts[captx2, 0] = 3..3
    capts[captx2, 1] = 4..4
    doAssert capts[captx1, 0] == 1..1
    doAssert capts[captx1, 1] == 2..2
    doAssert capts[captx2, 0] == 3..3
    doAssert capts[captx2, 1] == 4..4
    doAssert captx1 == 0
    doAssert captx2 == 1
  block:
    var capts = initCapts3(2)
    var captx1 = capts.diverge -1
    capts[captx1, 0] = 1..1
    capts[captx1, 1] = 2..2
    doAssert capts[captx1, 0] == 1..1
    doAssert capts[captx1, 1] == 2..2
    capts.recycle()
    capts.recycle()
    var captx2 = capts.diverge -1
    doAssert captx1 == captx2
    doAssert capts[captx1, 0] == nonCapture
    doAssert capts[captx1, 1] == nonCapture
  block:
    var capts = initCapts3(2)
    var captx1 = capts.diverge -1
    capts[captx1, 0] = 1..1
    capts[captx1, 1] = 2..2
    doAssert capts[captx1, 0] == 1..1
    doAssert capts[captx1, 1] == 2..2
    capts.recycle()
    capts.touch captx1
    var captx2 = capts.diverge -1
    doAssert captx1 != captx2
    doAssert capts[captx1, 0] == 1..1
    doAssert capts[captx1, 1] == 2..2
    doAssert capts[captx2, 0] == nonCapture
    doAssert capts[captx2, 1] == nonCapture
  block:
    var capts = initCapts3(2)
    var captx1 = capts.diverge -1
    capts[captx1, 0] = 1..1
    capts[captx1, 1] = 2..2
    doAssert capts[captx1, 0] == 1..1
    doAssert capts[captx1, 1] == 2..2
    capts.notRecyclable captx1
    capts.recycle()
    capts.recycle()
    var captx2 = capts.diverge -1
    doAssert captx1 != captx2
    doAssert capts[captx1, 0] == 1..1
    doAssert capts[captx1, 1] == 2..2
    doAssert capts[captx2, 0] == nonCapture
    doAssert capts[captx2, 1] == nonCapture
  block:
    var capts = initCapts3(2)
    var captx1 = capts.diverge -1
    capts[captx1, 0] = 1..1
    capts[captx1, 1] = 2..2
    doAssert capts[captx1, 0] == 1..1
    doAssert capts[captx1, 1] == 2..2
    capts.notRecyclable captx1
    capts.recyclable captx1
    capts.recycle()
    capts.recycle()
    var captx2 = capts.diverge -1
    doAssert captx1 == captx2
    doAssert capts[captx1, 0] == nonCapture
    doAssert capts[captx1, 1] == nonCapture
