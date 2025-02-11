## Types used by the NFA

import std/tables
import std/sets
import std/algorithm
import std/math
import std/bitops

import ./types
import ./litopt

const nonCapture* = -1 .. -2

# XXX limit lookarounds to int8.high per regex
type CaptState* = uint8
const
  stsInitial = 0.CaptState
  stsKeepAlive = 1.CaptState
  stsRecyclable = 2.CaptState
  stsRecycled = 3.CaptState
  stsNotRecyclable = 4.CaptState
  stsFrozen = 5.CaptState .. CaptState.high

type
  # XXX int16 same as max parallel states or max regex len
  #     but it's used by Pstate and the old capts
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
    states: seq[CaptState]
    free: seq[CaptIdx]
    freezeId: CaptState

func len(capts: Capts3): int {.inline.} =
  capts.s.len shr capts.blockSizeL2  # s.len / blockSize

func `[]`*(capts: Capts3, i, j: Natural): Slice[int] {.inline.} =
  doAssert i <= capts.len-1
  doAssert j <= capts.blockSize-1
  result = capts.s[(i shl capts.blockSizeL2) + j]  # i * blockSize

func `[]`*(capts: var Capts3, i, j: Natural): var Slice[int] {.inline.} =
  doAssert i <= capts.len-1
  doAssert j <= capts.blockSize-1
  result = capts.s[(i shl capts.blockSizeL2) + j]  # i * blockSize

func blockIdx(capts: Capts3, blockNum: Natural): int {.inline.} =
  assert blockNum <= capts.len-1
  blockNum shl capts.blockSizeL2

when defined(js):
  func jsLog2(x: Natural): int {.importjs: "Math.log2(@)".}

template fastLog2Tpl(x: Natural): untyped =
  when nimvm:
    fastLog2(x)
  else:
    when defined(js):
      jsLog2(x)
    else:
      fastLog2(x)

func reset*(capts: var Capts3, groupsLen: int) =
  if groupsLen == 0:
    return
  if capts.groupsLen != groupsLen:
    let blockSize = max(2, nextPowerOfTwo groupsLen)
    capts.groupsLen = groupsLen
    capts.blockSize = blockSize
    capts.blockSizeL2 = fastLog2Tpl blockSize
  capts.freezeId = stsFrozen.a
  capts.s.setLen 0
  capts.states.setLen 0
  capts.free.setLen 0

func initCapts3*(groupsLen: int): Capts3 =
  reset(result, groupsLen)

func check(curr, next: CaptState): bool =
  ## Check if transition from state curr to next is allowed
  result = case next:
  of stsInitial:
    curr == stsInitial or
    curr == stsRecycled or
    curr == stsNotRecyclable or
    curr in stsFrozen
  of stsKeepAlive:
    curr == stsInitial or
    curr == stsRecyclable
  of stsRecyclable:
    curr == stsInitial or
    curr == stsKeepAlive
  of stsRecycled:
    curr == stsRecyclable or
    curr == stsRecycled
  of stsNotRecyclable:
    curr == stsInitial or
    curr == stsKeepAlive or
    curr in stsFrozen
  else:
    doAssert next in stsFrozen
    curr == stsInitial or
    curr == stsKeepAlive or
    curr == stsRecyclable

proc to(a: var CaptState, b: CaptState) {.inline.} =
  doAssert check(a, b), $a.int & " " & $b.int
  a = b

func keepAlive*(capts: var Capts3, captIdx: CaptIdx) {.inline.} =
  template state: untyped = capts.states[captIdx]
  doAssert state != stsRecycled
  if state == stsInitial or
      state == stsRecyclable:
    state.to stsKeepAlive

func freeze*(capts: var Capts3): CaptState =
  ## Freeze all in use capts.
  ## Return freezeId
  doAssert capts.freezeId < stsFrozen.b
  inc capts.freezeId
  result = capts.freezeId
  for state in mitems capts.states:
    if state == stsInitial or
        state == stsKeepAlive or
        state == stsRecyclable:
      state.to result

func unfreeze*(capts: var Capts3, freezeId: CaptState) =
  doAssert freezeId in stsFrozen
  doAssert freezeId == capts.freezeId, "Unordered freeze/unfreeze call"
  for state in mitems capts.states:
    if state == freezeId:
      state.to stsInitial
  dec capts.freezeId

func diverge*(capts: var Capts3, captIdx: CaptIdx): CaptIdx =
  if capts.free.len > 0:
    result = capts.free.pop
    capts.states[result].to stsInitial
  else:
    result = capts.len.CaptIdx
    capts.s.setLen(capts.s.len+capts.blockSize)
    capts.states.add stsInitial
    doAssert result == capts.states.len-1
  let idx = capts.blockIdx(result)
  if captIdx != -1:
    let cidx = capts.blockIdx(captIdx)
    for i in 0 .. capts.blockSize-1:
      capts.s[idx+i] = capts.s[cidx+i]
  else:
    for i in 0 .. capts.blockSize-1:
      capts.s[idx+i] = nonCapture

func recycle*(capts: var Capts3) =
  ## Free recyclable entries
  ## Set initial/keepAlive entries to recyclable
  capts.free.setLen 0
  for i, state in mpairs capts.states:
    if state == stsRecyclable or
        state == stsRecycled:
      capts.free.add i.int16
      state.to stsRecycled
    if state == stsInitial or
        state == stsKeepAlive:
      state.to stsRecyclable

func notRecyclable*(capts: var Capts3, captIdx: CaptIdx) =
  capts.states[captIdx].to stsNotRecyclable

func recyclable*(capts: var Capts3, captIdx: CaptIdx) {.inline.} =
  capts.states[captIdx].to stsInitial

func clear*(capts: var Capts3) =
  capts.s.setLen 0
  capts.states.setLen 0
  capts.free.setLen 0

# XXX Deprecate
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

func reverse*(capts: var Capts, a, b: CaptIdx): CaptIdx =
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
  MatchFlag* = enum
    mfShortestMatch
    mfNoCaptures
    mfFindMatch
    mfFindMatchOpt
    mfAnchored
    mfBwMatch
    mfReverseCapts
    mfBytesInput
  MatchFlags* = set[MatchFlag]

func toMatchFlags*(f: RegexFlags): MatchFlags =
  result = default(MatchFlags)
  if regexArbitraryBytes in f:
    result.incl mfBytesInput

type
  RegexLit* = distinct string
    ## raw regex literal string
  Regex* = object
    ## deprecated
    nfa*: Nfa
    groupsCount*: int16
    namedGroups*: OrderedTable[string, int16]
    flags*: RegexFlags
    litOpt*: LitOpt
  RegexMatch* = object
    ## deprecated
    captures*: Captures
    namedGroups*: OrderedTable[string, int16]
    boundaries*: Slice[int]
  RegexMatch2* = object
    ## result from matching operations
    captures*: seq[Slice[int]]
    namedGroups*: OrderedTable[string, int16]
    boundaries*: Slice[int]

when defined(js) and (NimMajor, NimMinor) >= (1, 6) and (NimMajor, NimMinor) <= (1, 7):
  type
    Regex2* = object
      ## a compiled regular expression
      nfa*: Nfa
      groupsCount*: int16
      namedGroups*: OrderedTable[string, int16]
      #flags*: set[RegexFlag]
      litOpt*: LitOpt

  func toRegex2*(r: Regex): Regex2 =
    Regex2(nfa: r.nfa, groupsCount: r.groupsCount, namedGroups: r.namedGroups, litOpt: r.litOpt)

  func toRegex*(r: Regex2): Regex =
    Regex(nfa: r.nfa, groupsCount: r.groupsCount, namedGroups: r.namedGroups, litOpt: r.litOpt)
else:
  type
    Regex2* = distinct Regex
      ## a compiled regular expression

  template toRegex2*(r): untyped = Regex2(r)
  template toRegex*(r): untyped = Regex(r)

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
  Pstate* = object
    ni*: NodeIdx
    ci*: CaptIdx
    bounds*: Bounds
  Pstates* = object
    ## This is a sparse set
    sx: seq[Pstate]
    ss: seq[int16]
    si: int16

func initPstate*(ni: NodeIdx, ci: CaptIdx, bounds: Bounds): Pstate {.inline.} =
  Pstate(ni: ni, ci: ci, bounds: bounds)

when defined(release):
  {.push checks: off.}

func reset*(sm: var Pstates, size: int) {.inline.} =
  sm.sx.setLen 8
  sm.ss.setLen size
  sm.si = 0

func initPstates*(size: int): Pstates {.inline.} =
  reset(result, size)

func `[]`*(sm: Pstates, i: int): lent Pstate {.inline.} =
  assert i < sm.si
  sm.sx[i]

func contains*(sm: Pstates, n: int16): bool {.inline.} =
  sm.ss[n] < sm.si and sm.sx[sm.ss[n]].ni == n

func add*(sm: var Pstates, item: sink Pstate) {.inline.} =
  assert(item.ni notin sm)
  assert sm.si <= sm.sx.len
  if (sm.si == sm.sx.len).unlikely:
    sm.sx.setLen(sm.sx.len * 2)
  sm.sx[sm.si] = item
  sm.ss[item.ni] = sm.si
  sm.si += 1'i16

func len*(sm: Pstates): int {.inline.} =
  sm.si

func clear*(sm: var Pstates) {.inline.} =
  sm.si = 0

iterator items*(sm: Pstates): lent Pstate {.inline.} =
  for i in 0 .. sm.len-1:
    yield sm.sx[i]

func cap*(sm: Pstates): int {.inline.} =
  sm.ss.len

when defined(release):
  {.pop.}

# XXX maybe store the lookaround number + count, and use a fixed
#     size seq to reduce allocations
type
  SmLookaroundItem* = object
    a, b: Pstates
  SmLookaround* = object
    s: seq[SmLookaroundItem]
    i: int

func reset*(item: var SmLookaroundItem, size: int) {.inline.} =
  item.a.reset size
  item.b.reset size

template last*(sm: SmLookaround): untyped =
  sm.s[sm.i-1]

template lastA*(sm: SmLookaround): untyped =
  last(sm).a

template lastB*(sm: SmLookaround): untyped =
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
  func `[]=`(capts: var Capts3, i, j: Natural, x: Slice[int]) =
    doAssert i <= capts.len-1
    doAssert j <= capts.blockSize-1
    capts.s[(i shl capts.blockSizeL2) + j] = x
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
    capts.keepAlive captx1
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
  block:
    var capts = initCapts3(2)
    var captx1 = capts.diverge -1
    capts[captx1, 0] = 1..1
    discard capts.freeze()
    capts.recycle()
    capts.recycle()
    doAssert capts.free.len == 0
    discard capts.diverge -1
    doAssert capts[captx1, 0] == 1..1
  block:
    var capts = initCapts3(2)
    discard capts.diverge -1
    var freeze1 = capts.freeze()
    capts.recycle()
    capts.recycle()
    doAssert capts.free.len == 0
    discard capts.diverge -1
    var freeze2 = capts.freeze()
    doAssert freeze1 != freeze2
    capts.recycle()
    capts.recycle()
    doAssert capts.free.len == 0
    capts.unfreeze freeze2
    capts.recycle()
    capts.recycle()
    doAssert capts.free.len == 1
    capts.unfreeze freeze1
    capts.recycle()
    capts.recycle()
    doAssert capts.free.len == 2
  block:
    var capts = initCapts3(2)
    var captx1 = capts.diverge -1
    var freeze1 = capts.freeze()
    capts.recycle()
    capts.recycle()
    doAssert capts.free.len == 0
    capts.keepAlive captx1  # does nothing
    capts.recycle()
    capts.recycle()
    doAssert capts.free.len == 0
    capts.unfreeze freeze1
    capts.recycle()
    capts.recycle()
    doAssert capts.free.len == 1
