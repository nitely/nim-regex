## NFA matcher for non-static regexes

import std/unicode
import std/tables

import ./common
import ./nodematch
import ./types
import ./nfatype

type
  AheadSig = proc (
    smA, smB: var Pstates,
    capts: var Capts3,
    captIdx: var CaptIdx,
    text: string,
    nfa: Nfa,
    look: var Lookaround,
    start: int,
    flags: MatchFlags
  ): bool {.nimcall, noSideEffect, raises: [].}
  BehindSig = proc (
    smA, smB: var Pstates,
    capts: var Capts3,
    captIdx: var CaptIdx,
    text: string,
    nfa: Nfa,
    look: var Lookaround,
    start, limit: int,
    flags: MatchFlags
  ): int {.nimcall, noSideEffect, raises: [].}
  Lookaround* = object
    ahead*: AheadSig
    behind*: BehindSig
    smL*: SmLookaround

func lookAround(
  ntn: Node,
  capts: var Capts3,
  captIdx: var CaptIdx,
  text: string,
  look: var Lookaround,
  start: int,
  flags: MatchFlags
): bool =
  template smL: untyped = look.smL
  template smLa: untyped = smL.lastA
  template smLb: untyped = smL.lastB
  template subNfa: untyped = ntn.subExp.nfa
  var flags2 = {mfAnchored}
  if ntn.subExp.reverseCapts:
    flags2.incl mfReverseCapts
  if mfBytesInput in flags:
    flags2.incl mfBytesInput
  smL.grow()
  smL.last.reset subNfa.s.len
  result = case ntn.kind
  of reLookahead:
    look.ahead(
      smLa, smLb, capts, captIdx, text, subNfa, look, start, flags2
    )
  of reNotLookahead:
    not look.ahead(
      smLa, smLb, capts, captIdx, text, subNfa, look, start, flags2
    )
  of reLookbehind:
    look.behind(
      smLa, smLb, capts, captIdx, text, subNfa, look, start, 0, flags2
    ) != -1
  of reNotLookbehind:
    look.behind(
      smLa, smLb, capts, captIdx, text, subNfa, look, start, 0, flags2
    ) == -1
  else:
    doAssert false
    false
  smL.removeLast()

func epsilonMatch*(
  matched: var bool,
  captx: var CaptIdx,
  capts: var Capts3,
  look: var Lookaround,
  ntn: Node,
  text: string,
  i: int,
  cPrev: int32,
  c: Rune,
  flags: MatchFlags,
  bwMatch = false
) =
  template captElm: untyped =
    capts[captx, ntn.idx]
  case ntn.kind
  of reGroupStart:
    if mfNoCaptures notin flags:
      captx = capts.diverge captx
      if mfReverseCapts notin flags or
          captElm.a == nonCapture.a:
        captElm.a = i
  of reGroupEnd:
    if mfNoCaptures notin flags:
      captx = capts.diverge captx
      if mfReverseCapts notin flags or
          captElm.b == nonCapture.b:
        captElm.b = i-1
  of assertionKind - lookaroundKind:
    if bwMatch:
      matched = match(ntn, c, cPrev.Rune)
    else:
      matched = match(ntn, cPrev.Rune, c)
  of lookaroundKind:
    let freezed = capts.freeze()
    matched = lookAround(ntn, capts, captx, text, look, i, flags)
    capts.unfreeze freezed
    if captx != -1:
      capts.keepAlive captx
  else:
    doAssert false
    discard

func nextState(
  smA, smB: var Pstates,
  capts: var Capts3,
  look: var Lookaround,
  text: string,
  nfa2: Nfa,
  i: int,
  cPrev: int32,
  c: Rune,
  flags: MatchFlags,
  bwMatch = false
) {.inline.} =
  template nfa: untyped = nfa2.s
  template bounds2: untyped =
    if bwMatch: i .. bounds.b else: bounds.a .. i-1
  template nt: untyped = nfa[n].next[nti]
  template ntn: untyped = nfa[nt]
  template n: untyped = pstate.ni
  template capt: untyped = pstate.ci
  template bounds: untyped = pstate.bounds
  let anchored = mfAnchored in flags
  var captx = 0.CaptIdx
  var matched = true
  smB.clear()
  for pstate in items smA:
    if capt != -1:
      capts.keepAlive capt
    if anchored and nfa[n].kind == reEoe:
      if n notin smB:
        smB.add initPstate(n, capt, bounds)
      break
    let L = nfa[n].next.len
    var nti = 0
    while nti < L:
      let nt0 = nt
      matched = nt notin smB and
        (ntn.match(c) or (anchored and ntn.kind == reEoe))
      inc nti
      captx = capt
      while nti < L and isEpsilonTransition(ntn):
        if matched:
          epsilonMatch(
            matched, captx, capts, look, ntn, text, i, cPrev, c, flags, bwMatch
          )
        inc nti
      if matched:
        smB.add initPstate(nt0, captx, bounds2)
  swap smA, smB
  capts.recycle()

func matchImpl(
  smA, smB: var Pstates,
  capts: var Capts3,
  captIdx: var CaptIdx,
  text: string,
  nfa: Nfa,
  look: var Lookaround,
  start = 0,
  flags: MatchFlags = {}
): bool =
  var
    c = Rune(-1)
    cPrev = -1'i32
    i = start
    iNext = start
  let
    anchored = mfAnchored in flags
    binFlag = mfBytesInput in flags
  if start-1 in 0 .. text.len-1:
    cPrev = if binFlag:
      text[start-1].int32
    else:
      bwRuneAt(text, start-1).int32
  smA.clear()
  smA.add initPstate(0'i16, captIdx, i .. i-1)
  while i < text.len:
    if binFlag:
      c = text[iNext].Rune
      inc iNext
    else:
      fastRuneAt(text, iNext, c, true)
    nextState(smA, smB, capts, look, text, nfa, i, cPrev, c, flags)
    if smA.len == 0:
      return false
    if anchored and nfa.s[smA[0].ni].kind == reEoe:
      break
    i = iNext
    cPrev = c.int32
  c = Rune(-1)
  nextState(smA, smB, capts, look, text, nfa, i, cPrev, c, flags)
  if smA.len > 0:
    captIdx = smA[0].ci
  return smA.len > 0

func reversedMatchImpl(
  smA, smB: var Pstates,
  capts: var Capts3,
  captIdx: var CaptIdx,
  text: string,
  nfa: Nfa,
  look: var Lookaround,
  start: int,
  limit = 0,
  flags: MatchFlags = {}
): int =
  #doAssert start < len(text)
  doAssert start >= limit
  var
    c = Rune(-1)
    cPrev = -1'i32
    i = start
    iNext = start
  let flags = flags + {mfAnchored}
  let binFlag = mfBytesInput in flags
  if start in 0 .. text.len-1:
    cPrev = if binFlag:
      text[start].int32
    else:
      runeAt(text, start).int32
  smA.clear()
  smA.add initPstate(0'i16, captIdx, i .. i-1)
  while iNext > limit:
    if binFlag:
      c = text[iNext-1].Rune
      dec iNext
    else:
      bwFastRuneAt(text, iNext, c)
    nextState(smA, smB, capts, look, text, nfa, i, cPrev, c, flags, bwMatch = true)
    if smA.len == 0:
      return -1
    if nfa.s[smA[0].ni].kind == reEoe:
      break
    i = iNext
    cPrev = c.int32
  c = Rune(-1)
  if iNext > 0:
    if binFlag:
      c = text[iNext-1].Rune
      dec iNext
    else:
      bwFastRuneAt(text, iNext, c)
  nextState(smA, smB, capts, look, text, nfa, i, cPrev, c, flags, bwMatch = true)
  for pstate in items smA:
    if nfa.s[pstate.ni].kind == reEoe:
      captIdx = pstate.ci
      return pstate.bounds.a
  return -1

func reversedMatchImpl*(
  smA, smB: var Pstates,
  text: string,
  nfa: Nfa,
  look: var Lookaround,
  groupsLen: int,
  start, limit: int,
  flags: MatchFlags = {}
): int =
  var capts = initCapts3(groupsLen)
  var captIdx = -1.CaptIdx
  reversedMatchImpl(
    smA, smB, capts, captIdx, text, nfa, look, start, limit, flags
  )

template initLook*: Lookaround =
  Lookaround(
    ahead: matchImpl,
    behind: reversedMatchImpl
  )

func matchImpl*(
  text: string,
  regex: Regex,
  m: var RegexMatch2,
  start = 0,
  flags: MatchFlags = {}
): bool =
  m.clear()
  let flags = regex.flags.toMatchFlags + flags
  var
    smA = initPstates(regex.nfa.s.len)
    smB = initPstates(regex.nfa.s.len)
    capts = initCapts3(regex.groupsCount)
    captIdx = -1.CaptIdx
    look = initLook()
  result = matchImpl(
    smA, smB, capts, captIdx, text, regex.nfa, look, start, flags
  )
  if result:
    m.captures.setLen regex.groupsCount
    if captIdx != -1:
      for i in 0 .. m.captures.len-1:
        m.captures[i] = capts[captIdx, i]
    else:
      for i in 0 .. m.captures.len-1:
        m.captures[i] = nonCapture
    if regex.namedGroups.len > 0:
      m.namedGroups = regex.namedGroups
    m.boundaries = smA[0].bounds

func startsWithImpl2*(
  text: string,
  regex: Regex,
  start: int
): bool =
  # XXX optimize mfShortestMatch, mfNoCaptures
  let flags = regex.flags.toMatchFlags + {mfAnchored, mfShortestMatch, mfNoCaptures}
  var
    smA = initPstates(regex.nfa.s.len)
    smB = initPstates(regex.nfa.s.len)
    capts = initCapts3(regex.groupsCount)
    captIdx = -1.CaptIdx
    look = initLook()
  result = matchImpl(
    smA, smB, capts, captIdx, text, regex.nfa, look, start, flags
  )
