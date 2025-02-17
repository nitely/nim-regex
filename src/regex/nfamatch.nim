## DEPRECATED

import std/unicode
import std/tables

import ./common
import ./nodematch
import ./types
import ./nfatype

type
  AheadSig = proc (
    smA, smB: var Pstates,
    capts: var Capts,
    captIdx: var CaptIdx,
    text: string,
    nfa: Nfa,
    look: var Lookaround,
    start: int,
    flags: set[MatchFlag]
  ): bool {.nimcall, noSideEffect, raises: [].}
  BehindSig = proc (
    smA, smB: var Pstates,
    capts: var Capts,
    captIdx: var CaptIdx,
    text: string,
    nfa: Nfa,
    look: var Lookaround,
    start, limit: int,
    flags: set[MatchFlag]
  ): int {.nimcall, noSideEffect, raises: [].}
  Lookaround* = object
    ahead*: AheadSig
    behind*: BehindSig
    #smL*: SmLookaround

template lookAroundTpl*: untyped {.dirty.} =
  template zNfa: untyped = ntn.subExp.nfa
  let flags2 = if ntn.subExp.reverseCapts:
    {mfAnchored, mfReverseCapts}
  else:
    {mfAnchored}
  var smLa = initPstates(zNfa.s.len)
  var smLb = initPstates(zNfa.s.len)
  matched = case ntn.kind
  of reLookahead:
    look.ahead(
      smLa, smLb, capts, captx,
      text, zNfa, look, i, flags2)
  of reNotLookahead:
    not look.ahead(
      smLa, smLb, capts, captx,
      text, zNfa, look, i, flags2)
  of reLookbehind:
    look.behind(
      smLa, smLb, capts, captx,
      text, zNfa, look, i, 0, flags2) != -1
  of reNotLookbehind:
    look.behind(
      smLa, smLb, capts, captx,
      text, zNfa, look, i, 0, flags2) == -1
  else:
    doAssert false
    false

template nextStateTpl(bwMatch = false): untyped {.dirty.} =
  template bounds2: untyped =
    when bwMatch: i .. bounds.b else: bounds.a .. i-1
  template nt: untyped = nfa.s[n].next[nti]
  template ntn: untyped = nfa.s[nt]
  template n: untyped = pstate.ni
  template capt: untyped = pstate.ci
  template bounds: untyped = pstate.bounds
  smB.clear()
  for pstate in items smA:
    if anchored and nfa.s[n].kind == reEoe:
      if n notin smB:
        smB.add initPstate(n, capt, bounds)
      break
    let L = nfa.s[n].next.len
    var nti = 0
    while nti < L:
      let nt0 = nt
      matched = nt notin smB and
        (ntn.match(c) or (anchored and ntn.kind == reEoe))
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
            when bwMatch:
              matched = match(ntn, c, cPrev.Rune)
            else:
              matched = match(ntn, cPrev.Rune, c)
          of lookaroundKind:
            lookAroundTpl()
          else:
            doAssert false
            discard
        inc nti
      if matched:
        smB.add initPstate(nt0, captx, bounds2)
  swap smA, smB

func matchImpl(
  smA, smB: var Pstates,
  capts: var Capts,
  captIdx: var CaptIdx,
  text: string,
  nfa: Nfa,
  look: var Lookaround,
  start = 0,
  flags: set[MatchFlag] = {}
): bool =
  var
    c = Rune(-1)
    cPrev = -1'i32
    i = start
    iNext = start
    captx = -1.CaptIdx
    matched = false
    anchored = mfAnchored in flags
  if start-1 in 0 .. text.len-1:
    cPrev = bwRuneAt(text, start-1).int32
  smA.clear()
  smA.add initPstate(0'i16, captIdx, i .. i-1)
  while i < text.len:
    fastRuneAt(text, iNext, c, true)
    nextStateTpl()
    if smA.len == 0:
      return false
    if anchored and nfa.s[smA[0].ni].kind == reEoe:
      break
    i = iNext
    cPrev = c.int32
  c = Rune(-1)
  nextStateTpl()
  if smA.len > 0:
    if mfReverseCapts in flags:
      captIdx = reverse(capts, smA[0].ci, captIdx)
    else:
      captIdx = smA[0].ci
  return smA.len > 0

func reversedMatchImpl(
  smA, smB: var Pstates,
  capts: var Capts,
  captIdx: var CaptIdx,
  text: string,
  nfa: Nfa,
  look: var Lookaround,
  start: int,
  limit = 0,
  flags: set[MatchFlag] = {}
): int =
  #doAssert start < len(text)
  doAssert start >= limit
  var
    c = Rune(-1)
    cPrev = -1'i32
    i = start
    iNext = start
    captx = 0.CaptIdx
    matched = false
    anchored = true
  if start in 0 .. text.len-1:
    cPrev = text.runeAt(start).int32
  smA.clear()
  smA.add initPstate(0'i16, captIdx, i .. i-1)
  while iNext > limit:
    bwFastRuneAt(text, iNext, c)
    nextStateTpl(bwMatch = true)
    if smA.len == 0:
      return -1
    if nfa.s[smA[0].ni].kind == reEoe:
      break
    i = iNext
    cPrev = c.int32
  c = Rune(-1)
  if iNext > 0:
    bwFastRuneAt(text, iNext, c)
  nextStateTpl(bwMatch = true)
  for pstate in items smA:
    if nfa.s[pstate.ni].kind == reEoe:
      if mfReverseCapts in flags:
        captIdx = reverse(capts, pstate.ci, captIdx)
      else:
        captIdx = pstate.ci
      return pstate.bounds.a
  return -1

func reversedMatchImpl*(
  smA, smB: var Pstates,
  text: string,
  nfa: Nfa,
  look: var Lookaround,
  start, limit: int
): int =
  var capts = default(Capts)
  var captIdx = -1.CaptIdx
  reversedMatchImpl(
    smA, smB, capts, captIdx, text, nfa, look, start, limit
  )

template initLook*: Lookaround =
  Lookaround(
    ahead: matchImpl,
    behind: reversedMatchImpl)

func matchImpl*(
  text: string,
  regex: Regex,
  m: var RegexMatch,
  start = 0
): bool =
  m.clear()
  var
    smA = initPstates(regex.nfa.s.len)
    smB = initPstates(regex.nfa.s.len)
    capts = default(Capts)
    capt = -1.CaptIdx
    look = initLook()
  result = matchImpl(
    smA, smB, capts, capt, text, regex.nfa, look, start)
  if result:
    constructSubmatches(
      m.captures, capts, capt, regex.groupsCount)
    if regex.namedGroups.len > 0:
      m.namedGroups = regex.namedGroups
    m.boundaries = smA[0].bounds

func startsWithImpl*(text: string, regex: Regex, start: int): bool =
  # XXX optimize mfShortestMatch, mfNoCaptures
  template flags: untyped = {mfAnchored, mfShortestMatch, mfNoCaptures}
  var
    smA = initPstates(regex.nfa.s.len)
    smB = initPstates(regex.nfa.s.len)
    capts = default(Capts)
    capt = -1.CaptIdx
    look = initLook()
  result = matchImpl(
    smA, smB, capts, capt, text, regex.nfa, look, start, flags)
