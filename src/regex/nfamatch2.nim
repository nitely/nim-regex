## NFA matcher for non-static regexes

import std/unicode
import std/tables

import ./common
import ./nodematch
import ./types
import ./nfatype

type
  AheadSig = proc (
    smA, smB: var Submatches,
    capts: var Capts3,
    captIdx: var int32,
    text: string,
    nfa: Nfa,
    look: var Lookaround,
    start: int,
    flags: MatchFlags
  ): bool {.nimcall, noSideEffect, raises: [].}
  BehindSig = proc (
    smA, smB: var Submatches,
    capts: var Capts3,
    captIdx: var int32,
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

func lookAround*(
  ntn: Node,
  capts: var Capts3,
  captIdx: var int32,
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
  smL.last.setLen subNfa.s.len
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

template nextStateTpl(bwMatch = false): untyped {.dirty.} =
  template bounds2: untyped =
    when bwMatch: i .. bounds.b else: bounds.a .. i-1
  template captElm: untyped =
    capts[captx, nfa.s[nt].idx]
  template nt: untyped = nfa.s[n].next[nti]
  template ntn: untyped = nfa.s[nt]
  smB.clear()
  for n, capt, bounds in items smA:
    if capt != -1:
      capts.keepAlive capt
    if anchored and nfa.s[n].kind == reEoe:
      if not smB.hasState n:
        smB.add (n, capt, bounds)
      break
    let L = nfa.s[n].next.len
    var nti = 0
    while nti < L:
      let nt0 = nt
      matched = not smB.hasState(nt) and
        (ntn.match(c) or (anchored and ntn.kind == reEoe))
      inc nti
      captx = capt
      while nti < L and isEpsilonTransition(ntn):
        if matched:
          case ntn.kind
          of reGroupStart:
            # XXX this can be avoided in some cases?
            captx = capts.diverge captx
            if mfReverseCapts notin flags or
                captElm.a == nonCapture.a:
              captElm.a = i
          of reGroupEnd:
            captx = capts.diverge captx
            if mfReverseCapts notin flags or
                captElm.b == nonCapture.b:
              captElm.b = i-1
          of assertionKind - lookaroundKind:
            when bwMatch:
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
        inc nti
      if matched:
        smB.add (nt0, captx, bounds2)
  swap smA, smB
  capts.recycle()

func matchImpl(
  smA, smB: var Submatches,
  capts: var Capts3,
  captIdx: var int32,
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
    captx = -1'i32
    matched = false
  let
    anchored = mfAnchored in flags
    binFlag = mfBytesInput in flags
  if start-1 in 0 .. text.len-1:
    cPrev = if binFlag:
      text[start-1].int32
    else:
      bwRuneAt(text, start-1).int32
  smA.clear()
  smA.add (0'i16, captIdx, i .. i-1)
  while i < text.len:
    if binFlag:
      c = text[iNext].Rune
      inc iNext
    else:
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
    captIdx = smA[0].ci
  return smA.len > 0

func reversedMatchImpl(
  smA, smB: var Submatches,
  capts: var Capts3,
  captIdx: var int32,
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
    captx: int32
    matched = false
    anchored = true
  let binFlag = mfBytesInput in flags
  if start in 0 .. text.len-1:
    cPrev = if binFlag:
      text[start].int32
    else:
      runeAt(text, start).int32
  smA.clear()
  smA.add (0'i16, captIdx, i .. i-1)
  while iNext > limit:
    if binFlag:
      c = text[iNext-1].Rune
      dec iNext
    else:
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
    if binFlag:
      c = text[iNext-1].Rune
      dec iNext
    else:
      bwFastRuneAt(text, iNext, c)
  nextStateTpl(bwMatch = true)
  for n, capt, bounds in items smA:
    if nfa.s[n].kind == reEoe:
      captIdx = capt
      return bounds.a
  return -1

func reversedMatchImpl*(
  smA, smB: var Submatches,
  text: string,
  nfa: Nfa,
  look: var Lookaround,
  groupsLen: int,
  start, limit: int,
  flags: MatchFlags = {}
): int =
  var capts = initCapts3(groupsLen)
  var captIdx = -1'i32
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
    smA = newSubmatches(regex.nfa.s.len)
    smB = newSubmatches(regex.nfa.s.len)
    capts = initCapts3(regex.groupsCount)
    captIdx = -1'i32
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
    smA = newSubmatches(regex.nfa.s.len)
    smB = newSubmatches(regex.nfa.s.len)
    capts = initCapts3(regex.groupsCount)
    captIdx = -1'i32
    look = initLook()
  result = matchImpl(
    smA, smB, capts, captIdx, text, regex.nfa, look, start, flags
  )
