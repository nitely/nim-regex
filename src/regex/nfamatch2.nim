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
    flags: set[MatchFlag]
  ): bool {.noSideEffect, raises: [].}
  BehindSig = proc (
    smA, smB: var Submatches,
    capts: var Capts3,
    captIdx: var int32,
    text: string,
    nfa: Nfa,
    look: var Lookaround,
    start, limit: int,
    flags: set[MatchFlag]
  ): int {.noSideEffect, raises: [].}
  Lookaround* = object
    ahead*: AheadSig
    behind*: BehindSig
    smL*: SmLookaround

template lookAroundTpl*: untyped {.dirty.} =
  template smL: untyped = look.smL
  template smLa: untyped = smL.lastA
  template smLb: untyped = smL.lastB
  template zNfa: untyped = ntn.subExp.nfa
  let flags2 = if ntn.subExp.reverseCapts:
    {mfAnchored, mfReverseCapts}
  else:
    {mfAnchored}
  smL.grow()
  smL.last.setLen zNfa.s.len
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
    var nti = 0
    while nti <= nfa.s[n].next.len-1:
      matched = true
      captx = capt
      while isEpsilonTransition(ntn):
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
          (ntn.match(c) or (anchored and ntn.kind == reEoe)):
        smB.add (nt, captx, bounds2)
      inc nti
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
  flags: set[MatchFlag] = {}
): bool =
  var
    c = Rune(-1)
    cPrev = -1'i32
    i = start
    iNext = start
    captx = -1'i32
    matched = false
    anchored = mfAnchored in flags
  if start-1 in 0 .. text.len-1:
    cPrev = bwRuneAt(text, start-1).int32
  smA.clear()
  smA.add (0'i16, captIdx, i .. i-1)
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
  flags: set[MatchFlag] = {}
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
  if start in 0 .. text.len-1:
    cPrev = text.runeAt(start).int32
  smA.clear()
  smA.add (0'i16, captIdx, i .. i-1)
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
  start, limit: int
): int =
  var capts = initCapts3(groupsLen)
  var captIdx = -1'i32
  reversedMatchImpl(
    smA, smB, capts, captIdx, text, nfa, look, start, limit)

template initLook*: Lookaround =
  Lookaround(
    ahead: matchImpl,
    behind: reversedMatchImpl)

func matchImpl*(
  text: string,
  regex: Regex,
  m: var RegexMatch2,
  start = 0
): bool =
  m.clear()
  var
    smA = newSubmatches(regex.nfa.s.len)
    smB = newSubmatches(regex.nfa.s.len)
    capts = initCapts3(regex.groupsCount)
    captIdx = -1'i32
    look = initLook()
  result = matchImpl(
    smA, smB, capts, captIdx, text, regex.nfa, look, start)
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

func startsWithImpl2*(text: string, regex: Regex, start: int): bool =
  # XXX optimize mfShortestMatch, mfNoCaptures
  template flags: untyped = {mfAnchored, mfShortestMatch, mfNoCaptures}
  var
    smA = newSubmatches(regex.nfa.s.len)
    smB = newSubmatches(regex.nfa.s.len)
    capts = initCapts3(regex.groupsCount)
    captIdx = -1'i32
    look = initLook()
  result = matchImpl(
    smA, smB, capts, captIdx, text, regex.nfa, look, start, flags)
