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
    capts: var Capts2,
    captIdx: var int32,
    text: string,
    nfa: Nfa,
    look: var Lookaround,
    start: int,
    flags: set[MatchFlag]
  ): bool {.noSideEffect, raises: [].}
  BehindSig = proc (
    smA, smB: var Submatches,
    capts: var Capts2,
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
  template zNfa: untyped = z.subExp.nfa
  let flags2 = if z.subExp.reverseCapts:
    {mfAnchored, mfReverseCapts}
  else:
    {mfAnchored}
  smL.grow()
  smL.last.setLen zNfa.s.len
  matched = case z.kind
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
    capts[captx][z.idx]
  smB.clear()
  for n, capt, bounds in items smA:
    if anchored and nfa.s[n].kind == reEoe:
      if not smB.hasState n:
        smB.add (n, capt, bounds)
      break
    for nti, nt in pairs nfa.s[n].next:
      if smB.hasState nt:
        continue
      if not match(nfa.s[nt], c):
        if not (anchored and nfa.s[nt].kind == reEoe):
          continue
      if nfa.t.allZ[n][nti] == -1'i16:
        smB.add (nt, capt, bounds2)
        continue
      matched = true
      captx = capt
      for z in nfa.t.z[nfa.t.allZ[n][nti]]:
        if not matched:
          break
        case z.kind
        of groupKind:
          if captx < 0:
            capts.inc()
            captx = (capts.len-1).int32
          else:
            capts.add capts[captx]
            captx = (capts.len-1).int32
          # XXX put in parent case
          if z.kind == reGroupStart and
              (mfReverseCapts notin flags or
              captElm.a == nonCapture.a):
            captElm.a = i
          elif z.kind == reGroupEnd and
              (mfReverseCapts notin flags or
              captElm.b == nonCapture.b):
            captElm.b = i-1
        of assertionKind - lookaroundKind:
          when bwMatch:
            matched = match(z, c, cPrev.Rune)
          else:
            matched = match(z, cPrev.Rune, c)
        of lookaroundKind:
          lookAroundTpl()
        else:
          doAssert false
          discard
      if matched:
        smB.add (nt, captx, bounds2)
  swap smA, smB

func matchImpl(
  smA, smB: var Submatches,
  capts: var Capts2,
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
    #if captIdx != -1 and
    #    mfReverseCapts in flags:
    #  capts[captIdx].reverse()
  return smA.len > 0

func reversedMatchImpl(
  smA, smB: var Submatches,
  capts: var Capts2,
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
      #if mfReverseCapts in flags:
      #  capts[captIdx].reverse()
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
  var capts = initCapts2(groupsLen)
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
    capts = initCapts2(regex.groupsCount)
    captIdx = -1'i32
    look = initLook()
  result = matchImpl(
    smA, smB, capts, captIdx, text, regex.nfa, look, start)
  if result:
    if captIdx != -1:
      m.captures.add capts[captIdx]
    else:
      m.captures.setLen regex.groupsCount
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
    capts = initCapts2(regex.groupsCount)
    captIdx = -1'i32
    look = initLook()
  result = matchImpl(
    smA, smB, capts, captIdx, text, regex.nfa, look, start, flags)
