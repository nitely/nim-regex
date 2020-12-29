## NFA matcher for non-static regexes

# Pro-tip: try modifications here and
# run tests passing `-d:forceRegexAtRuntime`.
# Then port the changes to nfamacro. Hacking
# with nfamatch is a lot easier.

import std/unicode
import std/tables

import ./nodematch
import ./types
import ./nfatype

func bwRuneAt(s: string, n: int): Rune =
  ## Take rune ending at ``n``
  doAssert n >= 0
  doAssert n <= s.len-1
  var n = n
  while n > 0 and s[n].ord shr 6 == 0b10:
    dec n
  fastRuneAt(s, n, result, false)

template bwFastRuneAt(
  s: string, n: var int, result: var Rune
) =
  ## Take rune ending at ``n``
  doAssert n > 0
  doAssert n <= s.len-1
  dec n
  while n > 0 and s[n].ord shr 6 == 0b10:
    dec n
  fastRuneAt(s, n, result, false)

when true:
  type
    MatchSig = proc (
      smA, smB: var Submatches,
      capts: var Capts,
      captIdx: var int32,
      text: string,
      nfa: Nfa,
      look: Lookaround,
      start: int,
      flags: set[MatchFlag]
    ): bool {.noSideEffect, raises: [].}
    Lookaround = object
      ahead, behind: MatchSig
  
  template lookAroundTpl: untyped {.dirty.} =
    case z.kind
    of reLookahead:
      look.ahead(
        smAl, smBl, capts, captx,
        text, z.subExp.nfa, look, i, {mfAnchored})
    of reNotLookahead:
      not look.ahead(
        smAl, smBl, capts, captx,
        text, z.subExp.nfa, look, i, {mfAnchored})
    of reLookbehind:
      look.behind(
        smAl, smBl, capts, captx,
        text, z.subExp.nfa, look, i, {mfAnchored})
    of reNotLookbehind:
      not look.behind(
        smAl, smBl, capts, captx,
        text, z.subExp.nfa, look, i, {mfAnchored})
    else:
      doAssert false
      false

  template nextStateTpl: untyped {.dirty.} =
    smB.clear()
    for n, capt, bounds in items smA:
      if anchored and nfa.s[n].kind == reEoe:
        smB.add (n, capt, bounds)
        break
      for nti, nt in pairs nfa.s[n].next:
        if smB.hasState nt:
          continue
        if not match(nfa.s[nt], c):
          continue
        if nfa.t.allZ[n][nti] == -1'i16:
          smB.add (nt, capt, bounds.a .. i-1)
          continue
        matched = true
        captx = capt
        for z in nfa.t.z[nfa.t.allZ[n][nti]]:
          if not matched:
            break
          case z.kind
          of groupKind:
            capts.add CaptNode(
              parent: captx,
              bound: i,
              idx: z.idx)
            captx = (capts.len-1).int32
          of assertionKind - lookaroundKind:
            matched = match(z, cPrev.Rune, c)
          of lookaroundKind:
            # XXX optimize using a seq[SubMatches] of lookAround size
            var smAl = newSubmatches(z.subExp.nfa.s.len)
            var smBl = newSubmatches(z.subExp.nfa.s.len)
            matched = lookAroundTpl()
          else:
            doAssert false
            discard
        if matched:
          smB.add (nt, captx, bounds.a .. i-1)
    swap smA, smB

  func matchImpl(
    smA, smB: var Submatches,
    capts: var Capts,
    captIdx: var int32,
    text: string,
    nfa: Nfa,
    look: Lookaround,
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
    smA.clear()
    smA.add (0'i16, captIdx, i .. i-1)
    while i < text.len:
      fastRuneAt(text, iNext, c, true)
      nextStateTpl()
      if smA.len == 0:
        return false
      if anchored and nfa.s[smA[0].ni].kind == reEoe:
        captIdx = smA[0].ci
        return true
      i = iNext
      cPrev = c.int32
    c = Rune(-1)
    nextStateTpl()
    if smA.len > 0:
      captIdx = smA[0].ci
    return smA.len > 0

  func reversedMatchImpl(
    smA, smB: var Submatches,
    capts: var Capts,
    captIdx: var int32,
    text: string,
    nfa: Nfa,
    look: Lookaround,
    start = 0,
    flags: set[MatchFlag] = {}
  ): bool =
    doAssert start < len(text)
    var
      c = Rune(-1)
      cPrev = text.runeAt(start).int32
      i = start
      iNext = start
      captx: int32
      matched = false
      anchored = false
    #debugEcho start
    smA.clear()
    smA.add (0'i16, captIdx, i .. i-1)
    while i > 0:
      bwFastRuneAt(text, iNext, c)
      #debugEcho $c
      nextStateTpl()
      if smA.len == 0:
        return false
      if nfa.s[smA[0].ni].kind == reEoe:
        captIdx = smA[0].ci
        return true
      i = iNext
      cPrev = c.int32
    c = Rune(-1)
    nextStateTpl()
    if smA.len > 0:
      captIdx = smA[0].ci
    return smA.len > 0

  const look = Lookaround(
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
      smA = newSubmatches(regex.nfa.s.len)
      smB = newSubmatches(regex.nfa.s.len)
      capts: Capts
      capt = -1'i32
    result = matchImpl(
      smA, smB, capts, capt, text, regex.nfa, look, start)
    if result:
      constructSubmatches(
        m.captures, capts, smA[0].ci, regex.groupsCount)
      if regex.namedGroups.len > 0:
        m.namedGroups = regex.namedGroups
      m.boundaries = smA[0].bounds

  func reversedMatchImpl*(
    smA, smB: var Submatches,
    capts: var Capts,
    captIdx: var int32,
    text: string,
    nfa: Nfa,
    start = 0
  ): bool =
    reversedMatchImpl(
      smA, smB, capts, captIdx, text, nfa, look, start)
