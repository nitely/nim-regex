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

template findMatchBailOut: untyped {.dirty.} =
  if nfa[n].kind == reEoe:
    if not smB.hasState n:
      smB.add (n, capt, bounds)
    # first Eoe in SmA wins, it's pointless to
    # keep matching further than the last Eoe
    break

template notEoe: untyped {.dirty.} =
  when mfFindMatch in flags:
    nfa[nt].kind != reEoe
  else:
    true

func submatch(
  smA, smB: var Submatches,
  capts: var Capts,
  regex: Regex or SubExp,
  i: int,
  cPrev, c, c2: int32,
  flags: static MatchFlags,
) {.inline.} =
  template nfa: untyped = regex.nfa.s
  template t: untyped = regex.nfa.t
  smB.clear()
  var captx: int32
  var matched = true
  for n, capt, bounds in smA.items:
    when mfFindMatch in flags:
      findMatchBailOut()
    for nti, nt in nfa[n].next.pairs:
      if smB.hasState nt:
        continue
      if notEoe() and not match(nfa[nt], c2.Rune):
        continue
      if t.allZ[n][nti] == -1'i16:
        smB.add (nt, capt, bounds.a .. i-1)
        continue
      matched = true
      captx = capt
      for z in t.z[t.allZ[n][nti]]:
        if not matched:
          break
        case z.kind
        of groupKind:
          capts.add CaptNode(
            parent: captx,
            bound: i,
            idx: z.idx)
          captx = (capts.len-1).int32
        of assertionKind - {reLookahead}:
          matched = match(z, cPrev.Rune, c.Rune)
        of reLookahead:
          discard
        else:
          assert false
          discard
      if matched:
        smB.add (nt, captx, bounds.a .. i-1)
  swap smA, smB

template shortestMatch: untyped {.dirty.} =
  submatch(smA, smB, capts, regex, iPrev, cPrev, c.int32, -1'i32, flags)
  if smA.len > 0:
    return true
  swap smA, smB

template findMatch: untyped {.dirty.} =
  when mfFindMatchOpt in flags:
    if smA.len == 0:
      # XXX needed on exit too
      m.boundaries = i .. i-1
      return false
  smA.add((0'i16, -1'i32, i .. i-1))
  if regex.nfa.s[smA[0][0]].kind == reEoe:
    constructSubmatches(m.captures, capts, smA[0][1], regex.groupsCount)
    if regex.namedGroups.len > 0:
      m.namedGroups = regex.namedGroups
    m.boundaries = smA[0].bounds
    return true

func bwRuneAt(s: string, n: int): Rune =
  ## Take rune ending at ``n``
  doAssert n >= 0
  doAssert n <= s.len-1
  var n = n
  while n > 0 and s[n].ord shr 6 == 0b10:
    dec n
  fastRuneAt(s, n, result, false)

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
  smA = newSubmatches(regex.nfa.s.len)
  smB = newSubmatches(regex.nfa.s.len)
  smA.add (0'i16, -1'i32, start .. start-1)
  when mfFindMatch in flags:
    if 0 <= start-1 and start-1 <= len(text)-1:
      cPrev = bwRuneAt(text, start-1).int32
  while i < len(text):
    fastRuneAt(text, i, c, true)
    when mfShortestMatch in flags:
      shortestMatch()
    submatch(smA, smB, capts, regex, iPrev, cPrev, c.int32, c.int32, flags)
    when mfFindMatch in flags:
      findMatch()
    if smA.len == 0:
      return false
    iPrev = i
    cPrev = c.int32
  submatch(smA, smB, capts, regex, iPrev, cPrev, -1'i32, -1'i32, flags)
  if smA.len == 0:
    when mfFindMatchOpt in flags:
      m.boundaries = i .. i-1
    return false
  constructSubmatches(m.captures, capts, smA[0].ci, regex.groupsCount)
  if regex.namedGroups.len > 0:
    m.namedGroups = regex.namedGroups
  m.boundaries = smA[0].bounds
  return true

when false:
  type
    MatchSig = func (
      smA, smB: var Submatches,
      capts: var Capts,
      captIdx: int32,
      text: string,
      nfa: Nfa,
      look: Lookaround,
      start = 0
    ): bool
    Lookaround = object
      ahead, behind: MatchSig

  template nextStateTpl: {.dirty.} =
    smB.clear()
    for n, capt, bounds in smA.items:
      if anchored and
          nfa.s[n].kind == reEoe:
        smB.add (n, capt, bounds)
        break
      for nti, nt in nfa.s[n].next.pairs:
        if smB.hasState nt:
          continue
        if not match(nfa.s[nt], c.Rune):
          continue
        if nfa.t.allZ[n][nti] == -1'i16:
          smB.add (nt, captx, bounds.a .. i-1)
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
          of assertionKind:
            matched = match(z, cPrev.Rune, c.Rune)
          else:
            assert false
            discard
        if matched:
          smB.add (nt, captx, bounds.a .. i-1)
    swap smA, smB

  func matchImpl(
    smA, smB: var Submatches,
    capts: var Capts,
    captIdx: int32,
    text: string,
    nfa: Nfa,
    look: Lookaround,
    start = 0
  ): bool =
    var
      c = Rune(-1)
      cPrev = -1'i32
      i = start
      iPrev = start
      captx: int32
      matched = false
      anchored = false  # XXX take from flag
    smA.clear()
    smA.add (0'i16, captIdx, start .. start-1)
    while i < len(text):
      fastRuneAt(text, i, c, true)
      nextStateTpl()
      if smA.len == 0:
        return false
      if anchored and
          nfa[smA[0].ni].kind == reEoe:
        return true
      iPrev = i
      cPrev = c.int32
    nextStateTpl()
    return smA.len > 0

  func reversedMatchImpl(
    smA, smB: var Submatches,
    capts: var Capts,
    captIdx: int32,
    text: string,
    nfa: Nfa,
    look: Lookaround,
    start = 0
  ): bool =
    doAssert start < len(text)
    var
      c = Rune(-1)
      cPrev = text.runeAt(start).int32
      i = start
      iPrev = start
      captx: int32
      matched = false
    smA.clear()
    smA.add (0'i16, captIdx, i .. i-1)
    while i > 0:
      bwFastRuneAt(text, i, c)
      nextStateTpl()
      if smA.len == 0:
        return false
      if nfa[smA[0].ni].kind == reEoe:
        return true
      iPrev = i
      cPrev = c.int32
    if i > 0:
      bwFastRuneAt(text, i, c)
    else:
      c = Rune(-1)
    nextStateTpl()
    for n, capt, bounds in smA.items:
      if nfa.s[n].kind == reEoe:
        return true
    return false

  func matchImpl*(
    text: string,
    regex: Regex,
    m: var RegexMatch,
    start = 0
  ): bool {.inline.} =
    m.clear()
    let look = Lookaround(
      ahead: matchImpl,
      behind: reversedMatchImpl)
    var
      smA = newSubmatches(regex.nfa.len)
      smB = newSubmatches(regex.nfa.len)
      capts: Capts
    result = matchImpl(
      smA, smB, capts, -1'i32, text, regex.nfa, look, start)
    if result:
      constructSubmatches(
        m.captures, capts, smA[0].ci, regex.groupsCount)
      if regex.namedGroups.len > 0:
        m.namedGroups = regex.namedGroups
      m.boundaries = smA[0].bounds

  func reversedMatchImpl*(
    smA, smB: var Submatches,
    capts: var Capts,
    captIdx: int32,
    text: string,
    nfa: Nfa,
    start = 0
  ): bool {.inline.} =
    let look = Lookaround(
      ahead: matchImpl,
      behind: reversedMatchImpl)
    result = reversedMatchImpl(
      smA, smB, capts, captIdx, text, nfa, look, start)
