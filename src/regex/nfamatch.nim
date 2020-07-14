## NFA matcher for non-static regexes

# Pro-tip: try modifications here and
# run tests passing `-d:forceRegexAtRuntime`.
# Then port the changes to nfamacro. Hacking
# with nfamatch is a lot easier.

import std/unicode
import std/tables
from std/strutils import find

import nodematch
import nodetype
import nfatype

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
  regex: Regex,
  i: int,
  cPrev, c, c2: int32,
  flags: static MatchFlags,
) {.inline.} =
  template t: untyped = regex.transitions
  template nfa: untyped = regex.nfa
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
        of assertionKind:
          matched = match(z, cPrev.Rune, c.Rune)
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
  if regex.nfa[smA[0][0]].kind == reEoe:
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
  smA = newSubmatches(regex.nfa.len)
  smB = newSubmatches(regex.nfa.len)
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
    return false
  constructSubmatches(m.captures, capts, smA[0].ci, regex.groupsCount)
  if regex.namedGroups.len > 0:
    m.namedGroups = regex.namedGroups
  m.boundaries = smA[0].bounds
  return true

when true:
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

  func submatch2(
    smA, smB: var Submatches,
    regex: Regex,
    i: int,
    cPrev, c, c2: int32
  ) =
    template nfa: untyped = regex.litOpt.nfa
    template tns: untyped = regex.litOpt.tns
    smB.clear()
    var matched = true
    for n, capt, bounds in smA.items:
      findMatchBailOut()
      for nti, nt in nfa[n].next.pairs:
        if smB.hasState(nt):
          continue
        debugEcho nfa[nt].kind
        if nfa[nt].kind != reEoe and not match(nfa[nt], c2.Rune):
          continue
        if tns.allZ[n][nti] == -1'i16:
          smB.add((nt, capt, i .. bounds.b))
          continue
        matched = true
        for z in tns.z[tns.allZ[n][nti]]:
          if z.kind in assertionKind:
            matched = match(z, cPrev.Rune, c.Rune)
          if not matched:
            break
        if matched:
          smB.add((nt, capt, i .. bounds.b))
    swap smA, smB

  func matchImpl2*(
    text: string,
    regex: Regex,
    m: var RegexMatch,
    start = 0
  ): bool {.inline.} =
    template nfa: untyped = regex.litOpt.nfa
    var
      smA, smB: Submatches
      c = Rune(-1)
      cPrev = -1'i32
      i = start
      iPrev = start
    smA = newSubmatches(nfa.len)
    smB = newSubmatches(nfa.len)
    smA.add((0'i16, -1'i32, start .. start-1))
    while i > 0:
      bwFastRuneAt(text, i, c)
      debugEcho "txt.Rune=", c
      debugEcho "txt.i=", i
      submatch2(smA, smB, regex, iPrev, cPrev, c.int32, c.int32)
      if smA.len == 0:
        return false
      if nfa[smA[0][0]].kind == reEoe:
        m.boundaries = smA[0][2]
        return true
      iPrev = i
      cPrev = c.int32
    submatch2(smA, smB, regex, iPrev, cPrev, -1'i32, -1'i32)
    if smA.len == 0:
      return false
    m.boundaries = smA[0][2]
    return true

  func findImpl*(
    text: string,
    regex: Regex,
    m: var RegexMatch,
    start: int
  ): bool =
    template opt: untyped = regex.litOpt
    result = false
    var start = start
    while start < len(text):
      debugEcho "lit=", opt.lit
      debugEcho "start=", start
      var litIdx = text.find(opt.lit.char, start)
      if litIdx == -1:
        return false
      debugEcho "litIdx=", litIdx
      doAssert litIdx >= start
      start = litIdx
      if not matchImpl2(text, regex, m, start):
        debugEcho "not.Match=", start
        start = litIdx+1
      else:
        #doAssert start <= m.boundaries.a
        debugEcho "bounds=", m.boundaries
        start = m.boundaries.a
        if matchImpl(text, regex, m, {mfFindMatch, mfFindMatchOpt}, start):
          return true
        debugEcho "bound.b=", m.boundaries.b
        start = m.boundaries.b
