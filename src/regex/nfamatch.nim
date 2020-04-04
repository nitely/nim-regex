## NFA matcher for non-static regexes

import unicode
import tables

import nodematch
import nodetype
import nfa
import nfatype

func submatch(
  smA, smB: var Submatches,
  capts: var Capts,
  regex: Regex,
  i: int,
  cPrev, c, c2: int32
) {.inline.} =
  template t: untyped {.dirty.} = regex.transitions
  template nfa: untyped {.dirty.} = regex.nfa
  smB.clear()
  var captx: int32
  var matched = true
  for n, capt, start in smA.items:
    for nti, nt in nfa[n].next.pairs:
      if smB.hasState(nt):
        continue
      if not match(nfa[nt], c2.Rune):
        continue
      if t.allZ[n][nti] == -1'i16:
        smB.add((nt, capt, start))
        continue
      matched = true
      captx = capt
      for z in t.z[t.allZ[n][nti]]:
        if not matched:
          break
        case z.kind
        of groupKind:
          capts.add(CaptNode(
            parent: captx,
            bound: i,
            idx: z.idx))
          captx = (capts.len-1'i32).int32
        of assertionKind:
          matched = match(z, cPrev.Rune, c.Rune)
        else:
          assert false
          discard
      if matched:
        smB.add((nt, captx, start))
  swap smA, smB

template shortestMatch: untyped {.dirty.} =
  submatch(smA, smB, capts, regex, iPrev, cPrev, c.int32, -1'i32)
  if smA.len > 0:
    return true
  swap smA, smB

template longestMatchInit: untyped {.dirty.} =
  var
    matchedLong = false
    captLong = -1'i32
    startLong = start
    iPrevLong = start

template longestMatchEnter: untyped {.dirty.} =
  submatch(smA, smB, capts, regex, iPrev, cPrev, c.int32, -1'i32)
  if smA.len > 0:
    matchedLong = true
    captLong = smA[0][1]
    startLong = smA[0][2]
    iPrevLong = iPrev
  swap smA, smB

template longestMatchExit: untyped {.dirty.} =
  if not matchedLong:
    return false
  assert smA.len == 0
  constructSubmatches(m.captures, capts, captLong, regex.groupsCount)
  if regex.namedGroups.len > 0:
    m.namedGroups = regex.namedGroups
  m.boundaries = startLong .. iPrevLong-1
  return true

template findMatch: untyped {.dirty.} =
  when mfLongestMatch in flags:
    if matchedLong and
        (smA.len == 0 or startLong < smA[0][2]):
      smA.clear()
      longestMatchExit()
  elif mfShortestMatch in flags:
    discard
  else:
    doAssert false
  smA.add((0'i16, -1'i32, i))

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
  when mfLongestMatch in flags:
    longestMatchInit()
  smA = newSubmatches(regex.nfa.len)
  smB = newSubmatches(regex.nfa.len)
  smA.add((0'i16, -1'i32, start))
  while i < len(text):
    fastRuneAt(text, i, c, true)
    when mfShortestMatch in flags:
      shortestMatch()
    when mfLongestMatch in flags:
      longestMatchEnter()
    submatch(smA, smB, capts, regex, iPrev, cPrev, c.int32, c.int32)
    when mfFindMatch in flags:
      findMatch()
    if smA.len == 0:
      when mfLongestMatch in flags:
        longestMatchExit()
      return false
    iPrev = i
    cPrev = c.int32
  submatch(smA, smB, capts, regex, iPrev, cPrev, -1'i32, -1'i32)
  if smA.len == 0:
    when mfLongestMatch in flags:
      longestMatchExit()
    return false
  constructSubmatches(m.captures, capts, smA[0][1], regex.groupsCount)
  if regex.namedGroups.len > 0:
    m.namedGroups = regex.namedGroups
  m.boundaries = smA[0][2] .. iPrev-1
  return true
