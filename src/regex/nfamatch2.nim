## NFA matcher for non-static regexes

import std/unicode
import std/tables
import std/algorithm

import ./common
import ./nodematch
import ./types
import ./nfatype

template nextStateTpl(bwMatch = false): untyped {.dirty.} =
  template bounds2: untyped =
    when bwMatch: i .. bounds.b else: bounds.a .. i-1
  smB.clear()
  for n, capt, bounds in items smA:
    for nti, nt in pairs nfa.s[n].next:
      if smB.hasState nt:
        continue
      if not match(nfa.s[nt], c):
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
            capts.add matches
            captx = (capts.len-1).int32
          else:
            capts.add capts[captx]
            captx = (capts.len-1).int32
          if z.kind == reGroupStart:
            capts[captx][z.idx].a = i
          else:
            capts[captx][z.idx].b = i-1
        of assertionKind - lookaroundKind:
          when bwMatch:
            matched = match(z, c, cPrev.Rune)
          else:
            matched = match(z, cPrev.Rune, c)
        else:
          doAssert false
          discard
      if matched:
        smB.add (nt, captx, bounds2)
  swap smA, smB

func matchImpl(
  smA, smB: var Submatches,
  matches: var seq[Slice[int]],
  text: string,
  nfa: Nfa,
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
  if start-1 in 0 .. text.len-1:
    cPrev = bwRuneAt(text, start-1).int32
  # matches.setLen groups
  for i in 0 .. matches.len-1:
    matches[i] = 0 .. -1
  var capts = newSeq[seq[Slice[int]]]()
  smA.clear()
  smA.add (0'i16, captx, i .. i-1)
  while i < text.len:
    fastRuneAt(text, iNext, c, true)
    nextStateTpl()
    if smA.len == 0:
      return false
    i = iNext
    cPrev = c.int32
  c = Rune(-1)
  nextStateTpl()
  if smA.len > 0:
    if smA[0].ci > 0:
      matches = capts[smA[0].ci]
    if mfReverseCapts in flags:
      matches.reverse()
  return smA.len > 0

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
    capts = newSeq[Slice[int]]()
  capts.setLen regex.groupsCount
  result = matchImpl(
    smA, smB, capts, text, regex.nfa, start)
  if result:
    m.captures.add capts
    if regex.namedGroups.len > 0:
      m.namedGroups = regex.namedGroups
    m.boundaries = smA[0].bounds
