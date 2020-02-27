## DFA matcher for non-static regexes

import unicode
import sets
import tables
import deques
import algorithm

import unicodeplus except isUpper, isLower

import nodematch
import nodetype
import common
import nfa
import dfa

type
  CaptNode* = object
    parent*: int
    bound*: int
    idx*: int
  Capts* = seq[CaptNode]
  Captures* = seq[seq[Slice[int]]]

func constructSubmatches*(
  captures: var Captures,
  capts: Capts,
  capt, size: int
) {.inline.} =
  template currGroup: untyped = captures[capts[capt].idx]
  captures.setLen(size)
  for i in 0 .. captures.len-1:
    captures[i].setLen(0)
  if capts.len == 0:
    return
  var capt = capt
  while capt != -1:
    if currGroup.len == 0:
      currGroup.add(-2 .. -2)
    if currGroup[^1].a != -2:
      currGroup.add(-2 .. -2)
    if currGroup[^1].b == -2:
      currGroup[^1].b = capts[capt].bound-1
    else:
      currGroup[^1].a = capts[capt].bound
    capt = capts[capt].parent
  for c in captures.mitems:
    c.reverse()

type
  NodeIdx = int16
  CaptIdx = int32
  Submatches* = ref object
    ## Parallel states would be a better name
    sx: seq[(NodeIdx, CaptIdx)]
    # use custom len because setLen(0) is slower,
    # and {.noInit.} makes no difference
    si: int
    ss: set[int16]

func newSubmatches*(): Submatches {.inline.} =
  result = new Submatches
  result.sx = newSeq[(NodeIdx, CaptIdx)](8)
  result.si = 0

func `[]`*(sm: Submatches, i: int): (NodeIdx, CaptIdx) {.inline.} =
  assert i < sm.si
  sm.sx[i]

func add*(sm: var Submatches, item: (NodeIdx, CaptIdx)) {.inline.} =
  assert item[0] notin sm.ss
  assert sm.si <= sm.sx.len
  if (sm.si == sm.sx.len).unlikely:
    sm.sx.setLen(sm.sx.len * 2)
  sm.sx[sm.si] = item
  sm.si += 1 
  sm.ss.incl(item[0])

func len*(sm: Submatches): int {.inline.} =
  sm.si

func hasState*(sm: Submatches, n: int16): bool {.inline.} =
  n in sm.ss

func clear*(sm: var Submatches) {.inline.} =
  for i in 0 .. sm.len-1:
    assert sm.sx[i][0] in sm.ss
    sm.ss.excl sm.sx[i][0]
  sm.si = 0

iterator items*(sm: Submatches): (NodeIdx, CaptIdx) {.inline.} =
  for i in 0 .. sm.len-1:
    yield sm.sx[i]

func submatch(
  smA, smB: var Submatches,
  capts: var Capts,
  transitions: Transitions,
  states: Closure,
  i: int,
  cprev, c: int32
) {.inline.} =
  smB.clear()
  var captx: int32
  var matched = true
  for n, capt in smA.items:
    for nti, nt in transitions.all[n].pairs:
      if smB.hasState(nt):
        continue
      if nt notin states:
        continue
      if transitions.allZ[n][nti] == -1'i16:
        smB.add((nt, capt))
        continue
      matched = true
      captx = capt
      for z in transitions.z[transitions.allZ[n][nti]]:
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
          matched = match(z, cprev.Rune, c.Rune)
        of matchTransitionKind:
          matched = match(z, c.Rune)
        else:
          assert false
          discard
      if matched:
        smB.add((nt, captx))
  swap(smA, smB)

type
  RegexFlag* = enum
    reAscii
  Regex* = object
    ## a compiled regular expression
    dfa*: Dfa
    transitions*: Transitions
    groupsCount*: int16
    namedGroups*: OrderedTable[string, int16]
    flags*: set[RegexFlag]
  MatchFlag* = enum
    mfShortestMatch
    mfLongestMatch
    mfNoCaptures
  MatchFlags* = set[MatchFlag]
  RegexMatch* = object
    ## result from matching operations
    captures*: Captures
    namedGroups*: OrderedTable[string, int16]
    boundaries*: Slice[int]

func clear*(m: var RegexMatch) {.inline.} =
  if m.captures.len > 0:
    m.captures.setLen(0)
  if m.namedGroups.len > 0:
    m.namedGroups.clear()
  m.boundaries = 0 .. -1

# Order matters, subsets first
const syms* = [
  symDigit,
  symWord,
  symAny,
  symAnyNl
]

# Slow match
func symMatch(
  q: var int32,
  c: Rune,
  cSym: var int32,
  regex: Regex
) {.inline.} =
  var matched = false
  for sym in syms:
    if sym notin regex.dfa.table[q]:
      continue
    matched = case sym:
      of symDigit: c.isDecimal()
      of symWord: c.isWord()
      of symAny: c != lineBreakRune
      of symAnyNl: true
      else: false
    if matched:
      q = regex.dfa.table[q][sym]
      cSym = sym
      break
  if not matched:
    q = -1'i32

# Can't return early because of boundaries
template longestMatchEnter(): untyped {.dirty.} =
  if symEoe in regex.dfa.table[q]:
    matchedLong = true
    iPrevLong = iPrev
    if regex.transitions.z.len > 0:
      submatch(
        smA, smB, capts, regex.transitions,
        regex.dfa.cs[regex.dfa.closures[q][symEoe]], iPrev, cPrev, c.int32)
      if smA.len > 0:
        captLong = smA[0][1]
      swap(smA, smB)

template longestMatchExit(): untyped {.dirty.} =
  result = matchedLong
  if regex.transitions.z.len > 0:
    constructSubmatches(m.captures, capts, captLong, regex.groupsCount)
    if regex.namedGroups.len > 0:
      m.namedGroups = regex.namedGroups
  m.boundaries = start .. iPrevLong-1
  return

template shortestMatch(): untyped {.dirty.} =
  if symEoe in regex.dfa.table[q]:
    if regex.transitions.z.len > 0:
      submatch(
        smA, smB, capts, regex.transitions,
        regex.dfa.cs[regex.dfa.closures[q][symEoe]], iPrev, cPrev, c.int32)
      if smA.len > 0:
        result = true
        return
      swap(smA, smB)
    else:
      result = true
      return

func matchImpl*(
  text: string,
  regex: Regex,
  m: var RegexMatch,
  flags: static MatchFlags,
  start = 0
): bool {.inline.} =
  #echo dfa
  m.clear()
  result = false
  let asciiMode = reAscii in regex.flags
  var
    smA: Submatches
    smB: Submatches
    capts: Capts
    c: Rune
    cPrev = -1'i32
    cSym: int32
    q = 0'i32
    qnext = 0'i32
    i = start
    iPrev = start
    # Long match
    matchedLong {.used.} = false
    captLong {.used.} = -1
    iPrevLong {.used.} = start
  if regex.transitions.z.len > 0:
    smA = newSubmatches()
    smB = newSubmatches()
    smA.add((0'i16, -1'i32))
  #echo regex.dfa
  while i < len(text):
    if not asciiMode:
      fastRuneAt(text, i, c, true)
    else:
      c = Rune(text[i])
      inc i
    when mfShortestMatch in flags:
      shortestMatch()
    when mfLongestMatch in flags:
      longestMatchEnter()
    cSym = c.int32
    if (c.int32 in regex.dfa.table[q]).likely:
      qnext = regex.dfa.table[q][c.int32]
    else:
      if not asciiMode:
        symMatch(qnext, c, cSym, regex)
      if qnext == -1 or asciiMode:
        when mfLongestMatch in flags:
          longestMatchExit()
        else:
          return
    if regex.transitions.z.len > 0:
      submatch(
        smA, smB, capts, regex.transitions,
        regex.dfa.cs[regex.dfa.closures[q][cSym]], iPrev, cPrev, c.int32)
    iPrev = i
    cPrev = c.int32
    q = qnext
    #echo q
  result = symEoe in regex.dfa.table[q]
  if not result:
    when mfLongestMatch in flags:
      longestMatchExit()
    return
  if regex.transitions.z.len > 0:
    submatch(
      smA, smB, capts, regex.transitions,
      regex.dfa.cs[regex.dfa.closures[q][symEoe]], iPrev, cPrev, -1'i32)
    if smA.len == 0:  # XXX is this possible?
      when mfLongestMatch in flags:
        longestMatchExit()
      result = false
      return
    constructSubmatches(m.captures, capts, smA[0][1], regex.groupsCount)
    if regex.namedGroups.len > 0:
      m.namedGroups = regex.namedGroups
  m.boundaries = start .. iPrev-1
