import std/deques

import ./exptype
import ./nodetype
import ./common

func check(cond: bool, msg: string) =
  if not cond:
    raise newException(RegexError, msg)

type
  Enfa* = object
    s*: seq[Node]

type
  End = seq[int16]
    ## store the last
    ## states of a given state.
    ## Avoids having to recurse
    ## a state to find its end,
    ## but have to keep them up-to-date

type
  TransitionsAll* = seq[seq[int16]]
  ZclosureStates* = seq[seq[Node]]
  Transitions* = object
    allZ*: TransitionsAll
    z*: ZclosureStates
  Nfa* = object
    s*: seq[Node]
    t*: Transitions

func combine(
  eNfa: var Enfa,
  ends: var seq[End],
  org, target: int16
) =
  ## combine ends of ``org``
  ## with ``target``
  for e in ends[org]:
    for i in mitems eNfa.s[e].next:
      if eNfa.s[i].kind == reEOE:
        i = target
  ends[org] = ends[target]

const eoe = 0'i16

func update(
  ends: var seq[End],
  ni: int16,
  next: openArray[int16]
) =
  ## update the ends of Node ``ni``
  ## to point to the end of the nodes in next.
  ## If a node in next is Eoe,
  ## the end of ``ni`` will point to itself
  ends[ni].setLen 0
  for n in next:
    if n == eoe:
      ends[ni].add ni 
    else:
      ends[ni].add ends[n]

func eNfa*(exp: RpnExp): Enfa {.raises: [RegexError].} =
  ## Thompson's construction
  result.s = newSeq[Node](exp.s.len + 2)
  result.s.setLen 0
  result.s.add initEOENode()
  var
    ends = newSeq[End](exp.s.len + 1)
    states = newSeq[int16]()
  if exp.s.len == 0:
    states.add eoe
  for n in exp.s:
    var n = n
    doAssert n.next.len == 0
    check(
      result.s.high < int16.high,
      ("The expression is too long, " &
       "limit is ~$#") %% $int16.high)
    let ni = result.s.len.int16
    case n.kind
    of matchableKind, assertionKind:
      n.next.add eoe
      ends.update(ni, [eoe])
      result.s.add n
      states.add ni
    of reJoiner:
      let
        stateB = states.pop()
        stateA = states.pop()
      result.combine(ends, stateA, stateB)
      states.add stateA
    of reOr:
      check(
        states.len >= 2,
        "Invalid OR conditional, nothing to " &
        "match at right/left side of the condition")
      let
        stateB = states.pop()
        stateA = states.pop()
      n.next.add [stateA, stateB]
      ends.update(ni, n.next)
      result.s.add n
      states.add ni
    of reZeroOrMore:
      check(
        states.len >= 1,
        "Invalid `*` operator, " &
        "nothing to repeat")
      let stateA = states.pop()
      n.next.add [stateA, eoe]
      ends.update(ni, n.next)
      result.combine(ends, stateA, ni)
      result.s.add n
      states.add ni
      if not n.isGreedy:
        swap result.s[^1].next[0], result.s[^1].next[1]
    of reOneOrMore:
      check(
        states.len >= 1,
        "Invalid `+` operator, " &
        "nothing to repeat")
      let stateA = states.pop()
      n.next.add [stateA, eoe]
      ends.update(ni, n.next)
      result.combine(ends, stateA, ni)
      result.s.add n
      states.add stateA
      if not n.isGreedy:
        swap result.s[^1].next[0], result.s[^1].next[1]
    of reZeroOrOne:
      check(
        states.len >= 1,
        "Invalid `?` operator, " &
        "nothing to make optional")
      let stateA = states.pop()
      n.next.add [stateA, eoe]
      ends.update(ni, n.next)
      result.s.add n
      states.add ni
      if not n.isGreedy:
        swap result.s[^1].next[0], result.s[^1].next[1]
    of reGroupStart:
      let stateA = states.pop()
      n.next.add stateA
      ends.update(ni, n.next)
      result.s.add n
      states.add ni
    of reGroupEnd:
      n.next.add eoe
      ends.update(ni, n.next)
      let stateA = states.pop()
      result.combine(ends, stateA, ni)
      result.s.add n
      states.add stateA
    else:
      doAssert(false, "Unhandled node: $#" %% $n.kind)
  doAssert states.len == 1
  result.s.add initSkipNode(states)

type
  Zclosure = seq[int16]
  TeClosure = seq[(int16, Zclosure)]

func isTransitionZ(n: Node): bool {.inline.} =
  result = case n.kind
    of groupKind:
      n.isCapturing
    of assertionKind:
      true
    else:
      false

func teClosure(
  result: var TeClosure,
  eNfa: Enfa,
  state: int16,
  processing: var seq[int16],
  zTransitions: Zclosure
) =
  var zTransitionsCurr = zTransitions
  if isTransitionZ eNfa.s[state]:
    zTransitionsCurr.add state
  if eNfa.s[state].kind in matchableKind + {reEOE}:
    result.add (state, zTransitionsCurr)
    return
  for i, s in pairs eNfa.s[state].next:
    # Enter loops only once. "a", re"(a*)*" -> ["a", ""]
    if eNfa.s[state].kind in repetitionKind:
      if s notin processing or i == int(eNfa.s[state].isGreedy):
        processing.add s
        teClosure(result, eNfa, s, processing, zTransitionsCurr)
        discard processing.pop()
      # else skip loop
    else:
      teClosure(result, eNfa, s, processing, zTransitionsCurr)

func teClosure(
  result: var TeClosure,
  eNfa: Enfa,
  state: int16,
  processing: var seq[int16]
) =
  doAssert processing.len == 0
  var zclosure: Zclosure
  for s in eNfa.s[state].next:
    teClosure(result, eNfa, s, processing, zclosure)

func eRemoval*(eNfa: Enfa): Nfa {.raises: [].} =
  ## Remove e-transitions and return
  ## remaining state transtions and
  ## submatches, and zero matches.
  ## Transitions are added in matching order (BFS),
  ## which may help matching performance
  #echo eNfa
  result.s = newSeq[Node](eNfa.s.len)
  result.s.setLen 0
  result.t.allZ.setLen eNfa.s.len
  var statesMap = newSeq[int16](eNfa.s.len)
  for i in 0 .. statesMap.len-1:
    statesMap[i] = -1
  let start = int16(eNfa.s.len-1)
  result.s.add eNfa.s[start]
  statesMap[start] = 0'i16
  var closure: TeClosure
  var zc: seq[Node]
  var qw = initDeque[int16](2)
  qw.addFirst start
  var qu: set[int16]
  qu.incl start
  var qa: int16
  var processing = newSeqOfCap[int16](8)
  while qw.len > 0:
    try:
      qa = qw.popLast()
    except IndexError:
      doAssert false
    closure.setLen 0
    teClosure(closure, eNfa, qa, processing)
    result.s[statesMap[qa]].next.setLen 0
    for qb, zclosure in closure.items:
      if statesMap[qb] == -1:
        result.s.add eNfa.s[qb]
        statesMap[qb] = result.s.len.int16-1
      doAssert statesMap[qb] > -1
      doAssert statesMap[qa] > -1
      result.s[statesMap[qa]].next.add statesMap[qb]
      result.t.allZ[statesMap[qa]].add -1'i16
      zc.setLen 0
      for z in zclosure:
        zc.add eNfa.s[z]
      if zc.len > 0:
        result.t.z.add zc
        result.t.allZ[statesMap[qa]][^1] = int16(result.t.z.len-1)
      if qb notin qu:
        qu.incl qb
        qw.addFirst qb
  result.t.allZ.setLen result.s.len

# XXX rename to nfa when Nim v0.19 is dropped
func nfa2*(exp: RpnExp): Nfa {.raises: [RegexError].} =
  exp.eNfa.eRemoval
