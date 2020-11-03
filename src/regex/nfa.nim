import std/deques

import ./nodetype
import ./common

func check(cond: bool, msg: string) =
  if not cond:
    raise newException(RegexError, msg)

type
  Enfa* = seq[Node]
  Nfa* = seq[Node]
  End = seq[int16]
    ## store all the last
    ## states of a given state.
    ## Avoids having to recurse
    ## a state to find its ends,
    ## but have to keep them up-to-date

func combine(
  nfa: var seq[Node],
  ends: var seq[End],
  org, target: int16
) =
  ## combine ends of ``org``
  ## with ``target``
  for e in ends[org]:
    for i, ni in nfa[e].next.mpairs:
      if nfa[ni].kind == reEOE:
        ni = target
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
  ends[ni].setLen(0)
  for n in next:
    if n == eoe:
        ends[ni].add ni 
    else:
        ends[ni].add ends[n]

func eNfa*(expression: seq[Node]): Enfa {.raises: [RegexError].} =
  ## Thompson's construction
  result = newSeqOfCap[Node](expression.len + 2)
  result.add initEOENode()
  var
    ends = newSeq[End](expression.len + 1)
    states = newSeq[int16]()
  if expression.len == 0:
    states.add eoe
  for n in expression:
    var n = n
    doAssert n.next.len == 0
    check(
      result.high < int16.high,
      ("The expression is too long, " &
       "limit is ~$#") %% $int16.high)
    let ni = result.len.int16
    case n.kind
    of matchableKind, assertionKind:
      n.next.add eoe
      ends.update(ni, [eoe])
      result.add n
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
      result.add n
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
      result.add n
      states.add ni
      if not n.isGreedy:
        swap(result[^1].next[0], result[^1].next[1])
    of reOneOrMore:
      check(
        states.len >= 1,
        "Invalid `+` operator, " &
        "nothing to repeat")
      let stateA = states.pop()
      n.next.add [stateA, eoe]
      ends.update(ni, n.next)
      result.combine(ends, stateA, ni)
      result.add n
      states.add stateA
      if not n.isGreedy:
        swap(result[^1].next[0], result[^1].next[1])
    of reZeroOrOne:
      check(
        states.len >= 1,
        "Invalid `?` operator, " &
        "nothing to make optional")
      let stateA = states.pop()
      n.next.add [stateA, eoe]
      ends.update(ni, n.next)
      result.add n
      states.add ni
      if not n.isGreedy:
        swap(result[^1].next[0], result[^1].next[1])
    of reGroupStart:
      let stateA = states.pop()
      n.next.add stateA
      ends.update(ni, n.next)
      result.add n
      states.add ni
    of reGroupEnd:
      n.next.add eoe
      ends.update(ni, n.next)
      let stateA = states.pop()
      result.combine(ends, stateA, ni)
      result.add n
      states.add stateA
    else:
      doAssert(false, "Unhandled node: $#" %% $n.kind)
  doAssert states.len == 1
  result.add initSkipNode(states)

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
  enfa: Enfa,
  state: int16,
  processing: var seq[int16],
  zTransitions: Zclosure
) =
  var zTransitionsCurr = zTransitions
  if isTransitionZ(enfa[state]):
    zTransitionsCurr.add state
  if enfa[state].kind in matchableKind + {reEOE}:
    result.add((state, zTransitionsCurr))
    return
  for i, s in pairs enfa[state].next:
    # Enter loops only once. "a", re"(a*)*" -> ["a", ""]
    if enfa[state].kind in repetitionKind:
      if s notin processing or i == int(enfa[state].isGreedy):
        processing.add s
        teClosure(result, enfa, s, processing, zTransitionsCurr)
        discard processing.pop()
      # else skip loop
    else:
      teClosure(result, enfa, s, processing, zTransitionsCurr)

func teClosure(
  result: var TeClosure,
  enfa: Enfa,
  state: int16,
  processing: var seq[int16]
) =
  doAssert processing.len == 0
  var zclosure: Zclosure
  for s in enfa[state].next:
    teClosure(result, enfa, s, processing, zclosure)

type
  TransitionsAll* = seq[seq[int16]]
  ZclosureStates* = seq[seq[Node]]
  Transitions* = object
    allZ*: TransitionsAll
    z*: ZclosureStates

func eRemoval*(
  eNfa: Enfa,
  transitions: var Transitions
): Nfa {.raises: [].} =
  ## Remove e-transitions and return
  ## remaining state transtions and
  ## submatches, and zero matches.
  ## Transitions are added in matching order (BFS),
  ## which may help matching performance
  #echo eNfa
  result = newSeqOfCap[Node](eNfa.len)
  transitions.allZ.setLen eNfa.len
  var statesMap = newSeq[int16](eNfa.len)
  for i in 0 .. statesMap.len-1:
    statesMap[i] = -1
  let start = int16(eNfa.len-1)
  result.add eNfa[start]
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
    result[statesMap[qa]].next.setLen 0
    for qb, zclosure in closure.items:
      if statesMap[qb] == -1:
        result.add eNfa[qb]
        statesMap[qb] = result.len.int16-1
      doAssert statesMap[qb] > -1
      doAssert statesMap[qa] > -1
      result[statesMap[qa]].next.add statesMap[qb]
      transitions.allZ[statesMap[qa]].add -1'i16
      zc.setLen 0
      for z in zclosure:
        zc.add eNfa[z]
      if zc.len > 0:
        transitions.z.add zc
        transitions.allZ[statesMap[qa]][^1] = int16(transitions.z.len-1)
      if qb notin qu:
        qu.incl qb
        qw.addFirst qb
  transitions.allZ.setLen result.len

# XXX rename to nfa when Nim v0.19 is dropped
func nfa2*(
  exp: seq[Node],
  transitions: var Transitions
): Nfa {.raises: [RegexError].} =
  result = exp.eNfa.eRemoval(transitions)
