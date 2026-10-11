import std/deques
when defined(nimPreviewSlimSystem):
  import std/assertions

import ./types
import ./common

template check(cond, msg: untyped): untyped =
  {.line: instantiationInfo(fullPaths = true).}:
    if not cond:
      raise newException(RegexError, msg)

type
  End = seq[int16]
    ## store the last
    ## states of a given state.
    ## Avoids having to recurse
    ## a state to find its end,
    ## but have to keep them up-to-date

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

func eNfa*(
  exp: RpnExp,
  reverse = false
): Enfa {.raises: [RegexError].} =
  ## Thompson's construction. If ``reverse`` is true,
  ## the NFA matches the expression backwards
  result = default(Enfa)
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
    of matchableKind, assertionKind, reSkip:
      n.next.add eoe
      ends.update(ni, [eoe])
      result.s.add n
      states.add ni
    of reJoiner:
      var
        stateB = states.pop()
        stateA = states.pop()
      if reverse:
        swap stateA, stateB
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
      if not reverse:
        let stateA = states.pop()
        n.next.add stateA
        ends.update(ni, n.next)
        result.s.add n
        states.add ni
      else:
        n.next.add eoe
        ends.update(ni, n.next)
        let stateA = states.pop()
        result.combine(ends, stateA, ni)
        result.s.add n
        states.add stateA
    of reGroupEnd:
      if not reverse:
        n.next.add eoe
        ends.update(ni, n.next)
        let stateA = states.pop()
        result.combine(ends, stateA, ni)
        result.s.add n
        states.add stateA
      else:
        let stateA = states.pop()
        n.next.add stateA
        ends.update(ni, n.next)
        result.s.add n
        states.add ni
    else:
      doAssert(false, "Unhandled node: $#" %% $n.kind)
  doAssert states.len == 1
  result.s.add initSkipNode(states)

type
  Transitions = seq[int16]
  TeClosure = seq[(int16, Transitions)]
  TecVisit = object
    processing: seq[int16]
    assertions: set[NodeKind]
  TecVisited = seq[seq[TecVisit]]

func isEpsilonTransition2(n: Node): bool {.inline.} =
  result = case n.kind
    of groupKind:
      n.isCapturing
    of assertionKind:
      true
    else:
      false

func assertions(eNfa: Enfa, transitions: Transitions): set[NodeKind] =
  result = {}
  for t in transitions:
    if eNfa.s[t].kind in assertionKind:
      result.incl eNfa.s[t].kind

func teClosure(
  result: var TeClosure,
  eNfa: Enfa,
  state: int16,
  processing: var seq[int16],
  visited: var TecVisited,
  transitions: Transitions
) =
  let assertions = assertions(eNfa, transitions)
  for visit in visited[state]:
    if visit.processing == processing and visit.assertions <= assertions:
      return
  if assertions * lookaroundKind == {}:
    visited[state].add TecVisit(
      processing: processing,
      assertions: assertions)
  var transitionsCurr = transitions
  if isEpsilonTransition2 eNfa.s[state]:
    transitionsCurr.add state
  if eNfa.s[state].kind in matchableKind + {reEOE}:
    result.add (state, transitionsCurr)
    return
  for i, s in pairs eNfa.s[state].next:
    # Enter loops only once. "a", re"(a*)*" -> ["a", ""]
    if eNfa.s[state].kind in repetitionKind:
      if s notin processing or i == int(eNfa.s[state].isGreedy):
        processing.add s
        teClosure(result, eNfa, s, processing, visited, transitionsCurr)
        discard processing.pop()
      # else skip loop
    else:
      teClosure(result, eNfa, s, processing, visited, transitionsCurr)

func teClosure(
  result: var TeClosure,
  eNfa: Enfa,
  state: int16,
  processing: var seq[int16],
  visited: var TecVisited
) =
  doAssert processing.len == 0
  for v in mitems visited:
    v.setLen 0
  var transitions = default(Transitions)
  for s in eNfa.s[state].next:
    teClosure(result, eNfa, s, processing, visited, transitions)

func eRemoval*(eNfa: Enfa): Nfa {.raises: [].} =
  ## Remove e-transitions and return
  ## remaining state transtions and
  ## submatches, and zero matches.
  ## Transitions are added in matching order (BFS),
  ## which may help matching performance
  #echo eNfa
  result = default(Nfa)
  result.s = newSeq[Node](eNfa.s.len)
  result.s.setLen 0
  var statesMap = newSeq[int16](eNfa.s.len)
  for i in 0 .. statesMap.len-1:
    statesMap[i] = -1
  let start = int16(eNfa.s.len-1)
  result.s.add eNfa.s[start]
  statesMap[start] = 0'i16
  var closure = default(TeClosure)
  var qw = initDeque[int16](2)
  qw.addFirst start
  var qu: set[int16] = {}
  qu.incl start
  var qa = 0'i16
  var processing = newSeqOfCap[int16](8)
  var visited = newSeq[seq[TecVisit]](eNfa.s.len)
  while qw.len > 0:
    try:
      qa = qw.popLast()
    except IndexDefect:
      doAssert false
    closure.setLen 0
    teClosure(closure, eNfa, qa, processing, visited)
    doAssert statesMap[qa] > -1
    result.s[statesMap[qa]].next.setLen 0
    for qb, transitions in closure.items:
      if statesMap[qb] == -1:
        result.s.add eNfa.s[qb]
        statesMap[qb] = result.s.len.int16-1
      result.s[statesMap[qa]].next.add statesMap[qb]
      for eti in transitions:
        if statesMap[eti] == -1:
          result.s.add eNfa.s[eti]
          statesMap[eti] = result.s.len.int16-1
        result.s[statesMap[qa]].next.add statesMap[eti]
      if qb notin qu:
        qu.incl qb
        qw.addFirst qb

func subExps*(exp: RpnExp): RpnExp =
  result = exp
  for n in mitems result.s:
    case n.kind
    of reLookahead, reNotLookahead:
      n.subExp.nfa = n.subExp.rpn
        .subExps
        .eNfa
        .eRemoval
      n.subExp.rpn.s = newSeq[Node]()  # nullify
    of reLookbehind, reNotLookbehind:
      n.subExp.nfa = n.subExp.rpn
        .subExps
        .eNfa(reverse = true)
        .eRemoval
      n.subExp.rpn.s = newSeq[Node]()  # nullify
    else:
      discard

# XXX rename to nfa when Nim v0.19 is dropped
func nfa2*(exp: RpnExp): Nfa {.raises: [RegexError].} =
  exp.subExps.eNfa.eRemoval
