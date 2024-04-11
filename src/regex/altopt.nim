
# xxx rename to oropt

import std/tables
import std/unicode

import ./types

# find OR
# get lit - repeat
# end of lit is EOE or group close
# extract common lit for every term
# reconstruct regex with ORs to substr

func goesToOr(eNfa: Enfa, s: int16): bool =
  result = false
  for ss in eNfa.s[s].next:
    if eNfa.s[ss].kind == reOr:
      return true

func nextOr(eNfa: Enfa, s: int16): int16 =
  for ss in eNfa.s[s].next:
    if eNfa.s[ss].kind == reOr:
      return ss
  doAssert false

func lastOf(eNfa: Enfa, s: int16): (int16, int16) =
  doAssert eNfa.s[s].kind notin {reEoe, reGroupEnd}
  var gSymCount = 0
  var s = s
  var ps = s
  while true:
    doAssert eNfa.s[s].next.len <= 2
    if eNfa.s[s].kind == reEoe:
      return (ps, s)
    if eNfa.s[s].kind == reGroupEnd:
      if gSymCount == 0:
        return (ps, s)
      dec gSymCount
    if eNfa.s[s].kind == reGroupStart:
      inc gSymCount
    ps = s
    if eNfa.s[s].next.len == 2:  # xxx fix greedy
      s = eNfa.s[s].next[0]
    else:
      s = eNfa.s[s].next[0]

func endOfOr(eNfa: Enfa, s: int16): int16 =
  doAssert eNfa.s[s].kind == reOr
  let (_, tend) = eNfa.lastOf(s)
  return tend

func term(eNfa: Enfa, s: int16): int16 =
  doAssert eNfa.s[s].kind == reOr
  return eNfa.s[s].next[0]

func term2(eNfa: Enfa, s: int16): int16 =
  ## grab second term if not OR
  doAssert eNfa.s[s].kind == reOr
  let ss = eNfa.s[s].next[1]
  if eNfa.s[ss].kind != reOr:
    return ss
  return eNfa.s[s].next[0]

func eatOneState(eNfa: var Enfa, s: int16) =
  doAssert eNfa.s[s].kind == reOr
  let ss = eNfa.s[s].next[0]
  doAssert eNfa.s[ss].kind == reChar
  doAssert eNfa.s[ss].next.len == 1
  eNfa.s[s].next[0] = eNfa.s[ss].next[0]

func eatOneState2(eNfa: var Enfa, s: int16) =
  doAssert eNfa.s[s].kind == reOr
  let ss = eNfa.s[s].next[1]
  if eNfa.s[ss].kind == reOr:
    eatOneState(eNfa, s)
    return
  doAssert eNfa.s[ss].kind == reChar
  doAssert eNfa.s[ss].next.len == 1
  eNfa.s[s].next[1] = eNfa.s[ss].next[0]

func altOpt2(
  eNfa: var Enfa,
  state: int16,
  repStates: var Table[int16, int16]
) =
  var s = state
  if eNfa.s[s].kind == reOr:
    var ors = @[s]
    while eNfa.goesToOr(s):
      s = eNfa.nextOr(s)
      ors.add s
    ors.add s
    #debugEcho ors
    s = eNfa.endOfOr(s)
    #debugEcho repr(eNfa.s[s])
    doAssert eNfa.s[s].kind != reOr
    var i = 0
    while i < ors.len-1:
      var t = enfa.term(ors[i])
      if enfa.s[t].kind != reChar:
        inc i
        continue
      #debugEcho repr(enfa.s[t])
      var i2 = i
      let cp = enfa.s[t].cp
      while i2 < ors.len-1:
        var t2 = enfa.term2(ors[i2+1])
        if enfa.s[t2].kind != reChar: break
        if enfa.s[t2].cp != cp: break
        inc i2
      #debugEcho i2
      if i2 > i:
        #debugEcho "OR transformation"
        enfa.s.add toCharNode(cp)
        let cpIdx = (enfa.s.len-1).int16
        enfa.s.add initGroupStart(isCapturing = false)
        let gsIdx = (enfa.s.len-1).int16
        repStates[ors[i]] = cpIdx
        enfa.s[gsIdx].next.add ors[i]
        enfa.s[cpIdx].next.add gsIdx
        enfa.s.add Node(kind: reGroupEnd, cp: ')'.ord.Rune)
        let geIdx = (enfa.s.len-1).int16
        let (tlast, tend) = eNfa.lastOf enfa.term2(ors[i2])
        #debugEcho repr(enfa.s[tlast])
        #debugEcho repr(enfa.s[tend])
        enfa.s[geIdx].next.add tend
        for ii in 0 .. enfa.s[tlast].next.len-1:
          if enfa.s[tlast].next[ii] == tend:
            enfa.s[tlast].next[ii] = geIdx
        for i3 in i .. i2-1:
          enfa.eatOneState(ors[i3])
        enfa.eatOneState2(ors[i2])
        if eNfa.goesToOr(ors[i2]):
          enfa.s[ors[i2-1]].next[1] = enfa.s[ors[i2]].next[0]
          enfa.s[ors[i2]].next[0] = cpIdx
          repStates[ors[i]] = ors[i2]
      i = max(i2+1, i+1)
      #break
  doAssert eNfa.s[s].kind != reOr
  if eNfa.s[s].kind in repetitionKind:
    doAssert eNfa.s[s].next.len == 2
    altOpt2(eNfa, eNfa.s[s].next[0], repStates)  # xxx fix greedy
  else:
    doAssert eNfa.s[s].next.len <= 1
    for ss in eNfa.s[s].next:
      altOpt2(eNfa, ss, repStates)

proc altOpt2(eNfa: Enfa): Enfa =
  #debugEcho "altOpt2"
  result = eNfa
  let start = int16(eNfa.s.len-1)
  var repStates = initTable[int16, int16]()
  altOpt2(result, start, repStates)
  doAssert eNfa.s[start].kind == reSkip
  for s in 0 .. start:
    for i in 0 .. result.s[s].next.len-1:
      let ss = result.s[s].next[i]
      if ss in repStates:
        result.s[s].next[i] = repStates[ss]
  result.s.add result.s[start]
  #debugEcho repr(result.s[result.s.len-1])
  #debugEcho repr(result.s[10])
  #debugEcho repr(result.s[11])

when isMainModule:
  import ./parser
  import ./exptransformation
  import ./nfa

  func altopt(s: string): Nfa =
    let flags: RegexFlags = {}
    var groups: GroupsCapture
    let rpn = s
      .parse(flags)
      .transformExp(groups, flags)
    result = rpn
      .subExps
      .eNfa
      .altOpt2
      .eRemoval

  func toNfa(s: string): Nfa =
    let flags: RegexFlags = {}
    var groups: GroupsCapture
    result = s
      .parse(flags)
      .transformExp(groups, flags)
      .nfa2

  proc toString(
    nfa: Nfa,
    nIdx: int16,
    visited: var set[int16]
  ): string =
    # XXX zero-match transitions are missing
    if nfa.s[nIdx].kind == reEoe:
      result = "eoe"
      return
    if nIdx in visited:
      result = "[...]"
      return
    visited.incl nIdx
    let n = nfa.s[nIdx]
    result = "["
    result.add $n.cp
    for nn in n.next:
      if isEpsilonTransition(nfa.s[nn]):
        continue
      result.add ", "
      result.add toString(nfa, nn, visited)
    result.add "]"

  proc toString(nfa: Nfa): string {.used.} =
    var visited: set[int16]
    result = toString(nfa, 0, visited)

  doAssert r"abc".altopt.toString == r"abc".toNfa.toString
  doAssert r"a|b".altopt.toString == r"a|b".toNfa.toString
  doAssert r"ab|ab|ab".altopt.toString == r"a(b|b|b)".toNfa.toString
  doAssert r"ab|ab".altopt.toString == r"a(b|b)".toNfa.toString
  doAssert r"ab|ab|bc|bc".altopt.toString == r"a(b|b)|b(c|c)".toNfa.toString
  doAssert r"ab|ab|bc|bc".altopt.toString != r"a(b|b|b(c|c))".toNfa.toString
  echo "ok altopt.nim"
