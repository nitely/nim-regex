## Literals optimization.
## This is about optimizing the find operation
## by quickly searching Regular Expression literals
## within the input string. See issue #59.

#[
Only optimizations that are guaranteed to run in linear
time are implemented. We must also ensure the matches
are not overlapped.

Here’s a high-level description of the algorithm:
  * we pick a literal that is memchr'ed
    to skip parts of the text
  * the prefix is the regex part before
    the literal; none of the characters
    or symbols can match the literal
  * the prefix is run backwards to find
    the start of the match
  * a full findAll is run from the start
    of the match until a character that
    cannot be matched is found (safe break point)
    or the end is reached

How is quadratic runtime avoided?
  * None of the prefix characters
    or symbols can match the literal;
    hence the literal is a delimeter for
    the prefix
  * In cases where a literal is found, but
    not match is found, the next prefix match
    is limited by the literal delimiter;
    the runtime is O(2N) at most
  * In cases where a match is found, the next
    literal is searched from the last character
    consumed by the matcher
  * The prefix matcher is also limited by the
    end boundary of the last match found; this
    prevents overlapped matches;
  * The `overlapTests` in tests.nim are good examples
    of this

There used to be a list of regex examples here, but
I wrote a blog post explaining things better, see
https://nitely.github.io/2020/11/30/regex-literals-optimization.html

]#

import std/algorithm
import std/sets
import std/unicode

import ./types
import ./nodematch
import ./nfa

when (NimMajor, NimMinor, NimPatch) < (0, 20, 0):
  import common

type
  LitNfa = Enfa
  End = seq[int16]

func combine(
  eNfa: var Enfa,
  ends: var seq[End],
  org, target: int16
) =
  for e in ends[org]:
    for i in mitems eNfa.s[e].next:
      if eNfa.s[i].kind == reEoe:
        i = target
  ends[org] = ends[target]

const eoe = 0'i16

func update(
  ends: var seq[End],
  ni: int16,
  next: openArray[int16]
) =
  ends[ni].setLen 0
  for n in next:
    if n == eoe:
        ends[ni].add ni
    else:
        ends[ni].add ends[n]

# Keep lits,
# replace (...)+, (...)*, (...)?,
# and (...|...) by skip nodes.
# Based on Thompson's construction
func toLitNfa(exp: RpnExp): LitNfa =
  result.s = newSeq[Node](exp.s.len + 2)
  result.s.setLen 0
  result.s.add initEoeNode()
  var
    ends = newSeq[End](exp.s.len + 1)
    states = newSeq[int16]()
  if exp.s.len == 0:
    states.add eoe
  for n in exp.s:
    var n = n
    doAssert n.next.len == 0
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
      discard states.pop()
      discard states.pop()
      ends.update(ni, [eoe])
      result.s.add initSkipNode([eoe])
      states.add ni
    of reZeroOrMore, reZeroOrOne, reOneOrMore:
      discard states.pop()
      ends.update(ni, [eoe])
      result.s.add initSkipNode([eoe])
      states.add ni
    of reGroupStart:
      discard
    of reGroupEnd:
      discard
    else:
      doAssert false
  doAssert states.len == 1
  result.s.add initSkipNode(states)

type
  NodeIdx = int16

func lonelyLit(exp: RpnExp): NodeIdx =
  template state: untyped = litNfa.s[stateIdx]
  result = -1
  let litNfa = exp.toLitNfa()
  var cpSeen = initHashSet[Rune](16)
  var lits = newSeq[int16]()
  var stateIdx = litNfa.s.len.int16-1
  while state.kind != reEoe:
    if state.kind == reChar and
        state.cp.int <= 127:  # XXX support unicode
      if state.cp notin cpSeen:
        cpSeen.incl state.cp
        lits.add stateIdx
    doAssert state.next.len == 1
    stateIdx = state.next[0]
  assert lits == sorted(lits, system.cmp)
  # The algo takes O(litsLen * NfaSize)
  # time, it seems sensible to limit the lits
  lits.setLen min(lits.len, 128)
  var litsTmp = newSeq[int16]()
  for ni, n in pairs exp.s:
    # break after last lit of first lits sequence
    if result > -1 and exp.s[result].uid+1 < n.uid:
      break
    if n.kind notin matchableKind:
      continue
    for nlit in lits:
      doAssert n.uid <= litNfa.s[nlit].uid
      if n.uid == litNfa.s[nlit].uid:
        result = ni.NodeIdx
        #return
      if not match(n, litNfa.s[nlit].cp):
        litsTmp.add nlit
    swap lits, litsTmp
    litsTmp.setLen 0

func prefix(eNfa: Enfa, uid: NodeUid): Enfa =
  template state0: untyped = eNfa.s.len.int16-1
  result = eNfa
  for n in result.s.mitems:
    n.next.setLen 0
  # reverse transitions; DFS
  var stack = @[(state0, -1'i16)]
  var visited: set[int16]
  template state: untyped = eNfa.s[ni]
  while stack.len > 0:
    let (ni, pi) = stack.pop()
    if pi > -1:
      result.s[ni].next.add pi
    if ni in visited:
      continue
    visited.incl ni
    # we only care about the prefix
    if state.uid == uid:
      continue
    for mi in state.next:
      stack.add (mi, ni)
  for n in mitems result.s:
    n.next.reverse
    n.isGreedy = true
  # Swap initial state by eoe
  var eoeIdx = -1'i16
  for ni, n in pairs result.s:
    if n.kind == reEoe:
      doAssert eoeIdx == -1
      eoeIdx = ni.int16
  doAssert eoeIdx != -1
  for ni in eNfa.s[state0].next:
    for i in 0 .. result.s[ni].next.len-1:
      if result.s[ni].next[i] == state0:
        result.s[ni].next[i] = eoeIdx
  doAssert result.s[eoeIdx].kind == reEoe
  doAssert result.s[state0].kind == reSkip
  swap result.s[state0].kind, result.s[eoeIdx].kind
  swap result.s[state0], result.s[eoeIdx]
  # cut
  var nIdx = -1
  for ni, n in pairs eNfa.s:
    if n.uid == uid:
      doAssert nIdx == -1
      nIdx = ni
  doAssert nIdx != -1
  result.s[state0].next = result.s[nIdx].next

type
  LitOpt* = object
    lit*: Rune
    nfa*: Nfa

func canOpt*(litOpt: LitOpt): bool =
  return litOpt.nfa.s.len > 0

func litopt2*(exp: RpnExp): LitOpt =
  template litNode: untyped = exp.s[litIdx]
  let litIdx = exp.lonelyLit()
  if litIdx == -1:
    return
  result.lit = litNode.cp
  result.nfa = exp
    .subExps
    .eNfa
    .prefix(litNode.uid)
    .eRemoval

when isMainModule:
  from unicode import toUtf8, `$`
  import parser
  import exptransformation

  func rpn(s: string, groups: var GroupsCapture): RpnExp =
    result = s
      .parse
      .transformExp(groups)
  func rpn(s: string): RpnExp =
    var groups: GroupsCapture
    rpn(s, groups)

  func lit(s: string): string =
    let opt = s.rpn.litopt2
    if not opt.canOpt: return ""
    return opt.lit.toUtf8

  func prefix(s: string): Nfa =
    let opt = s.rpn.litopt2
    if not opt.canOpt: return
    return opt.nfa

  func toNfa(s: string): Nfa =
    var groups: GroupsCapture
    result = s
      .rpn(groups)
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
      result.add ", "
      result.add toString(nfa, nn, visited)
    result.add "]"

  proc toString(nfa: Nfa): string {.used.} =
    var visited: set[int16]
    result = toString(nfa, 0, visited)

  doAssert lit"abc" == "c"
  doAssert lit"abcab" == "c"
  doAssert lit"abc\wxyz" == "c"
  doAssert lit"(abc)+" == ""
  doAssert lit"(abc)+xyz" == "z"
  doAssert lit"(abc)*" == ""
  doAssert lit"(abc)*xyz" == "z"
  doAssert lit"(a|b)" == ""
  doAssert lit"(a+|b)" == ""
  doAssert lit"(a+|b+)" == ""
  doAssert lit"((abc)|(xyz))" == ""
  doAssert lit"((abc)+|(xyz)+)" == ""
  doAssert lit"((abc)*|(xyz)*)" == ""
  doAssert lit"a?" == ""
  doAssert lit"a?b" == "b"
  doAssert lit"a" == "a"
  doAssert lit"(a|b)xyz" == "z"
  doAssert lit"(a|bc)xyz" == "z"
  doAssert lit"(ab|c)xyz" == "z"
  doAssert lit"a+b" == "b"
  doAssert lit"a*b" == "b"
  doAssert lit"b+b" == ""
  doAssert lit"b*b" == ""
  doAssert lit"\d+x" == "x"
  doAssert lit"\d*x" == "x"
  doAssert lit"\d?x" == "x"
  doAssert lit"\w+x" == ""
  doAssert lit"\w*x" == ""
  doAssert lit"\w?x" == ""
  doAssert lit"(\w\d)*abc" == ""
  doAssert lit"(\w\d)+abc" == ""
  doAssert lit"(\w\d)?abc" == ""
  doAssert lit"z(x|y)+abc" == "z"
  doAssert lit"((a|b)c*d?(ef)*\w\d\b@)+" == ""
  doAssert lit"((a|b)c*d?(ef)*\w\d\b@)+&%" == "%"
  doAssert lit"((a|b)c*d?(ef)*\w\d\b)+@&%" == "%"
  doAssert lit"((a|b)c*d?(ef)*\w\d\bx)+" == ""
  doAssert lit"((a|b)c*d?(ef)*\w\d\bx)+yz" == ""
  doAssert lit"((a|b)c*d?(ef)*\w\d\b)+xyz" == ""
  doAssert lit"^a?" == ""
  doAssert lit"a?$" == ""
  doAssert lit"(?m)^" == ""  # XXX \n
  doAssert lit"(?m)$" == ""  # XXX \n
  doAssert lit"弢" == ""  # XXX support unicode

  doAssert r"abc".prefix.toString == r"ba".toNfa.toString
  doAssert r"abcab".prefix.toString == r"ba".toNfa.toString
  doAssert r"abc\wxyz".prefix.toString == r"ba".toNfa.toString
  doAssert r"abccc".prefix.toString == r"ba".toNfa.toString
  doAssert r"\w@".prefix.toString == r"\w".toNfa.toString
  doAssert r"\w@&%".prefix.toString == r"&@\w".toNfa.toString
  doAssert r"\w\d@&%".prefix.toString == r"&@\d\w".toNfa.toString

  doAssert r"(a|b)xyz".prefix.toString == r"yx(a|b)".toNfa.toString
  doAssert r"(a|ab)\w@&%".prefix.toString == r"&@\w(a|ba)".toNfa.toString
  doAssert r"(a|ab)\w@&%".prefix.toString ==
    "[#, [&, [@, [w, [a, eoe], [b, [a, eoe]]]]]]"
  doAssert r"a?b".prefix.toString == r"a?".toNfa.toString
  doAssert r"".prefix.s.len == 0
  doAssert r"a?".prefix.s.len == 0
  doAssert r"\w".prefix.s.len == 0
  doAssert r"(\w\d)*@".prefix.toString == r"(\d\w)*".toNfa.toString
  doAssert r"(\w\d)+@".prefix.toString == r"(\d\w)+".toNfa.toString
  # We search the start of the match, so greeddiness
  # does not matter
  doAssert r"(\w\d)*?@".prefix.toString == r"(\d\w)*".toNfa.toString
  doAssert r"(\w\d)+?@".prefix.toString == r"(\d\w)+".toNfa.toString
  doAssert r"\w+\d+?@".prefix.toString == r"\d+\w+".toNfa.toString
  doAssert r"\w+?\d+@".prefix.toString == r"\d+\w+".toNfa.toString
  doAssert r"\w+?\d+?@".prefix.toString == r"\d+\w+".toNfa.toString
  doAssert r"\w*\d*?@".prefix.toString == r"\d*\w*".toNfa.toString
  doAssert r"\w*?\d*@".prefix.toString == r"\d*\w*".toNfa.toString
  doAssert r"\w*?\d*?@".prefix.toString == r"\d*\w*".toNfa.toString
  doAssert r"\w+\d*?@".prefix.toString == r"\d*\w+".toNfa.toString
  doAssert r"\w+?\d*@".prefix.toString == r"\d*\w+".toNfa.toString
  doAssert r"\w+?\d*?@".prefix.toString == r"\d*\w+".toNfa.toString
  doAssert r"\w*\d+?@".prefix.toString == r"\d+\w*".toNfa.toString
  doAssert r"\w*?\d+@".prefix.toString == r"\d+\w*".toNfa.toString
  doAssert r"\w*?\d+?@".prefix.toString == r"\d+\w*".toNfa.toString

  echo "litopt ok"
