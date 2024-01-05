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
    of matchableKind, assertionKind, reSkip:
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

func delimiterLit(exp: RpnExp): NodeIdx =
  template state: untyped = litNfa.s[stateIdx]
  result = -1
  let litNfa = exp.toLitNfa()
  var cpSeen = initHashSet[Rune](16)
  var lits = newSeq[int16]()
  var stateIdx = litNfa.s.len.int16-1
  while state.kind != reEoe:
    if state.kind == reChar:
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

type
  Lits = object
    idx: NodeIdx
    s: string

func find(nodes: seq[Node], uid: int): NodeIdx =
  for idx in 0 .. nodes.len-1:
    if nodes[idx].uid == uid:
      return idx.NodeIdx
  doAssert false

func lits(exp: RpnExp, flags: RegexFlags): Lits =
  template state: untyped = litNfa.s[stateIdx]
  result.idx = exp.delimiterLit()
  if result.idx == -1:
    return
  let litUid = exp.s[result.idx].uid
  let litNfa = exp.toLitNfa()
  var lits = newSeq[int16]()
  var stateIdx = litNfa.s.len.int16-1
  while state.kind != reEoe:
    if state.kind == reChar:
      lits.add stateIdx
    doAssert state.next.len == 1
    stateIdx = state.next[0]
  assert lits == sorted(lits, system.cmp)
  var litIdxStart = -1
  for i, stateIdx in pairs lits:
    if state.uid == litUid:
      litIdxStart = i
      break
  doAssert litIdxStart != -1
  for i in countdown(litIdxStart-1, 0):
    if litNfa.s[lits[litIdxStart]].uid-1 != litNfa.s[lits[i]].uid:
      break
    litIdxStart = i
  var litIdxEnd = litIdxStart
  for i in litIdxStart+1 .. lits.len-1:
    if litNfa.s[lits[litIdxEnd]].uid+1 != litNfa.s[lits[i]].uid:
      break
    litIdxEnd = i
  doAssert litIdxEnd >= litIdxStart
  var ss = ""
  if regexArbitraryBytes in flags:
    for i in litIdxStart .. litIdxEnd:
      ss.add litNfa.s[lits[i]].cp.char
  else:
    for i in litIdxStart .. litIdxEnd:
      ss.add litNfa.s[lits[i]].cp
  # true for non ascii chars (>127) and lit sequences
  if ss.len > 1:
    result.idx = find(exp.s, litNfa.s[lits[litIdxStart]].uid)
    result.s.add ss

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
    lits*: string
    bytelits*: string
    nfa*: Nfa

func canOpt*(litOpt: LitOpt): bool =
  return litOpt.nfa.s.len > 0

func litopt3*(exp: RpnExp, flags: RegexFlags = {}): LitOpt =
  template litNode: untyped = exp.s[lits2.idx]
  let lits2 = exp.lits(flags)
  if lits2.idx == -1:
    return
  result.lit = litNode.cp
  result.lits = lits2.s
  result.nfa = exp
    .subExps
    .eNfa
    .prefix(litNode.uid)
    .eRemoval

when isMainModule:
  import parser
  import exptransformation

  func rpn(
    s: string,
    groups: var GroupsCapture,
    flags: RegexFlags = {}
  ): RpnExp =
    result = s
      .parse(flags)
      .transformExp(groups, flags)

  func rpn(s: string, flags: RegexFlags = {}): RpnExp =
    var groups: GroupsCapture
    rpn(s, groups, flags)

  func delim(s: string): string =
    ## Delimiter lit
    let idx = s.rpn.delimiterLit
    if idx == -1: return ""
    return s.rpn.s[idx].cp.toUtf8

  func lits(s: string): string =
    let opt = s.rpn.litopt3
    if not opt.canOpt: return
    return opt.lits

  func bytelits(s: string): string =
    let flags = {regexArbitraryBytes}
    let opt = s
      .rpn(flags)
      .litopt3(flags)
    if not opt.canOpt: return
    return opt.lits

  func lit(s: string): Rune =
    let opt = s.rpn.litopt3
    if not opt.canOpt: return  # beware Rune(0) is valid
    return opt.lit

  func prefix(s: string): Nfa =
    let opt = s.rpn.litopt3
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
      if isEpsilonTransition(nfa.s[nn]):
        continue
      result.add ", "
      result.add toString(nfa, nn, visited)
    result.add "]"

  proc toString(nfa: Nfa): string {.used.} =
    var visited: set[int16]
    result = toString(nfa, 0, visited)

  doAssert delim"abc" == "c"
  doAssert delim"abcab" == "c"
  doAssert delim"abc\wxyz" == "c"
  doAssert delim"(abc)+" == ""
  doAssert delim"(abc)+xyz" == "z"
  doAssert delim"(abc)*" == ""
  doAssert delim"(abc)*xyz" == "z"
  doAssert delim"(a|b)" == ""
  doAssert delim"(a+|b)" == ""
  doAssert delim"(a+|b+)" == ""
  doAssert delim"((abc)|(xyz))" == ""
  doAssert delim"((abc)+|(xyz)+)" == ""
  doAssert delim"((abc)*|(xyz)*)" == ""
  doAssert delim"a?" == ""
  doAssert delim"a?b" == "b"
  doAssert delim"a" == "a"
  doAssert delim"(a|b)xyz" == "z"
  doAssert delim"(a|bc)xyz" == "z"
  doAssert delim"(ab|c)xyz" == "z"
  doAssert delim"a+b" == "b"
  doAssert delim"a*b" == "b"
  doAssert delim"b+b" == ""
  doAssert delim"b*b" == ""
  doAssert delim"\d+x" == "x"
  doAssert delim"\d*x" == "x"
  doAssert delim"\d?x" == "x"
  doAssert delim"\w+x" == ""
  doAssert delim"\w*x" == ""
  doAssert delim"\w?x" == ""
  doAssert delim"(\w\d)*abc" == ""
  doAssert delim"(\w\d)+abc" == ""
  doAssert delim"(\w\d)?abc" == ""
  doAssert delim"z(x|y)+abc" == "z"
  doAssert delim"((a|b)c*d?(ef)*\w\d\b@)+" == ""
  doAssert delim"((a|b)c*d?(ef)*\w\d\b@)+&%" == "%"
  doAssert delim"((a|b)c*d?(ef)*\w\d\b)+@&%" == "%"
  doAssert delim"((a|b)c*d?(ef)*\w\d\bx)+" == ""
  doAssert delim"((a|b)c*d?(ef)*\w\d\bx)+yz" == ""
  doAssert delim"((a|b)c*d?(ef)*\w\d\b)+xyz" == ""
  doAssert delim"^a?" == ""
  doAssert delim"a?$" == ""
  doAssert delim"(?m)^" == ""  # XXX \n
  doAssert delim"(?m)$" == ""  # XXX \n
  doAssert delim"弢" == "弢"
  doAssert delim"(?<=\d)ab" == "b"
  doAssert delim"(?<=\w)ab" == "b"
  doAssert delim"(?<=\w+)ab" == "b"
  doAssert delim"\dab2" == "b"
  doAssert delim"\d+ab2" == "b"
  doAssert delim"\wab2" == ""
  doAssert delim"abcd?" == "c"
  doAssert delim"abcd+" == "c"
  doAssert delim"abcd*" == "c"
  doAssert delim"a?bc" == "c"
  doAssert delim"a*bc" == "c"
  doAssert delim"a+bc" == "c"
  doAssert delim"ab(c)" == "b"  # XXX c
  doAssert delim"(ab)c" == "b"  # XXX c
  doAssert delim"abc?d" == "b"
  doAssert delim"abc*d" == "b"
  doAssert delim"abc+d" == "b"

  doAssert lits"a" == ""
  doAssert lits"ab" == "ab"
  doAssert lits"abc" == "abc"
  doAssert lits"abcab" == "abcab"
  doAssert lits"abc\wxyz" == "abc"
  doAssert lits"(abc)+" == ""
  doAssert lits"(abc)+xyz" == "xyz"
  doAssert lits"(abc)*" == ""
  doAssert lits"(abc)*xyz" == "xyz"
  doAssert lits"(a|b)" == ""
  doAssert lits"(a+|b)" == ""
  doAssert lits"(a+|b+)" == ""
  doAssert lits"((abc)|(xyz))" == ""
  doAssert lits"((abc)+|(xyz)+)" == ""
  doAssert lits"((abc)*|(xyz)*)" == ""
  doAssert lits"a?" == ""
  doAssert lits"a?b" == ""
  doAssert lits"a?bc" == "bc"
  doAssert lits"(a|b)xyz" == "xyz"
  doAssert lits"(a|bc)xyz" == "xyz"
  doAssert lits"(ab|c)xyz" == "xyz"
  doAssert lits"a+b" == ""
  doAssert lits"a+bc" == "bc"
  doAssert lits"a*b" == ""
  doAssert lits"a*bc" == "bc"
  doAssert lits"b+b" == ""
  doAssert lits"b*b" == ""
  doAssert lits"\d+x" == ""
  doAssert lits"\d+xy" == "xy"
  doAssert lits"\d*x" == ""
  doAssert lits"\d*xy" == "xy"
  doAssert lits"\d?x" == ""
  doAssert lits"\d?xy" == "xy"
  doAssert lits"\w+x" == ""
  doAssert lits"\w*x" == ""
  doAssert lits"\w?x" == ""
  doAssert lits"(\w\d)*abc" == ""
  doAssert lits"(\w\d)+abc" == ""
  doAssert lits"(\w\d)?abc" == ""
  doAssert lits"z(x|y)+abc" == ""
  doAssert lits"xyz(x|y)+abc" == "xyz"
  doAssert lits"((a|b)c*d?(ef)*\w\d\b@)+" == ""
  doAssert lits"((a|b)c*d?(ef)*\w\d\b@)+&%" == "&%"
  doAssert lits"((a|b)c*d?(ef)*\w\d\b)+@&%" == "@&%"
  doAssert lits"((a|b)c*d?(ef)*\w\d\bx)+" == ""
  doAssert lits"((a|b)c*d?(ef)*\w\d\bx)+yz" == ""
  doAssert lits"((a|b)c*d?(ef)*\w\d\b)+xyz" == ""
  doAssert lits"^a?" == ""
  doAssert lits"a?$" == ""
  doAssert lits"(?m)^" == ""  # XXX \n
  doAssert lits"(?m)$" == ""  # XXX \n
  doAssert lits"弢" == "弢"
  doAssert lits"(?<=\d)ab" == "ab"
  doAssert lits"(?<=\w)ab" == "ab"
  doAssert lits"(?<=\w+)ab" == "ab"
  doAssert lits"\dab2" == "ab2"
  doAssert lits"\d+ab2" == "ab2"
  doAssert lits"\wab2" == ""
  doAssert lits"abcd?" == "abc"
  doAssert lits"abcd*" == "abc"
  doAssert lits"abcd+" == "abc"
  doAssert lits"a?bc" == "bc"
  doAssert lits"a*bc" == "bc"
  doAssert lits"a+bc" == "bc"
  doAssert lits"ab(c)" == "ab"  # XXX abc
  doAssert lits"(ab)c" == "ab"  # XXX abc
  doAssert lits"abc?d" == "ab"
  doAssert lits"abc*d" == "ab"
  doAssert lits"abc+d" == "ab"

  doAssert bytelits"\xff\xff" == "\xff\xff"
  doAssert bytelits"\x0f\xff" == "\x0f\xff"
  doAssert bytelits"\xff\x0f" == "\xff\x0f"
  doAssert bytelits"\x80\x80" == "\x80\x80"
  doAssert bytelits"\x00\x00" == "\x00\x00"
  doAssert lit"\xff" == '\xff'.Rune
  doAssert lit"\x80" == '\x80'.Rune  # 128
  doAssert lit"\x7F" == '\x7F'.Rune  # 127
  doAssert bytelits"\xff" == ""
  doAssert bytelits"\x00" == ""
  doAssert bytelits"弢" == "弢"
  doAssert bytelits"弢" == "\xF0\xAF\xA2\x94"
  # this is not a bug, regex is interpreted as bytes not utf8
  doAssert bytelits"弢?" == "\xF0\xAF\xA2"
  doAssert bytelits"弢*" == "\xF0\xAF\xA2"
  doAssert bytelits"弢+" == "\xF0\xAF\xA2"
  # but /x{ffffff} works as expected
  doAssert bytelits"\x{2F895}" == "\x02\xF8\x95"
  doAssert bytelits"\x{2F895}?" == ""
  doAssert bytelits"\x{2F895}*" == ""
  doAssert bytelits"\x{2F895}+" == ""
  # XXX need to skip group nodes between lits
  doAssert bytelits"\x{2F895}\x{2F895}" == "\x02\xF8\x95"  # XXX \x02\xF8\x95\x02\xF8\x95
  doAssert bytelits"\x{2F895}{2}" == "\x02\xF8\x95"  # XXX \x02\xF8\x95\x02\xF8\x95
  doAssert bytelits"\xff{2}" == "\xff\xff"
  doAssert bytelits"\x{2F895}?\x02\xF8\x95" == ""
  doAssert bytelits"\x{2F895}*\x02\xF8\x95" == ""
  doAssert bytelits"\x{2F895}+\x02\xF8\x95" == ""

  doAssert r"abc".prefix.toString == r"".toNfa.toString
  doAssert r"\dabc".prefix.toString == r"\d".toNfa.toString
  doAssert r"abcab".prefix.toString == r"".toNfa.toString
  doAssert r"\dabcab".prefix.toString == r"\d".toNfa.toString
  doAssert r"abc\wxyz".prefix.toString == r"".toNfa.toString
  doAssert r"\dabc\wxyz".prefix.toString == r"\d".toNfa.toString
  doAssert r"abccc".prefix.toString == r"".toNfa.toString
  doAssert r"\dabccc".prefix.toString == r"\d".toNfa.toString
  doAssert r"\w@".prefix.toString == r"\w".toNfa.toString
  doAssert r"\w@&%".prefix.toString == r"\w".toNfa.toString
  doAssert r"\w\d@&%".prefix.toString == r"\d\w".toNfa.toString
  doAssert r"\w\d@&%".prefix.toString == "[#, [d, [w, eoe]]]"

  doAssert r"(a|b)xyz".prefix.toString == r"(a|b)".toNfa.toString
  doAssert r"(a|ab)\w@&%".prefix.toString == r"\w(a|ba)".toNfa.toString
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

  # sanity check
  block:
    let skipChars = {'(', ')', '+', '?', '*', '[', ']', '\\'}
    var i = 0
    for cp in 0 .. 127:
      if cp.char in skipChars:
        continue
      doAssert lits(r"" & cp.char).len == 0
      inc i
    doAssert i == 128-skipChars.len
    doAssert lits(128.Rune.toUtf8).len == 2

  echo "litopt ok"
