## Literals optimization.
## This is about optimizing the find operation
## by quickly searching Regular Expression literals
## within the input string. See issue #59.

#[
Only optimizations that are
guaranteed to run in linear
time are implemented.

re"(?ms)^.+abc"
can be optimized by running the match
at every \n. We can do a quick memchr to find
every \n.

There is a gotcha, though. If we do this on
an input such as `\n\n\n\n\n` the matcher will
consume the whole string for every `\n`, hence
runtime is quadratic.

The solution is to run a special kind of find,
instead of a regular match. We need a special find,
because the regular find will always consume the
whole string, so we'd only be able to skip the first
part of the input until the first `\n`, instead
of skipping multiple parts of the input.

The special find works just like the regular one,
except it stops when the current character of the
input cannot be matched by the Regular Expression (RE).
Once the special find stops, we can run a memchr from
the index where the find stopped.

We never process a character more than once, hence
the algorithm runs in linear time.

Notice find returns non-overlapping matches,
hence the next search must not process
characters that are part of a previous matched string.
The start of text cannot be within the boundaries
of the previous match.

^ There is a bunch of possible literal optimizations,
  but this is the meat of it.

re"\wabc"
This can be optimized the same way as
the first example, except going back
one character after running memchr.

The runtime is again linear, every
character can be consumed at most 2 times:
one fordward (memchr), and one fordward
(special find). The "go back" operation does
not consumes characters, it just does
index - len(prefix).

The prefix may be longer than just one
character, as long as the lenght of it
is fixed we can just go back the lenght of
the prefix and run a find. We can do
this even when the prefix contain alternations
as long as they have the same lenght, for
example re"(x|y)abc".

The alternative to going back X chars
is to run the regex prefix in reverse, and
then run the special find for the full regex.
This would support prefixes with mixed
alternation lenghts (i.e: re"(\w\w|\d)foo").
Since this is needed, the going back X
chars may just be an optimal optimization
(meaning I may not implement it).

re"\w+abc"
This can be optimized by doing a memchr
for "a", running a special match for the
regex prefix ("\w+") in reverse, and running
the special find for the full regex.

The special match is a regular match that returns
the longest match.

We cannot divide the regex in a prefix and suffix
of "a", and just run that because that would
run in cuadratic time (see the first "\n\n\n" input example).
Also, we need to support captures.

re"\w(a|b)"
This can be optimized by running two scans,
one for "a" and one for "b" at the start and
after each match, and then trying to match the one
with smaller char index first (i.e: the one
that we found first).

My experiments show PCRE do this with up to
two literal alternations. When there are more
than two alternations, it's likely better to
generate a DFA. For example:
re"foo|bar|baz|quz" would generate a DFA
that matches re"f|b|q". We'd use the DFA
instead of memchr.

Single literals should be preferred over
alternations. For example: re"\w(a|b)c" would
memchr the "c" character. Meaning other listed
optimizations are preferred.

---------------------------

We should try to match the larger
amount of literals and try to fail
early. Let's say we have re"\w+abc",
trying to match "a" and then an unbounded
amount of chars at the left will likely take
more time than trying to match "c", then
"ab" at the left, since "a" will appear
at least the same amount of times as "abc",
but possibly more times.

This translates to finding the largest amount
of contiguous lits and picking the right most
lit to memchr.

However, this has the cost of potentially
requiring a larger left part of the regex (to match the left side),
so it may be better to find the first lit in the regex and then
pick the last lit of its contiguous lits (if any).


----------------------------------------

XXX edge case

re"\d\w+x"
text: "xxxxxxxxxxx"
does it take quadratic time?
the prefix find goes to the start
of the string every time?

re"\d\w+@"
text: "@@@@@@@@"
text2: "@@@@xxxx@@@@"
does not has this problem,
since the literal is not
matched by the prefix,
it works as a delimiter

]#

import algorithm
import sets
import unicode

import nodetype
import nodematch
import nfa

type
  RpnExp = seq[Node]
  LitNfa = Nfa
  End = seq[int16]

func combine(
  nfa: var seq[Node],
  ends: var seq[End],
  org, target: int16
) =
  for e in ends[org]:
    for i, ni in nfa[e].next.mpairs:
      if nfa[ni].kind == reEoe:
        ni = target
  ends[org] = ends[target]

const eoe = 0'i16

func update(
  ends: var seq[End],
  ni: int16,
  next: openArray[int16]
) =
  ends[ni].setLen(0)
  for n in next:
    if n == eoe:
        ends[ni].add ni
    else:
        ends[ni].add ends[n]

# Keep lits,
# replace (...)+, (...)*, (...)?,
# and (...|...) by skip nodes.
# Based on Thompson's construction
func toLitNfa(exp: RPNExp): LitNfa =
  result = newSeqOfCap[Node](exp.len + 2)
  result.add initEoeNode()
  var
    ends = newSeq[End](exp.len + 1)
    states = newSeq[int16]()
  if exp.len == 0:
    states.add eoe
  for n in exp:
    var n = n
    doAssert n.next.len == 0
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
      discard states.pop()
      discard states.pop()
      ends.update(ni, [eoe])
      result.add initSkipNode([eoe])
      states.add ni
    of reZeroOrMore, reZeroOrOne, reOneOrMore:
      discard states.pop()
      ends.update(ni, [eoe])
      result.add initSkipNode([eoe])
      states.add ni
    of reGroupStart:
      discard
    of reGroupEnd:
      discard
    else:
      doAssert false
  doAssert states.len == 1
  result.add initSkipNode(states)

type
  NodeIdx = int16

func lonelyLit(exp: RpnExp): NodeIdx =
  template state: untyped = litNfa[stateIdx]
  result = -1
  let litNfa = exp.toLitNfa()
  var cpSeen = initHashSet[Rune](16)
  var lits = newSeq[int16]()
  var stateIdx = litNfa.len.int16-1
  while state.kind != reEoe:
    if state.kind == reChar and
        state.cp.int <= 127:  # XXX support unicode
      if state.cp notin cpSeen:
        cpSeen.incl state.cp
        lits.add stateIdx
    doAssert state.next.len == 1
    stateIdx = state.next[0]
  assert lits == sorted(lits)
  # The algo takes O(litsLen * NfaSize)
  # time, it seems sensible to limit the lits
  lits.setLen min(lits.len, 128)
  var litsTmp = newSeq[int16]()
  for ni, n in exp.pairs:
    if n.kind notin matchableKind:
      continue
    for nlit in lits:
      doAssert n.uid <= litNfa[nlit].uid
      if n.uid == litNfa[nlit].uid:
        return ni.NodeIdx
      if not match(n, litNfa[nlit].cp):
        litsTmp.add nlit
    swap lits, litsTmp
    litsTmp.setLen 0

func prefix(eNfa: Enfa, uid: NodeUid): Enfa =
  template state0: untyped = eNfa.len.int16-1
  result = eNfa
  for n in result.mitems:
    n.next.setLen 0
  # reverse transitions; DFS
  var stack = @[(state0, -1'i16)]
  var visited: set[int16]
  template state: untyped = eNfa[ni]
  while stack.len > 0:
    let (ni, pi) = stack.pop()
    if pi > -1:
      result[ni].next.add pi
    if ni in visited:
      continue
    visited.incl ni
    # we only care about the prefix
    if state.uid == uid:
      continue
    for mi in state.next:
      stack.add (mi, ni)
  for n in result.mitems:
    n.next.reverse
  # Swap initial state by eoe
  var eoeIdx = -1'i16
  for ni, n in result.pairs:
    if n.kind == reEoe:
      doAssert eoeIdx == -1
      eoeIdx = ni.int16
  doAssert eoeIdx != -1
  for ni in eNfa[state0].next:
    for i in 0 .. result[ni].next.len-1:
      if result[ni].next[i] == state0:
        result[ni].next[i] = eoeIdx
  doAssert result[eoeIdx].kind == reEoe
  doAssert result[state0].kind == reSkip
  swap result[state0].kind, result[eoeIdx].kind
  swap result[state0], result[eoeIdx]
  # cut
  var nIdx = -1
  for ni, n in eNfa.pairs:
    if n.uid == uid:
      doAssert nIdx == -1
      nIdx = ni
  doAssert nIdx != -1
  result[state0].next = result[nIdx].next

type
  LitOpt* = object
    lit*: Rune
    nfa*: Nfa
    tns*: Transitions

func canOpt*(litOpt: LitOpt): bool =
  return litOpt.nfa.len > 0

func litopt2*(exp: RpnExp): LitOpt =
  template litNode: untyped = exp[litIdx]
  let litIdx = exp.lonelyLit()
  if litIdx == -1:
    return
  result.lit = litNode.cp
  result.nfa = exp
    .eNfa
    .prefix(litNode.uid)
    .eRemoval(result.tns)

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
    var transitions: Transitions
    result = s
      .rpn(groups)
      .nfa2(transitions)

  proc toString(
    nfa: Nfa,
    nIdx: int16,
    visited: var set[int16]
  ): string =
    # XXX zero-match transitions are missing
    if nfa[nIdx].kind == reEoe:
      result = "eoe"
      return
    if nIdx in visited:
      result = "[...]"
      return
    visited.incl nIdx
    let n = nfa[nIdx]
    result = "["
    result.add $n.cp
    for nn in n.next:
      result.add ", "
      result.add toString(nfa, nn, visited)
    result.add "]"

  proc toString(nfa: Nfa): string {.used.} =
    var visited: set[int16]
    result = toString(nfa, 0, visited)

  doAssert lit"(abc)+" == ""
  doAssert lit"(abc)+xyz" == "x"
  doAssert lit"(abc)*" == ""
  doAssert lit"(abc)*xyz" == "x"
  doAssert lit"(a|b)" == ""
  doAssert lit"(a+|b)" == ""
  doAssert lit"(a+|b+)" == ""
  doAssert lit"((abc)|(xyz))" == ""
  doAssert lit"((abc)+|(xyz)+)" == ""
  doAssert lit"((abc)*|(xyz)*)" == ""
  doAssert lit"a?" == ""
  doAssert lit"a?b" == "b"
  doAssert lit"a" == "a"
  doAssert lit"(a|b)xyz" == "x"
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
  doAssert lit"((a|b)c*d?(ef)*\w\d\b@)+&%" == "&"
  doAssert lit"((a|b)c*d?(ef)*\w\d\b)+@&%" == "@"
  doAssert lit"((a|b)c*d?(ef)*\w\d\bx)+" == ""
  doAssert lit"((a|b)c*d?(ef)*\w\d\bx)+yz" == ""
  doAssert lit"((a|b)c*d?(ef)*\w\d\b)+xyz" == ""
  doAssert lit"^a?" == ""
  doAssert lit"a?$" == ""
  doAssert lit"(?m)^" == ""  # XXX \n
  doAssert lit"(?m)$" == ""  # XXX \n
  doAssert lit"弢" == ""  # XXX support unicode

  doAssert r"abc".prefix.toString == r"".toNfa.toString
  doAssert r"abc".prefix.toString == "[#, eoe]"
  doAssert r"\w@".prefix.toString == r"\w".toNfa.toString
  doAssert r"\w@&%".prefix.toString == r"\w".toNfa.toString
  doAssert r"\w\d@&%".prefix.toString == r"\d\w".toNfa.toString

  doAssert r"(a|b)xyz".prefix.toString == r"(a|b)".toNfa.toString
  doAssert r"(a|ab)\w@&%".prefix.toString == r"\w(a|ba)".toNfa.toString
  doAssert r"(a|ab)\w@&%".prefix.toString == "[#, [w, [a, eoe], [b, [a, eoe]]]]"
  doAssert r"a?b".prefix.toString == r"a?".toNfa.toString
  doAssert r"".prefix.len == 0
  doAssert r"a?".prefix.len == 0
  doAssert r"\w".prefix.len == 0
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
