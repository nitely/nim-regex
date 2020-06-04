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
runtime is cuadratic.

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

]#

import nodetype
import nfa

type
  RpnExp = seq[Node]
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
# replace (...)*, (...)?, and
# (...|...) by skip nodes.
# The (...)+ remain, but
# break the loop.
# Based on Thompson's construction
func litNfa(exp: RPNExp): Nfa =
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
    of reZeroOrMore, reZeroOrOne:
      discard states.pop()
      ends.update(ni, [eoe])
      result.add initSkipNode([eoe])
      states.add ni
    of reOneOrMore:
      # states.add(states.pop())
      discard
    of reGroupStart:
      discard
    of reGroupEnd:
      discard
    else:
      doAssert false
  doAssert states.len == 1
  result.add initSkipNode(states)

# re"(abc)+" -> a
# re"(abc)*" -> -1
# re"(abc)*xyz" -> x
func lonelyLit(nfa: Nfa): int32 =
  result = -1
  var state = nfa[^1]
  while state.kind != reEoe:
    if state.kind == reChar:
      return state.cp.int32
    doAssert state.next.len == 1
    state = nfa[state.next[0]]

when isMainModule:
  import parser
  import exptransformation

  func rpn(s: string): RpnExp =
    var groups: GroupsCapture
    result = s
      .parse
      .transformExp(groups)

  func lit(s: string): int32 =
    s.rpn.litNfa.lonelyLit

  doAssert lit"(abc)+" == 'a'.int
  doAssert lit"(abc)*" == -1
  doAssert lit"(abc)*xyz" == 'x'.int
  doAssert lit"(a|b)" == -1
  doAssert lit"a?" == -1
  doAssert lit"a" == 'a'.int
  doAssert lit"(a|b)xyz" == 'x'.int
  doAssert lit"(\w\d)*abc" == 'a'.int
  doAssert lit"(\w\d)+abc" == 'a'.int
  doAssert lit"(\w\d)?abc" == 'a'.int
  doAssert lit"z(x|y)+abc" == 'z'.int
  doAssert lit"((a|b)c*d?(ef)*\w\d\bx)+" == 'x'.int
  doAssert lit"((a|b)c*d?(ef)*\w\d\bx)+yz" == 'x'.int
  doAssert lit"((a|b)c*d?(ef)*\w\d\b)+yz" == 'y'.int
  echo "ok"
