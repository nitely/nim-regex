import unicode
import sets
import tables
import algorithm

import nodetype
import common
import scanner

# todo: can not use unicodeplus due to
# https://github.com/nim-lang/Nim/issues/7059
func swapCase(r: Rune): Rune =
  # Note a character can be
  # non-lower and non-upper
  if r.isUpper():
    result = r.toLower()
  elif r.isLower():
    result = r.toUpper()
  else:
    result = r

func check(cond: bool, msg: string) =
  if not cond:
    raise newException(RegexError, msg)

func greediness(expression: seq[Node]): seq[Node] =
  ## apply greediness to an expression
  result = newSeqOfCap[Node](expression.len)
  var sc = expression.scan()
  for n in sc.mitems():
    if (n.kind in repetitionKind or
        n.kind == reZeroOrOne) and
        sc.peek.kind == reZeroOrOne:
      n.isGreedy = true
      discard sc.next
    result.add(n)

type
  GroupsCapture* = object
    count*: int16
    names*: OrderedTable[string, int16]

func fillGroups(
  exp: seq[Node],
  groups: var GroupsCapture
): seq[Node] =
  ## populate group indices, names and capturing mark
  result = exp
  groups.names = initOrderedTable[string, int16](2)
  groups.count = 0'i16
  var gs = newSeq[int]()
  for i, n in result.mpairs:
    case n.kind
    of reGroupStart:
      gs.add(i)
      if n.isCapturing:
        n.idx = groups.count
        inc groups.count
      if n.name.len > 0:
        assert n.isCapturing
        groups.names[n.name] = n.idx
    of reGroupEnd:
      check(
        gs.len > 0,
        "Invalid capturing group. " &
        "Found too many closing symbols")
      let start = gs.pop()
      n.isCapturing = result[start].isCapturing
      n.idx = result[start].idx
    else:
      discard
    check(
      groups.count < int16.high,
      ("Invalid number of capturing groups, " &
       "the limit is $#") %% $(int16.high - 1))
  check(
    gs.len == 0,
    "Invalid capturing group. " &
    "Found too many opening symbols")

func toAsciiKind(k: NodeKind): NodeKind =
  case k
  of reWordBoundary:
    reWordBoundaryAscii
  of reNotWordBoundary:
    reNotWordBoundaryAscii
  of reWord:
    reWordAscii
  of reDigit:
    reDigitAscii
  of reWhiteSpace:
    reWhiteSpaceAscii
  of reNotAlphaNum:
    reNotAlphaNumAscii
  of reNotDigit:
    reNotDigitAscii
  of reNotWhiteSpace:
    reNotWhiteSpaceAscii
  of reAny:
    reAnyAscii
  of reAnyNL:
    reAnyNLAscii
  else:
    k

func toggle(f: Flag): Flag =
  ## toggle regular flag to
  ## negated flag and the other way around
  case f
  of flagCaseInsensitive:
    flagNotCaseInsensitive
  of flagNotCaseInsensitive:
    flagCaseInsensitive
  of flagMultiLine:
    flagNotMultiLine
  of flagNotMultiLine:
    flagMultiLine
  of flagAnyMatchNewLine:
    flagNotAnyMatchNewLine
  of flagNotAnyMatchNewLine:
    flagAnyMatchNewLine
  of flagUnGreedy:
    flagNotUnGreedy
  of flagNotUnGreedy:
    flagUnGreedy
  of flagUnicode:
    flagNotUnicode
  of flagNotUnicode:
    flagUnicode
  of flagVerbose:
    flagNotVerbose
  of flagNotVerbose:
    flagVerbose

func squash(flags: seq[seq[Flag]]): array[Flag, bool] =
  ## Nested groups may contain flags,
  ## this will set/unset those flags
  ## in order. It should be done each time
  ## there is a group start/end
  for ff in flags:
    for f in ff:
      result[f.toggle()] = false
      result[f] = true

func applyFlag(n: var Node, f: Flag) =
  case f
  of flagAnyMatchNewLine:
    if n.kind == reAny:
      n.kind = reAnyNL
  of flagMultiLine:
    case n.kind
    of reStartSym:
      n.kind = reStartSymML
    of reEndSym:
      n.kind = reEndSymML
    else:
      discard
  of flagCaseInsensitive:
    if n.kind == reChar and n.cp != n.cp.swapCase():
      n.kind = reCharCI
    # todo: apply recursevely to
    #       shorthands of reInSet/reNotSet (i.e: [:ascii:])
    if n.kind in {reInSet, reNotSet}:
      var cps = initHashSet[Rune]()
      cps.incl(n.cps)
      for cp in cps:
        let cpsc = cp.swapCase()
        if cp != cpsc:
          n.cps.incl(cpsc)
      for sl in n.ranges[0 .. ^1]:
        let
          cpa = sl.a.swapCase()
          cpb = sl.b.swapCase()
        if sl.a != cpa and sl.b != cpb:
          n.ranges.add(cpa .. cpb)
  of flagUnGreedy:
    if n.kind in opKind:
      n.isGreedy = not n.isGreedy
  of flagNotUnicode:
    n.kind = n.kind.toAsciiKind()
    if n.kind in {reInSet, reNotSet}:
      for nn in n.shorthands.mitems:
        nn.kind = nn.kind.toAsciiKind()
  else:
    assert f in {
      flagNotAnyMatchNewLine,
      flagNotMultiLine,
      flagNotCaseInsensitive,
      flagNotUnGreedy,
      flagUnicode,
      flagVerbose,
      flagNotVerbose}

func applyFlags(expression: seq[Node]): seq[Node] =
  ## apply flags to each group
  result = newSeqOfCap[Node](expression.len)
  var flags = newSeq[seq[Flag]]()
  var sc = expression.scan()
  for n in sc.mitems():
    # (?flags)
    # Orphan flags are added to current group
    case n.kind
    of reGroupStart:
      if n.flags.len == 0:
        flags.add(@[])
        result.add(n)
        continue
      if sc.peek.kind == reGroupEnd:  # (?flags)
        discard sc.next()
        if flags.len > 0:
          flags[flags.len - 1].add(n.flags)
        else:
          flags.add(n.flags)
        continue  # skip (
      flags.add(n.flags)
    of reGroupEnd:
      discard flags.pop()
    else:
      let ff = flags.squash()
      for f in Flag.low .. Flag.high:
        if ff[f]:
          applyFlag(n, f)
    result.add(n)

func expandOneRepRange(subExpr: seq[Node], n: Node): seq[Node] =
  ## expand a repetition-range expression
  ## into the equivalent repeated expression
  assert n.kind == reRepRange
  if n.max == -1:  # a{n,} -> aaa*
    result = newSeqOfCap[Node](subExpr.len * (n.min + 1) + 1)
    for _ in 0 ..< n.min:
      result.add(subExpr)
    result.add(Node(
      kind: reZeroOrMore,
      cp: "*".toRune,
      isGreedy: n.isGreedy))
  elif n.min == n.max:  # a{n} -> aaa
    result = newSeqOfCap[Node](subExpr.len * n.max)
    for _ in 0 ..< n.max - 1:
      result.add(subExpr)
  else:  # a{n,m} -> aaa?a?
    assert n.min < n.max
    result = newSeqOfCap[Node](subExpr.len * n.max + n.max - n.min)
    for _ in 0 ..< n.min:
      result.add(subExpr)
    for _ in n.min ..< n.max - 1:
      result.add(Node(
        kind: reZeroOrOne,
        cp: "?".toRune,
        isGreedy: n.isGreedy))
      result.add(subExpr)
    result.add(Node(
      kind: reZeroOrOne,
      cp: "?".toRune,
      isGreedy: n.isGreedy))

func expandRepRange(expression: seq[Node]): seq[Node] =
  ## expand every repetition range
  result = newSeqOfCap[Node](expression.len)
  var i: int
  var gi: int
  for n in expression:
    if n.kind != reRepRange:
      result.add(n)
      continue
    check(
      result.len > 0,
      "Invalid repeition range, " &
      "nothing to repeat")
    case result[^1].kind
    of reGroupEnd:
      i = 0
      gi = 0
      for ne in result.reversed:
        inc i
        if ne.kind == reGroupEnd:
          inc gi
        if ne.kind == reGroupStart:
          dec gi
        if gi == 0:
          break
        doAssert gi >= 0
      doAssert gi == 0
      assert result[result.len-i].kind == reGroupStart
      result.add(result[result.len-i .. result.len-1].expandOneRepRange(n))
    of matchableKind:
      result.add(result[result.len-1 .. result.len-1].expandOneRepRange(n))
    else:
      raise newException(RegexError, (
        "Invalid repetition range, either " &
        "char, shorthand (i.e: \\w), group, or set " &
        "expected before repetition range"))

func joinAtoms(expression: seq[Node]): seq[Node] =
  ## Put a ``~`` joiner between atoms. An atom is
  ## a piece of expression that would loose
  ## meaning when breaking it up (i.e.: ``a~(b|c)*~d``)
  result = newSeqOfCap[Node](expression.len * 2)
  var atomsCount = 0
  for n in expression:
    case n.kind
    of matchableKind, assertionKind:
      inc atomsCount
      if atomsCount > 1:
        atomsCount = 1
        result.add(initJoinerNode())
    of reGroupStart:
      if atomsCount > 0:
        result.add(initJoinerNode())
      atomsCount = 0
    of reOr:
      atomsCount = 0
    of reGroupEnd,
        reZeroOrMore,
        reOneOrMore,
        reZeroOrOne,
        reRepRange:
      inc atomsCount
    else:
      assert false
    result.add(n)

type
  Associativity = enum
    ## Operator associativity. Unary ops are
    ## right[-to-left] and binary ops are
    ## left[-to-right]
    asyRight
    asyLeft
  OpsPA = tuple
    precedence: int
    associativity: Associativity

func opsPA(nk: NodeKind): OpsPA =
  ## return the precedence and
  ## associativity of a given node kind
  assert nk in opKind
  case nk
  of reRepRange,
      reZeroOrMore,
      reOneOrMore,
      reZeroOrOne:
    result = (5, asyRight)
  of reJoiner:
    result = (4, asyLeft)
  of reOr:
    result = (3, asyLeft)
  else:
    assert false

func hasPrecedence(a: NodeKind, b: NodeKind): bool =
  ## Check ``b`` has precedence over ``a``.
  ## Both ``a`` and ``b`` are expected to
  ## be valid operators. Unary operators such
  ## as: ``*``, ``?`` and ``+`` have right-to-left
  ## associativity. Binary operators
  ## such as: ``|`` (or) and ``~`` (joiner) have
  ## left-to-right associativity
  result =
    (opsPA(b).associativity == asyRight and
      opsPA(b).precedence <= opsPA(a).precedence) or
    (opsPA(b).associativity == asyLeft and
      opsPA(b).precedence < opsPA(a).precedence)

func popGreaterThan(ops: var seq[Node], op: Node): seq[Node] =
  assert op.kind in opKind
  result = newSeqOfCap[Node](ops.len)
  while (ops.len > 0 and
      ops[ops.len - 1].kind in opKind and
      ops[ops.len - 1].kind.hasPrecedence(op.kind)):
    result.add(ops.pop())

func popUntilGroupStart(ops: var seq[Node]): seq[Node] =
  result = newSeqOfCap[Node](ops.len)
  while true:
    let op = ops.pop()
    result.add(op)
    if op.kind == reGroupStart:
      break

func rpn(expression: seq[Node]): seq[Node] =
  ## An adaptation of the Shunting-yard algorithm
  ## for producing `Reverse Polish Notation` out of
  ## an expression specified in infix notation.
  ## It supports regex primitives including groups.
  ## The point of doing this is greatly simplifying
  ## the parsing of the regular expression into an NFA.
  ## Suffix notation removes nesting and so it can
  ## be parsed in a linear way instead of recursively
  result = newSeqOfCap[Node](expression.len)
  var ops = newSeq[Node]()
  for n in expression:
    case n.kind
    of matchableKind, assertionKind:
      result.add(n)
    of reGroupStart:
      ops.add(n)
    of reGroupEnd:
      result.add(ops.popUntilGroupStart())
      result.add(n)
    of opKind:
      result.add(ops.popGreaterThan(n))
      ops.add(n)
    else:
      assert false
  # reverse ops
  for i in 1 .. ops.len:
    result.add(ops[ops.len - i])

func transformExp*(
  exp: seq[Node],
  groups: var GroupsCapture
): seq[Node] {.inline.} =
  result = exp
    .fillGroups(groups)
    .greediness
    .applyFlags
    .expandRepRange
    .joinAtoms
    .rpn
