import std/unicode
import std/sets
import std/tables
import std/algorithm

import ./exptype
import ./types
import ./common
import ./scanner

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

func fixEmptyOps(exp: Exp): Exp =
  ## Handle "|", "(|)", "a|", "|b", "||", "a||b", ...
  ## Handle "()"
  result.s = newSeq[Node](exp.s.len)
  result.s.setLen 0
  for i in 0 .. exp.s.len-1:
    if exp.s[i].kind == reOr:
      if i-1 < 0 or exp.s[i-1].kind == reGroupStart:
        result.s.add initSkipNode()
      result.s.add exp.s[i]
      if i+1 > exp.s.len-1 or exp.s[i+1].kind in {reGroupEnd, reOr}:
        result.s.add initSkipNode()
    elif exp.s[i].kind == reGroupStart:
      result.s.add exp.s[i]
      if i+1 > exp.s.len-1 or exp.s[i+1].kind == reGroupEnd:
        result.s.add initSkipNode()
    else:
      result.s.add exp.s[i]

func greediness(exp: Exp): Exp =
  ## apply greediness to an expression
  result.s = newSeq[Node](exp.s.len)
  result.s.setLen 0
  var sc = exp.s.scan()
  for n in sc.mitems():
    if n.kind in repetitionKind or
        n.kind == reZeroOrOne:
      n.isGreedy = true
      if sc.peek.kind == reZeroOrOne:
        n.isGreedy = false
        discard sc.next
    result.s.add n

type
  GroupsCapture* = object
    count*: int16
    names*: OrderedTable[string, int16]

func fillGroups(
  exp: Exp,
  groups: var GroupsCapture
): Exp =
  ## populate group indices, names and capturing mark
  result = exp
  groups.names = initOrderedTable[string, int16](2)
  groups.count = 0'i16
  var gs = newSeq[int]()
  for i, n in mpairs result.s:
    case n.kind
    of groupStartKind:
      gs.add i
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
      n.isCapturing = result.s[start].isCapturing
      n.idx = result.s[start].idx
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
      var cps = initHashSet[Rune](2)
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
    doAssert f in {
      flagNotAnyMatchNewLine,
      flagNotMultiLine,
      flagNotCaseInsensitive,
      flagNotUnGreedy,
      flagUnicode,
      flagVerbose,
      flagNotVerbose}

func applyFlags(exp: Exp): Exp =
  ## apply flags to each group
  result.s = newSeq[Node](exp.s.len)
  result.s.setLen 0
  var flags = newSeq[seq[Flag]]()
  var sc = exp.s.scan()
  for n in sc.mitems():
    # (?flags)
    # Orphan flags are added to current group
    case n.kind
    of groupStartKind:
      flags.add n.flags
    of reGroupEnd:
      discard flags.pop()
    of reFlags:
      if flags.len > 0:
        flags[flags.len-1].add n.flags
      else:
        flags.add n.flags
      # XXX define skipKind = {reSkip, reFlags} in types,
      #     instead of this hack
      result.s.add Node(kind: reSkip)
      continue  # skip node
    else:
      let ff = flags.squash()
      for f in Flag.low .. Flag.high:
        if ff[f]:
          applyFlag(n, f)
    result.s.add n

func expandOneRepRange(subExpr: seq[Node], n: Node): seq[Node] =
  ## expand a repetition-range expression
  ## into the equivalent repeated expression
  doAssert n.kind == reRepRange
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
    doAssert n.min < n.max
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

func expandRepRange(exp: Exp): Exp =
  ## expand every repetition range
  result.s = newSeq[Node](exp.s.len)
  result.s.setLen 0
  var i: int
  var gi: int
  for n in exp.s:
    if n.kind != reRepRange:
      result.s.add n
      continue
    check(
      result.s.len > 0,
      "Invalid repeition range, " &
      "nothing to repeat")
    case result.s[^1].kind
    of reGroupEnd:
      i = 0
      gi = 0
      for ne in result.s.reversed:
        inc i
        if ne.kind == reGroupEnd:
          inc gi
        if ne.kind == reGroupStart:
          dec gi
        if gi == 0:
          break
        doAssert gi >= 0
      doAssert gi == 0
      assert result.s[result.s.len-i].kind == reGroupStart
      result.s.add result.s[result.s.len-i .. result.s.len-1].expandOneRepRange(n)
    of matchableKind:
      result.s.add result.s[result.s.len-1 .. result.s.len-1].expandOneRepRange(n)
    else:
      check(
        false, 
        ("Invalid repetition range, either " &
         "char, shorthand (i.e: \\w), group, or set " &
         "expected before repetition range"))

func populateUid(exp: Exp): Exp =
  check(
    exp.s.high < NodeUid.high,
    ("The expression is too long, " &
     "limit is ~$#") %% $NodeUid.high)
  result = exp
  var uid = 1.NodeUid
  for n in mitems result.s:
    n.uid = uid
    inc uid

func joinAtoms(exp: Exp): AtomsExp =
  ## Put a ``~`` joiner between atoms. An atom is
  ## a piece of expression that would loose
  ## meaning when breaking it up (i.e.: ``a~(b|c)*~d``)
  result.s = newSeq[Node](exp.s.len * 2)
  result.s.setLen 0
  var atomsCount = 0
  for n in exp.s:
    case n.kind
    of matchableKind, assertionKind - lookaroundKind, reSkip:
      inc atomsCount
      if atomsCount > 1:
        atomsCount = 1
        result.s.add initJoinerNode()
    of groupStartKind:
      if atomsCount > 0:
        result.s.add initJoinerNode()
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
      doAssert false
    result.s.add n

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
    doAssert false

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
  doAssert op.kind in opKind
  result = newSeqOfCap[Node](ops.len)
  while (ops.len > 0 and
      ops[ops.len - 1].kind in opKind and
      ops[ops.len - 1].kind.hasPrecedence(op.kind)):
    result.add ops.pop()

func popUntilGroupStart(ops: var seq[Node]): seq[Node] =
  result = newSeqOfCap[Node](ops.len)
  while true:
    let op = ops.pop()
    result.add op
    if op.kind == reGroupStart:
      break

func rpn(exp: AtomsExp): RpnExp =
  ## An adaptation of the Shunting-yard algorithm
  ## for producing `Reverse Polish Notation` out of
  ## an expression specified in infix notation.
  ## It supports regex primitives including groups.
  ## The point of doing this is greatly simplifying
  ## the parsing of the regular expression into an NFA.
  ## Suffix notation removes nesting and so it can
  ## be parsed in a linear way instead of recursively
  result.s = newSeq[Node](exp.s.len)
  result.s.setLen 0
  var ops = newSeq[Node]()
  for n in exp.s:
    case n.kind
    of matchableKind, assertionKind, reSkip:
      result.s.add n
    of reGroupStart:
      ops.add n
    of reGroupEnd:
      result.s.add ops.popUntilGroupStart()
      result.s.add n
    of opKind:
      result.s.add ops.popGreaterThan(n)
      ops.add n
    else:
      doAssert false
  # reverse ops
  for i in 1 .. ops.len:
    result.s.add ops[ops.len-i]

func subExps(exp: AtomsExp, parentKind = reLookahead): AtomsExp =
  ## Collect and convert lookaround sub-expressions to RPN
  template n: untyped = result.s[^1]
  result.s = newSeq[Node](exp.s.len)
  result.s.setLen 0
  var i = 0
  var parens = newSeq[bool]()
  while i < exp.s.len:
    if exp.s[i].kind in lookaroundKind:
      result.s.add exp.s[i]
      inc i
      parens.setLen 0
      let i0 = i
      while i < exp.s.len:
        case exp.s[i].kind
        of groupStartKind:
          parens.add true
        of reGroupEnd:
          if parens.len > 0:
            discard parens.pop()
          else:
            break
        else:
          discard
        inc i
      doAssert exp.s[i].kind == reGroupEnd
      let atoms = AtomsExp(s: exp.s[i0 .. i-1])
      n.subExp.rpn = atoms
        .subExps(parentKind = n.kind)
        .rpn
      # Store whether the capts must be reversed
      if parentKind in lookaheadKind:
        n.subExp.reverseCapts = n.kind in lookbehindKind
      else:
        doAssert parentKind in lookbehindKind
        n.subExp.reverseCapts = n.kind in lookaheadKind
      inc i
    else:
      result.s.add exp.s[i]
      inc i

func toAtoms*(
  exp: Exp,
  groups: var GroupsCapture
): AtomsExp {.inline.} =
  result = exp
    .fixEmptyOps
    .fillGroups(groups)
    .greediness
    .applyFlags
    .expandRepRange
    .populateUid
    .joinAtoms

func transformExp*(
  exp: Exp,
  groups: var GroupsCapture
): RpnExp {.inline.} =
  result = exp
    .toAtoms(groups)
    .subExps
    .rpn
