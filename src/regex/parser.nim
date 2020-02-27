import unicode
import algorithm
import strutils
import sets
import parseutils

import unicodedb/properties

import nodetype
import common
import scanner

func check(cond: bool, msg: string) =
  if not cond:
    raise newException(RegexError, msg)

func isAsciiPrintable(s: string): bool =
  result = true
  for c in s.runes:
    case c.int
    of ' '.ord .. '~'.ord:
      discard
    else:
      return false

func check(cond: bool, msg: string, at: int, exp: string) =
  if not cond:
    # todo: overflow checks
    const spaces = repeat(' ', "\n".len)
    var exp = exp.replace("\n", spaces)
    var start = max(0, at-15)
    var mark = at
    var expMsg = msg
    expMsg.add("\n")
    if not exp.runeSubStr(start, at-1).isAsciiPrintable:
      start = at-1
      let cleft = "~$# chars~" %% $start
      mark = cleft.len+1
      expMsg.add(cleft)
    elif start > 0:
      let cleft = "~$# chars~" %% $start
      mark = cleft.len+15
      expMsg.add(cleft)
    expMsg.add(exp.runeSubStr(start, 30))
    if start+30 < exp.len:
      expMsg.add("~$# chars~" %% $(exp.len - start - 30))
    expMsg.add("\n")
    expMsg.add(strutils.align("^", mark))
    raise newException(RegexError, expMsg)

template prettyCheck(cond: bool, msg: string) {.dirty.} =
  check(cond, msg, startPos, sc.raw)

func `$`(n: Node): string =
  ## return the string representation
  ## of a `Node`. The string is always
  ## equivalent to the original
  ## expression but not necessarily equal
  # Note this is not complete. Just
  # what needed for debugging so far
  case n.kind
  of reZeroOrMore,
      reOneOrMore,
      reZeroOrOne:
    if n.isGreedy:
      n.cp.toUTF8 & "?"
    else:
      n.cp.toUTF8
  of reRepRange:
    "#"  # Invalid
  of reStart:
    "\\A"
  of reEnd:
    "\\z"
  of reWordBoundary:
    "\\b"
  of reNotWordBoundary:
    "\\B"
  of shorthandKind:
    '\\' & n.cp.toUTF8
  of reInSet, reNotSet:
    var str = ""
    str.add('[')
    if n.kind == reNotSet:
      str.add('^')
    var
      cps = newSeq[Rune](n.cps.len)
      i = 0
    for cp in n.cps:
      cps[i] = cp
      inc i
    for cp in cps.sorted(cmp):
      str.add(cp.toUTF8)
    for sl in n.ranges:
      str.add(sl.a.toUTF8 & '-' & sl.b.toUTF8)
    for nn in n.shorthands:
      str.add($nn)
    str.add(']')
    str
  of reSkip:
    "SKIP"
  of reEOE:
    "EOE"
  else:
    n.cp.toUTF8

#proc `$`(n: seq[Node]): string {.used.} =
#  result = newStringOfCap(n.len)
#  for nn in n:
#    result.add($nn)

func toShorthandNode(r: Rune): Node =
  ## the given character must be a shorthand or
  ## else a ``CharNode`` is returned
  case r
  of "w".toRune:
    Node(kind: reWord, cp: r)
  of "d".toRune:
    Node(kind: reDigit, cp: r)
  of "s".toRune:
    Node(kind: reWhiteSpace, cp: r)
  of "W".toRune:
    Node(kind: reNotAlphaNum, cp: r)
  of "D".toRune:
    Node(kind: reNotDigit, cp: r)
  of "S".toRune:
    Node(kind: reNotWhiteSpace, cp: r)
  else:
    r.toCharNode

func toAssertionNode(r: Rune): Node =
  ## the given character must be an assertion or
  ## else a ``CharNode`` is returned
  case r
  of "A".toRune:
    Node(kind: reStart, cp: r)
  of "z".toRune:
    Node(kind: reEnd, cp: r)
  of "b".toRune:
    Node(kind: reWordBoundary, cp: r)
  of "B".toRune:
    Node(kind: reNotWordBoundary, cp: r)
  else:
    r.toCharNode

func toEscapedSeqNode(r: Rune): Node =
  ## the given character must be an
  ## escaped sequence or else a regular char
  ## Node is returned
  case r
  of "a".toRune:
    Node(kind: reChar, cp: "\x07".toRune)
  of "f".toRune:
    Node(kind: reChar, cp: "\x0C".toRune)
  of "t".toRune:
    Node(kind: reChar, cp: "\t".toRune)
  of "n".toRune:
    Node(kind: reChar, cp: "\L".toRune)
  of "r".toRune:
    Node(kind: reChar, cp: "\r".toRune)
  of "v".toRune:
    Node(kind: reChar, cp: "\x0B".toRune)
  else:
    r.toCharNode

func toEscapedNode(r: Rune): Node =
  ## return either a shorthand,
  ## an assertion, or a char node
  result = r.toShorthandNode
  if result.kind == reChar:
    result = r.toAssertionNode
  if result.kind == reChar:
    result = r.toEscapedSeqNode

func parseUnicodeLit(sc: Scanner[Rune], size: int): Node =
  let startPos = sc.pos-1
  var rawCP = newString(size)
  for i in 0 ..< size:
    prettyCheck(
      not sc.finished,
      ("Invalid unicode literal. " &
       "Expected $# hex digits, but found $#") %% [$size, $i])
    prettyCheck(
      sc.curr.int in {
        '0'.ord .. '9'.ord,
        'a'.ord .. 'z'.ord,
        'A'.ord .. 'Z'.ord},
      ("Invalid unicode literal. " &
       "Expected hex digit, but found $#") %% $sc.curr)
    rawCP[i] = sc.next().int.char
  var cp = 0
  discard parseHex(rawCP, cp)
  prettyCheck(
    cp != -1 and cp <= int32.high,
    "Invalid unicode literal. $# value is too big" %% rawCP)
  result = Rune(cp).toCharNode

func parseUnicodeLitX(sc: Scanner[Rune]): Node =
  let startPos = sc.pos-1
  assert sc.peek == "{".toRune
  discard sc.next()
  let litEnd = sc.find("}".toRune)
  prettyCheck(
    litEnd != -1,
    "Invalid unicode literal. Expected `}`")
  prettyCheck(
    litEnd <= 8,
    ("Invalid unicode literal. " &
     "Expected at most 8 chars, found $#") %% $litEnd)
  result = parseUnicodeLit(sc, litEnd)
  assert sc.peek == "}".toRune
  discard sc.next()

func parseOctalLit(sc: Scanner[Rune]): Node =
  let startPos = sc.pos
  var rawCP = newString(3)
  for i in 0 ..< 3:
    prettyCheck(
      not sc.finished,
      ("Invalid octal literal. " &
       "Expected 3 octal digits, but found $#") %% $i)
    prettyCheck(
      sc.curr.int in {'0'.ord .. '7'.ord},
      ("Invalid octal literal. " &
       "Expected octal digit, but found $#") %% $sc.curr)
    rawCP[i] = sc.next().int.char
  var cp = 0
  discard parseOct(rawCP, cp)
  result = Rune(cp).toCharNode

func parseCC(s: string): UnicodeCategorySet =
  try:
    result = s.categoryMap.UnicodeCategorySet
  except ValueError:
    try:
      result = s.categorySetMap
    except ValueError:
      check(false, "Invalid unicode name?")

func parseUnicodeNameX(sc: Scanner[Rune]): Node =
  let startPos = sc.pos-1
  assert sc.peek == "{".toRune
  discard sc.next()
  let nameEnd = sc.find("}".toRune)
  prettyCheck(
    nameEnd != -1,
    "Invalid unicode name. Expected `}`")
  var name = newString(nameEnd)
  for i in 0 ..< nameEnd:
    prettyCheck(
      sc.curr.int in {
        'a'.ord .. 'z'.ord,
        'A'.ord .. 'Z'.ord},
      "Invalid unicode name. " &
      "Expected chars in {'a'..'z', 'A'..'Z'}")
    name[i] = sc.next().int.char
  assert sc.peek == "}".toRune
  discard sc.next()
  prettyCheck(
    name in [
      "Cn", "Lu", "Ll", "Lt", "Mn", "Mc", "Me", "Nd", "Nl",
      "No", "Zs", "Zl", "Zp", "Cc", "Cf", "Cs", "Co", "Cn",
      "Lm", "Lo", "Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po",
      "Sm", "Sc", "Sk", "So", "C", "L", "M", "N",
      "Z", "P", "S"],
    "Invalid unicode name. Found $#" %% name)
  result = Node(
    kind: reUCC,
    cp: "#".toRune,
    cc: name.parseCC)

func parseUnicodeName(sc: Scanner[Rune]): Node =
  let startPos = sc.pos-1
  case sc.peek
  of "{".toRune:
    result = parseUnicodeNameX(sc)
  else:
    prettyCheck(
      sc.peek in [
        "C".toRune, "L".toRune, "M".toRune, "N".toRune,
        "Z".toRune, "P".toRune, "S".toRune],
      "Invalid unicode name. Found $#" %% sc.peek.toUTF8)
    result = Node(
      kind: reUCC,
      cp: "¿".toRune,
      cc: sc.next().toUTF8.parseCC)

func parseEscapedSeq(sc: Scanner[Rune]): Node =
  ## Parse a escaped sequence
  case sc.curr
  of "u".toRune:
    discard sc.next()
    result = parseUnicodeLit(sc, 4)
  of "U".toRune:
    discard sc.next()
    result = parseUnicodeLit(sc, 8)
  of "x".toRune:
    discard sc.next()
    case sc.peek
    of "{".toRune:
      result = parseUnicodeLitX(sc)
    else:
      result = parseUnicodeLit(sc, 2)
  of "0".toRune .. "7".toRune:
    result = parseOctalLit(sc)
  of "p".toRune:
    discard sc.next()
    result = parseUnicodeName(sc)
  of "P".toRune:
    discard sc.next()
    result = parseUnicodeName(sc)
    result.kind = reNotUCC
  else:
    result = next(sc).toEscapedNode

func parseSetEscapedSeq(sc: Scanner[Rune]): Node =
  ## Just like regular ``parseEscapedSeq``
  ## but treats assertions as chars (ignore escaping)
  let cp = sc.peek
  result = parseEscapedSeq(sc)
  if result.kind in assertionKind:
    result = cp.toCharNode

func parseAsciiSet(sc: Scanner[Rune]): Node =
  ## Parse an ascii set (i.e: ``[:ascii:]``).
  ## The ascii set will get expanded
  ## and merged with the outer set
  let startPos = sc.pos
  assert sc.peek == ":".toRune
  discard sc.next()
  result = case sc.peek
  of "^".toRune:
    discard sc.next()
    initNotSetNode()
  else:
    initSetNode()
  var name = newStringOfCap(16)
  for r in sc:
    if r == ":".toRune:
      break
    name.add(r.toUTF8)
  prettyCheck(
    sc.peek == "]".toRune,
    "Invalid ascii set. Expected [:name:]")
  discard sc.next
  case name
  of "alpha":
    result.ranges.add([
      "a".toRune .. "z".toRune,
      "A".toRune .. "Z".toRune])
  of "alnum":
    result.ranges.add([
      "0".toRune .. "9".toRune,
      "a".toRune .. "z".toRune,
      "A".toRune .. "Z".toRune])
  of "ascii":
    result.ranges.add(
      "\x00".toRune .. "\x7F".toRune)
  of "blank":
    result.cps.incl(toHashSet([
      "\t".toRune, " ".toRune]))
  of "cntrl":
    result.ranges.add(
      "\x00".toRune .. "\x1F".toRune)
    result.cps.incl("\x7F".toRune)
  of "digit":
    result.ranges.add(
      "0".toRune .. "9".toRune)
  of "graph":
    result.ranges.add(
      "!".toRune .. "~".toRune)
  of "lower":
    result.ranges.add(
      "a".toRune .. "z".toRune)
  of "print":
    result.ranges.add(
      " ".toRune .. "~".toRune)
  of "punct":
    result.ranges.add([
      "!".toRune .. "/".toRune,
      ":".toRune .. "@".toRune,
      "[".toRune .. "`".toRune,
      "{".toRune .. "~".toRune])
  of "space":
    result.cps.incl(toHashSet([
      "\t".toRune, "\L".toRune, "\v".toRune,
      "\f".toRune, "\r".toRune, " ".toRune]))
  of "upper":
    result.ranges.add(
      "A".toRune .. "Z".toRune)
  of "word":
    result.ranges.add([
      "0".toRune .. "9".toRune,
      "a".toRune .. "z".toRune,
      "A".toRune .. "Z".toRune])
    result.cps.incl("_".toRune)
  of "xdigit":
    result.ranges.add([
      "0".toRune .. "9".toRune,
      "a".toRune .. "f".toRune,
      "A".toRune .. "F".toRune])
  else:
    prettyCheck(
      false,
      "Invalid ascii set. `$#` is not a valid name" %% name)

func parseSet(sc: Scanner[Rune]): Node =
  ## parse a set atom (i.e ``[a-z]``) into a
  ## ``Node`` of ``reInSet`` or ``reNotSet`` kind.
  ## This proc is PCRE compatible and
  ## handles a ton of edge cases
  let startPos = sc.pos
  result = case sc.peek
  of "^".toRune:
    discard sc.next()
    initNotSetNode()
  else:
    initSetNode()
  var
    hasEnd = false
    cps = newSeq[Rune]()
  for cp in sc:
    case cp
    of "]".toRune:
      hasEnd = not result.isEmpty or cps.len > 0
      if hasEnd:
        break
      cps.add(cp)
    of "\\".toRune:
      let nn = parseSetEscapedSeq(sc)
      case nn.kind
      of reChar:
        cps.add(nn.cp)
      else:
        assert nn.kind in shorthandKind
        result.shorthands.add(nn)
        # can't be range so discard
        if sc.peek == "-".toRune:
          cps.add(sc.next())
    of "-".toRune:
      if sc.finished:
        # no end
        continue
      if cps.len == 0:
        cps.add(cp)
        continue
      var last: Rune
      case sc.peek
      of "]".toRune:
        cps.add(cp)
        continue
      of "\\".toRune:
        discard sc.next()
        let nn = parseSetEscapedSeq(sc)
        check(
          nn.kind == reChar,
          "Invalid set range. Range can't contain " &
          "a character-class or assertion",
          sc.pos-1,
          sc.raw)
        last = nn.cp
      else:
        assert(not sc.finished)
        last = sc.next()
      let first = cps.pop()
      check(
        first <= last,
        "Invalid set range. " &
        "Start must be lesser than end",
        sc.pos,
        sc.raw)
      result.ranges.add(first .. last)
      if sc.peek == "-".toRune:
        cps.add(sc.next())
    of "[".toRune:
      if sc.peek == ":".toRune:
        # todo: rename shorhands
        result.shorthands.add(parseAsciiSet(sc))
      else:
        cps.add(cp)
    else:
      cps.add(cp)
  # todo: use ref and set to nil when empty
  result.cps.incl(cps.toHashSet)
  prettyCheck(
    hasEnd,
    "Invalid set. Missing `]`")

func parseRepRange(sc: Scanner[Rune]): Node =
  ## parse a repetition range ``{n,m}``
  let startPos = sc.pos
  var
    first, last: string
    hasFirst = false
    curr = ""
  for cp in sc:
    if cp == "}".toRune:
      last = curr
      break
    if cp == ",".toRune:
      first = curr
      curr = ""
      hasFirst = true
      continue
    prettyCheck(
      cp.int in '0'.ord .. '9'.ord,
      "Invalid repetition range. Range can only contain digits")
    curr.add(char(cp.int))
  if not hasFirst:  # {n}
    first = curr
  if first.len == 0:  # {,m} or {,}
    first.add('0')
  if last.len == 0:  # {n,} or {,}
    last = "-1"
  var
    firstNum: int
    lastNum: int
  when (NimMajor, NimMinor, NimPatch) < (0, 19, 9):
    type MyError = ref OverflowError
  else:
    type MyError = ref ValueError
  try:
    discard parseInt(first, firstNum)
    discard parseInt(last, lastNum)
  except MyError:
    prettyCheck(
      false,
      "Invalid repetition range. Max value is $#" %% $int16.high)
  prettyCheck(
    firstNum <= int16.high and
    lastNum <= int16.high,
    "Invalid repetition range. Max value is $#" %% $int16.high)
  # for perf reasons. This becomes a?a?a?...
  # too many parallel states
  prettyCheck(
    not (lastNum - firstNum > 100),
    ("Invalid repetition range. " &
     "Expected 100 repetitions or less, " &
     "but found: $#") %% $(lastNum - firstNum))
  result = Node(
    kind: reRepRange,
    min: firstNum.int16,
    max: lastNum.int16)

func toFlag(r: Rune): Flag =
  result = case r
  of "i".toRune:
    flagCaseInsensitive
  of "m".toRune:
    flagMultiLine
  of "s".toRune:
    flagAnyMatchNewLine
  of "U".toRune:
    flagUnGreedy
  of "u".toRune:
    flagUnicode
  of "x".toRune:
    flagVerbose
  else:
    # todo: return err and show a better error msg
    raise newException(RegexError,
      ("Invalid group flag, found $# " &
       "but expected one of: i, m, s, U or u") %% $r)

func toNegFlag(r: Rune): Flag =
  result = case r
  of "i".toRune:
    flagNotCaseInsensitive
  of "m".toRune:
    flagNotMultiLine
  of "s".toRune:
    flagNotAnyMatchNewLine
  of "U".toRune:
    flagNotUnGreedy
  of "u".toRune:
    flagNotUnicode
  of "x".toRune:
    flagNotVerbose
  else:
    # todo: return err and show a better error msg
    raise newException(RegexError,
      ("Invalid group flag, found -$# " &
       "but expected one of: -i, -m, -s, -U or -u") %% $r)

template checkEmptyGroup() {.dirty.} =
  prettyCheck(
    peek(sc) != toRune(")"),
    "Invalid group. Empty group is not allowed")

func parseGroupTag(sc: Scanner[Rune]): Node =
  ## parse a special group (name, flags, non-captures).
  ## Return a regular ``reGroupStart``
  ## if it's not special enough
  # A regular group
  let startPos = sc.pos
  if sc.peek != "?".toRune:
    checkEmptyGroup()
    result = initGroupStart()
    return
  discard sc.next()  # Consume "?"
  case sc.peek
  of ":".toRune:
    discard sc.next()
    checkEmptyGroup()
    result = initGroupStart(isCapturing = false)
  of "P".toRune:
    discard sc.next()
    prettyCheck(
      sc.peek == "<".toRune,
      "Invalid group name. Missing `<`")
    discard sc.next()  # Consume "<"
    var name = newStringOfCap(75)
    for r in sc:
      if r == ">".toRune:
        break
      prettyCheck(
        r.int in {
          'a'.ord .. 'z'.ord,
          'A'.ord .. 'Z'.ord,
          '0'.ord .. '9'.ord,
          '-'.ord, '_'.ord},
        ("Invalid group name. Expected char in " &
         "{'a'..'z', 'A'..'Z', '0'..'9', '-', '_'}, " &
         "but found `$#`") %% $r)
      name.add(r.int.char)
    prettyCheck(
      name.len > 0,
      "Invalid group name. Name can't be empty")
    prettyCheck(
      sc.prev == ">".toRune,
      "Invalid group name. Missing `>`")
    checkEmptyGroup()
    result = initGroupStart(name)
  of "i".toRune,
      "m".toRune,
      "s".toRune,
      "U".toRune,
      "u".toRune,
      "x".toRune,
      "-".toRune:
    var
      flags: seq[Flag] = @[]
      isNegated = false
    for cp in sc:
      if cp == ":".toRune:
        checkEmptyGroup()
        break
      if cp == "-".toRune:
        isNegated = true
        continue
      if isNegated:
        flags.add(toNegFlag(cp))
      else:
        flags.add(toFlag(cp))
      if sc.peek == ")".toRune:
        break
    result = initGroupStart(
      flags = flags,
      isCapturing = false)
  #reLookahead,
  #reLookbehind,
  of "=".toRune:
    discard sc.next()
    # todo: support sets and more
    case sc.peek
    of "\\".toRune:
      let n = parseEscapedSeq(sc)
      prettyCheck(
        n.kind == reChar,
        "Invalid lookahead. A " &
        "character was expected, but " &
        "found a special symbol")
      result = Node(kind: reLookahead, cp: n.cp)
    else:
      prettyCheck(
        not sc.finished,
        "Invalid lookahead. A character " &
        "was expected, but found nothing (end of string)")
      result = Node(kind: reLookahead, cp: sc.next())
    prettyCheck(
      sc.peek == ")".toRune,
      "Invalid lookahead, expected closing symbol")
    discard sc.next()
  else:
    prettyCheck(
      false,
      "Invalid group. Unknown group type")

func subParse(sc: Scanner[Rune]): Node =
  let r = sc.prev
  case r
  of "\\".toRune:
    sc.parseEscapedSeq()
  of "[".toRune:
    sc.parseSet()
  of "{".toRune:
    sc.parseRepRange()
  of "(".toRune:
    sc.parseGroupTag()
  of "|".toRune:
    Node(kind: reOr, cp: r)
  of "*".toRune:
    Node(kind: reZeroOrMore, cp: r)
  of "+".toRune:
    Node(kind: reOneOrMore, cp: r)
  of "?".toRune:
    Node(kind: reZeroOrOne, cp: r)
  of ")".toRune:
    Node(kind: reGroupEnd, cp: r)
  of "^".toRune:
    Node(kind: reStartSym, cp: r)
  of "$".toRune:
    Node(kind: reEndSym, cp: r)
  of ".".toRune:
    Node(kind: reAny, cp: r)
  else:
    r.toCharNode

func skipWhiteSpace(sc: Scanner[Rune], vb: seq[bool]): bool =
  ## skip white-spaces and comments on verbose mode
  result = false
  if vb.len == 0 or not vb[vb.len-1]:
    return
  result = case sc.prev
  of " ".toRune,
      "\t".toRune,
      "\L".toRune,
      "\r".toRune,
      "\f".toRune,
      "\v".toRune:
    true
  of "#".toRune:
    for r in sc:
      if r == "\L".toRune:
        break
    true
  else:
    false

func verbosity(
  vb: var seq[bool],
  sc: Scanner[Rune],
  n: Node
) =
  ## update verbose mode on current group
  case n.kind:
  of reGroupStart:
    if vb.len > 0:
      vb.add(vb[vb.high])
    else:
      vb.add(false)
    for f in n.flags:
      case f:
      of flagVerbose:
        vb[vb.high] = true
      of flagNotVerbose:
        vb[vb.high] = false
      else:
        discard
    if sc.peek == ")".toRune:  # (?flags)
      if vb.len > 1:  # set outter group
        vb[vb.high - 1] = vb[vb.high]
      else:
        vb.add(vb[vb.high])
  of reGroupEnd:
    if vb.len > 0:
      discard vb.pop()
    # else: unbalanced parentheses,
    # it'll raise later
  else:
    discard

func parse*(expression: string): seq[Node] =
  ## convert a ``string`` regex expression
  ## into a ``Node`` expression
  result = newSeqOfCap[Node](expression.len)
  var vb = newSeq[bool]()
  let sc = expression.scan()
  for _ in sc:
    if sc.skipWhiteSpace(vb): continue
    result.add(sc.subParse())
    vb.verbosity(sc, result[^1])
