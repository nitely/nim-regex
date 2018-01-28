##[
A library for parsing, compiling, and executing
regular expressions. The match time is linear
with respect to the length of the input and
the regular expression. So, it can handle
input from untrusted users. Its syntax is similar to PCRE
but lacks a few features that can not be implemented
while keeping the space/time complexity guarantees,
i.e.: backreferences and look-around assertions.

Syntax
******

Matching one character
######################

.. code-block::
  .          any character except new line (includes new line with s flag)
  \d         digit (\p{Nd})
  \D         not digit
  \pN        One-letter name Unicode character class
  \p{Greek}  Unicode character class (general category or script)
  \PN        Negated one-letter name Unicode character class
  \P{Greek}  negated Unicode character class (general category or script)

Character classes
#################

.. code-block::
  [xyz]         A character class matching either x, y or z (union).
  [^xyz]        A character class matching any character except x, y and z.
  [a-z]         A character class matching any character in range a-z.
  [[:alpha:]]   ASCII character class ([A-Za-z])
  [[:^alpha:]]  Negated ASCII character class ([^A-Za-z])
  [\[\]]        Escaping in character classes (matching [ or ])

Composites
##########

.. code-block::
  xy   concatenation (x followed by y)
  x|y  alternation (x or y, prefer x)

Repetitions
###########

.. code-block::
  x*       zero or more of x (greedy)
  x+       one or more of x (greedy)
  x?       zero or one of x (greedy)
  x*?      zero or more of x (ungreedy/lazy)
  x+?      one or more of x (ungreedy/lazy)
  x??      zero or one of x (ungreedy/lazy)
  x{n,m}   at least n x and at most m x (greedy)
  x{n,}    at least n x (greedy)
  x{n}     exactly n x
  x{n,m}?  at least n x and at most m x (ungreedy/lazy)
  x{n,}?   at least n x (ungreedy/lazy)
  x{n}?    exactly n x

Empty matches
#############

.. code-block::
  ^   the beginning of text (or start-of-line with multi-line mode)
  $   the end of text (or end-of-line with multi-line mode)
  \A  only the beginning of text (even with multi-line mode enabled)
  \z  only the end of text (even with multi-line mode enabled)
  \b  a Unicode word boundary (\w on one side and \W, \A, or \z on other)
  \B  not a Unicode word boundary

Grouping and flags
##################

.. code-block::
  (exp)          numbered capture group (indexed by opening parenthesis)
  (?P<name>exp)  named (also numbered) capture group (allowed chars: [_0-9a-zA-Z])
  (?:exp)        non-capturing group
  (?flags)       set flags within current group
  (?flags:exp)   set flags for exp (non-capturing)

Flags are each a single character. For example,
(?x) sets the flag x and (?-x) clears the flag x.
Multiple flags can be set or cleared at the same
time: (?xy) sets both the x and y flags, (?x-y)
sets the x flag and clears the y flag, and (?-xy)
clears both the x and y flags.

.. code-block::
  i  case-insensitive: letters match both upper and lower case
  m  multi-line mode: ^ and $ match begin/end of line
  s  allow . to match \L (new line)
  U  swap the meaning of x* and x*? (un-greedy mode)
  u  Unicode support (enabled by default)
  x  ignore whitespace and allow line comments (starting with `#`)

`All flags are disabled by default unless stated otherwise`

Escape sequences
################

.. code-block::
  \*         literal *, works for any punctuation character: \.+*?()|[]{}^$
  \a         bell (\x07)
  \f         form feed (\x0C)
  \t         horizontal tab
  \n         new line (\L)
  \r         carriage return
  \v         vertical tab (\x0B)
  \123       octal character code (up to three digits)
  \x7F       hex character code (exactly two digits)
  \x{10FFFF} any hex character code corresponding to a Unicode code point

Perl character classes (Unicode friendly)
#########################################

These classes are based on the definitions provided in
`UTS#18 <http://www.unicode.org/reports/tr18/#Compatibility_Properties>`_

.. code-block::
  \d  digit (\p{Nd})
  \D  not digit
  \s  whitespace (\p{White_Space})
  \S  not whitespace
  \w  word character (\p{Alphabetic} + \p{M} + \d + \p{Pc} + \p{Join_Control})
  \W  not word character

ASCII character classes
#######################

.. code-block::
  [[:alnum:]]   alphanumeric ([0-9A-Za-z])
  [[:alpha:]]   alphabetic ([A-Za-z])
  [[:ascii:]]   ASCII ([\x00-\x7F])
  [[:blank:]]   blank ([\t ])
  [[:cntrl:]]   control ([\x00-\x1F\x7F])
  [[:digit:]]   digits ([0-9])
  [[:graph:]]   graphical ([!-~])
  [[:lower:]]   lower case ([a-z])
  [[:print:]]   printable ([ -~])
  [[:punct:]]   punctuation ([!-/:-@\[-`{-~])
  [[:space:]]   whitespace ([\t\n\v\f\r ])
  [[:upper:]]   upper case ([A-Z])
  [[:word:]]    word characters ([0-9A-Za-z_])
  [[:xdigit:]]  hex digit ([0-9A-Fa-f])

]##

import unicode
import algorithm
import strutils
import sets
import tables
import options

import unicodedb
import unicodeplus except isUpper, isLower

export Option, isSome, get

type
  RegexError* = object of ValueError
  ## raised when the pattern
  ## is not a valid regex

proc check(cond: bool, msg: string) =
  if not cond:
    raise newException(RegexError, msg)

const
  # This is used as start
  # and end of string. It should
  # be invalid code, but while it
  # works it simplifies things a bit.
  # An alternative would be opt[Rune]
  # or just using int32 and convert
  # Rune to int when needed
  invalidRune = Rune(-1)
  # `\n` is platform specific in
  # Nim and not the actual `\n`
  lineBreakRune = Rune(10)

proc toRune(s: string): Rune =
  result = s.runeAt(0)

type
  SetRange = tuple
    rangeStart: Rune
    rangeEnd: Rune
  Flag = enum
    flagCaseInsensitive,  # i
    flagNotCaseInsensitive,  # -i
    flagMultiLine,  # m
    flagNotMultiLine,  # -m
    flagAnyMatchNewLine,  # s
    flagNotAnyMatchNewLine,  # -s
    flagUnGreedy,  # U
    flagNotUnGreedy,  # -U
    flagUnicode,  # u
    flagNotUnicode  # -u
  NodeKind = enum
    reChar,
    reCharCI,
    reJoiner,  # ~
    reGroupStart,  # (
    reGroupEnd,  # )
    reOr,  # |
    reZeroOrMore,  # *
    reOneOrMore,  # +
    reZeroOrOne,  # ?
    reRepRange,  # {n,m}
    reStartSym,  # ^
    reEndSym,  # $
    reStartSymML,  # ^ multi-line
    reEndSymML,  # $ multi-line
    reStart,  # \A
    reEnd,  # \z
    reWordBoundary,  # \b
    reNotWordBoundary,  # \B
    reAlphaNum,  # \w
    reDigit,  # \d
    reWhiteSpace,  # \s
    reNotAlphaNum,  # \W
    reNotDigit,  # \D
    reNotWhiteSpace,  # \S
    reAny,  # .
    reAnyNL,  # . new-line
    reWordBoundaryAscii,  # \b ascii only
    reNotWordBoundaryAscii,  # \B ascii only
    reAlphaNumAscii,  # \w ascii only
    reDigitAscii,  # \d ascii only
    reWhiteSpaceAscii,  # \s ascii only
    reNotAlphaNumAscii,  # \W ascii only
    reNotDigitAscii,  # \D ascii only
    reNotWhiteSpaceAscii,  # \S ascii only
    reAnyAscii,  # . ascii only
    reAnyNLAscii,  # . new-line ascii only
    reSet,  # [abc]
    reNotSet,  # [^abc]
    reSkip,  # dummy
    reEOE  # End of expression
  Node = object
    kind: NodeKind
    cp: Rune
    outA, outB: int32
    isGreedy: bool
    # reGroupStart, reGroupEnd
    idx: int16  # todo: rename?
    isCapturing: bool
    name: string
    flags: seq[Flag]
    # reRepRange
    min, max: int16
    # reSet, reNotSet
    cps: HashSet[Rune]
    ranges: seq[SetRange]  # todo: interval tree
    shorthands: seq[Node]

proc toCharNode(r: Rune): Node =
  ## return a ``Node`` that is meant to be matched
  ## against text characters
  Node(kind: reChar, cp: r)

proc initJoinerNode(): Node =
  ## return a ``Node`` of ``reJoiner`` kind.
  ## Joiners are temporary nodes,
  ## they serve to generate the NFA
  ## but they are never part of it
  Node(kind: reJoiner, cp: "~".toRune)

proc initEOENode(): Node =
  ## return the end-of-expression ``Node``.
  ## This is a dummy node that marks a match as successful
  Node(kind: reEOE, cp: "¿".toRune, outA: -1, outB: -1)

template initSetNodeCommon(k: NodeKind): Node =
  ## base node
  assert k in {reSet, reNotSet}
  Node(
    kind: k,
    cp: "¿".toRune,
    cps: initSet[Rune](),
    ranges: @[],
    shorthands: @[])

proc initSetNode(): Node =
  ## return a set ``Node``,
  ## parsed from an expression such as ``[a-z]``
  initSetNodeCommon(reSet)

proc initNotSetNode(): Node =
  ## return a negated set ``Node``,
  ## parsed from an expression such as ``[^a-z]``
  initSetNodeCommon(reNotSet)

proc initGroupStart(
    name: string = nil,
    flags: seq[Flag] = nil,
    isCapturing = true): Node =
  ## return a ``reGroupStart`` node
  Node(
    kind: reGroupStart,
    cp: "(".toRune,
    name: name,
    flags: flags,
    isCapturing: isCapturing)

proc isEmpty(n: Node): bool =
  ## check if a set ``Node`` is empty
  assert n.kind in {reSet, reNotSet}
  result = (
    n.cps.len == 0 and
    n.ranges.len == 0 and
    n.shorthands.len == 0)

proc isAlphaNumAscii(r: Rune): bool {.inline.} =
  ## return ``true`` if the given
  ## rune is in ``[A-Za-z0-9]`` range
  r.int in {
    'A'.ord .. 'Z'.ord,
    'a'.ord .. 'z'.ord,
    '0'.ord .. '9'.ord}

template isWordBoundaryImpl(r, nxt, alnumProc): bool =
  let
    isWord = r != invalidRune and alnumProc(r)
    isNxtWord = nxt != invalidRune and alnumProc(nxt)
  ((isWord and not isNxtWord) or
   (not isWord and isNxtWord))

proc isWordBoundary(r: Rune, nxt: Rune): bool {.inline.} =
  ## check if current match
  ## is a boundary (i.e the end of a word)
  isWordBoundaryImpl(r, nxt, isalnum)

proc isWordBoundaryAscii(r: Rune, nxt: Rune): bool {.inline.} =
  ## check if current match
  ## is a boundary. Match ascii only
  isWordBoundaryImpl(r, nxt, isAlphaNumAscii)

proc match(n: Node, r: Rune, nxt: Rune): bool =
  ## match for ``Node`` of assertion kind.
  ## Return whether the node matches
  ## the current characters or not
  case n.kind
  of reStart, reStartSym:
    r == invalidRune
  of reEnd, reEndSym:
    nxt == invalidRune
  of reStartSymML:
    (r == invalidRune or
     r == lineBreakRune)
  of reEndSymML:
    (nxt == invalidRune or
     nxt == lineBreakRune)
  of reWordBoundary:
    isWordBoundary(r, nxt)
  of reNotWordBoundary:
    not isWordBoundary(r, nxt)
  of reWordBoundaryAscii:
    isWordBoundaryAscii(r, nxt)
  of reNotWordBoundaryAscii:
    not isWordBoundaryAscii(r, nxt)
  else:
    doAssert(false)
    false

proc `<=`(x, y: Rune): bool =
  x.int <= y.int

proc contains(sr: seq[SetRange], r: Rune): bool =
  result = false
  for first, last in sr.items:
    if first <= r and r <= last:
      result = true
      break

proc isWhiteSpace(r: Rune): bool {.inline.} =
  result = (
    r.int in {
      ' '.ord,
      '\t'.ord,
      '\L'.ord,
      '\r'.ord,
      '\f'.ord,
      '\v'.ord} or
    r.category()[0] == 'Z')

proc isWhiteSpaceAscii(r: Rune): bool {.inline.} =
  r.int in {
    ' '.ord,
    '\t'.ord,
    '\L'.ord,
    '\r'.ord,
    '\f'.ord,
    '\v'.ord}

# todo: can not use unicodeplus due to
# https://github.com/nim-lang/Nim/issues/7059
proc swapCase(r: Rune): Rune =
  # Note a character can be
  # non-lower and non-upper
  if r.isUpper():
    result = r.toLower()
  elif r.isLower():
    result = r.toUpper()
  else:
    result = r

proc match(n: Node, r: Rune): bool =
  ## match for ``Node`` of matchable kind.
  ## Return whether the node matches
  ## the current character or not
  assert r != invalidRune
  case n.kind
  of reEOE:
    false
  of reAlphaNum:
    r.isAlnum()
  of reNotAlphaNum:
    not r.isAlnum()
  of reDigit:
    r.isNumeric()
  of reNotDigit:
    not r.isNumeric()
  of reWhiteSpace:
    r.isWhiteSpace()
  of reNotWhiteSpace:
    not r.isWhiteSpace()
  of reSet, reNotSet:
    var matches = (
      r in n.cps or
      r in n.ranges)
    if not matches:
      for nn in n.shorthands:
        matches = nn.match(r)
    if n.kind == reSet:
      matches
    else:
      not matches
  of reAny:
    r != lineBreakRune
  of reAnyNL:
    true
  of reCharCI:
    r == n.cp or r == n.cp.swapCase()
  of reAlphaNumAscii:
    r.isAlphaNumAscii()
  of reDigitAscii:
    r.int in {'0'.ord .. '9'.ord}
  of reWhiteSpaceAscii:
    r.isWhiteSpaceAscii()
  of reNotAlphaNumAscii:
    not r.isAlphaNumAscii()
  of reNotDigitAscii:
    r.int notin {'0'.ord .. '9'.ord}
  of reNotWhiteSpaceAscii:
    not r.isWhiteSpaceAscii()
  of reAnyAscii:
    (r.int in {0 .. 127} and
     r != lineBreakRune)
  of reAnyNLAscii:
    r.int in {0 .. 127}
  else:
    assert n.kind == reChar
    n.cp == r

const
  opKind = {
    reJoiner,
    reOr,
    reZeroOrMore,
    reOneOrMore,
    reZeroOrOne,
    reRepRange}
  assertionKind = {
    reStartSym,
    reEndSym,
    reStartSymML,
    reEndSymML,
    reStart,
    reEnd,
    reWordBoundary,
    reNotWordBoundary,
    reWordBoundaryAscii,
    reNotWordBoundaryAscii}
  shorthandKind = {
    reAlphaNum,
    reDigit,
    reWhiteSpace,
    reNotAlphaNum,
    reNotDigit,
    reNotWhiteSpace,
    reAlphaNumAscii,
    reDigitAscii,
    reWhiteSpaceAscii,
    reNotAlphaNumAscii,
    reNotDigitAscii,
    reNotWhiteSpaceAscii}
  matchableKind = {
    reChar,
    reCharCI,
    reAlphaNum,
    reDigit,
    reWhiteSpace,
    reNotAlphaNum,
    reNotDigit,
    reNotWhiteSpace,
    reAny,
    reAnyNL,
    reSet,
    reNotSet,
    reAlphaNumAscii,
    reDigitAscii,
    reWhiteSpaceAscii,
    reNotAlphaNumAscii,
    reNotDigitAscii,
    reNotWhiteSpaceAscii,
    reAnyAscii,
    reAnyNLAscii}
  repetitionKind = {
    reZeroOrMore,
    reOneOrMore,
    reRepRange}

proc cmp(x, y: Rune): int =
  x.int - y.int

proc `$`(n: Node): string =
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
    "¿"  # Invalid
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
  of reSet, reNotSet:
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
    for rs, re in n.ranges.items:
      str.add(rs.toUTF8 & '-' & re.toUTF8)
    for nn in n.shorthands:
      str.add('\\' & nn.cp.toUTF8)
    str.add(']')
    str
  of reSkip:
    "SKIP"
  of reEOE:
    "EOE"
  else:
    n.cp.toUTF8

proc `$`(n: seq[Node]): string =
  result = newStringOfCap(n.len)
  for nn in n:
    result.add($nn)

type
  Scanner[T: Rune|Node] = ref object
    ## A scanner is a common
    ## construct for reading data
    s: seq[T]
    pos: int

proc newScanner[T](s: seq[T]): Scanner[T] =
  Scanner[T](s: s, pos: 0)

proc scan[T](s: seq[T]): Scanner[T] =
  newScanner(s)

iterator items[T](sc: Scanner[T]): T =
  ## the yielded item gets consumed
  while sc.pos <= sc.s.high:
    inc sc.pos
    yield sc.s[sc.pos - 1]

proc prev[T](sc: Scanner[T]): T =
  sc.s[sc.pos - 1]

proc curr[T](sc: Scanner[T]): T =
  sc.s[sc.pos]

proc next[T](sc: Scanner[T]): T =
  ## return current item and consume it
  result = sc.s[sc.pos]
  inc sc.pos

proc peekImpl[T](sc: Scanner[T], default: T): T {.inline.} =
  ## same as ``curr`` except it
  ## returns a default/invalid value when
  ## the data is fully consumed
  if sc.pos >= sc.s.high:
    default
  else:
    sc.s[sc.pos]

proc peek(sc: Scanner[Rune]): Rune =
  peekImpl(sc, invalidRune)

proc peek(sc: Scanner[Node]): Node =
  peekImpl(sc, initEOENode())

iterator peek[T](sc: Scanner[T]): (T, T) =
  for s in sc:
    yield (s, sc.peek)

proc toShorthandNode(r: Rune): Node =
  ## the given character must be a shorthand or
  ## else a ``CharNode`` is returned
  case r
  of "w".toRune:
    Node(kind: reAlphaNum, cp: r)
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

proc toAssertionNode(r: Rune): Node =
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

proc toEscapedSeqNode(r: Rune): Node =
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

proc toEscapedNode(r: Rune): Node =
  ## return either a shorthand,
  ## an assertion, or a char node
  result = r.toShorthandNode
  if result.kind == reChar:
    result = r.toAssertionNode
  if result.kind == reChar:
    result = r.toEscapedSeqNode

proc toSetEscapedNode(r: Rune): Node =
  ## return either a shorthand or a char node
  result = r.toShorthandNode
  if result.kind == reChar:
    result = r.toEscapedSeqNode

proc parseSet(sc: Scanner[Rune]): seq[Node] =
  ## parse a set atom (i.e ``[a-z]``) into a
  ## ``Node`` of ``reSet`` or ``reNotSet`` kind.
  ## This proc is PCRE compatible and
  ## handles a ton of edge cases
  var n = case sc.curr
  of "^".toRune:
    discard sc.next()
    initNotSetNode()
  else:
    initSetNode()
  var
    isRange, isEscaped, hasEnd = false
    cps = newSeq[Rune]()
  for cp, nxt in sc.peek:
    if cp == "]".toRune and
        not isEscaped and
        (not n.isEmpty or cps.len > 0):
      hasEnd = true
      break
    if cp == "\\".toRune and not isEscaped:
      isEscaped = true
      continue
    if isRange:
      doAssert(cps.len > 0)
      var cp = cp
      if isEscaped:
        let nn = cp.toSetEscapedNode()
        doAssert(nn.kind == reChar,
          "invalid range in character class")
        cp = nn.cp
      isRange = false
      isEscaped = false
      let start = cps.pop()
      doAssert(start <= cp)
      n.ranges.add((
        rangeStart: start,
        rangeEnd: cp))
      if nxt == "-".toRune:
        cps.add(nxt)
        discard sc.next()
      continue
    if isEscaped:
      isEscaped = false
      let nn = cp.toSetEscapedNode()
      if nn.kind == reChar:
        cps.add(nn.cp)
        continue
      doAssert(nn.kind in shorthandKind)
      n.shorthands.add(nn)
      if nxt == "-".toRune:
        cps.add(nxt)
        discard sc.next()
      continue
    if cp == "-".toRune and cps.len > 0:
      isRange = true
      continue
    cps.add(cp)
  if isRange:
    cps.add("-".toRune)
  n.cps = cps.toSet
  doAssert(not isEscaped)
  doAssert(not n.isEmpty)
  doAssert(hasEnd)
  result = @[n]

proc parseRepRange(sc: Scanner[Rune]): seq[Node] =
  ## parse a repetition range ``{n,m}``
  var
    first, last: string
    curr = ""
  for cp in sc:
    if cp == "}".toRune:
      last = curr
      break
    if cp == ",".toRune:
      first = curr
      curr = ""
      continue
    check(
      cp.int in '0'.ord .. '9'.ord,
      "Invalid repetition range near position " &
      $sc.pos & ", can only contain [0-9]")
    curr.add(char(cp.int))
  if first.isNil:  # {n}
    first = curr
  if first.len == 0:  # {,m} or {,}
    first.add('0')
  if last.len == 0:  # {n,} or {,}
    last = "-1"
  var
    firstNum: int16
    lastNum: int16
  try:
    firstNum = first.parseInt().int16
    lastNum = last.parseInt().int16
  except ValueError, RangeError:
    raise newException(RegexError,
      "Invalid repetition " &
      "range near position " & $sc.pos &
      ", max value is " & $int16.high &
      ", but found: " & first & ", " & last)
  # for perf reasons. This becomes a?a?a?...
  # too many parallel states
  check(
    not (lastNum - firstNum > 100),
    "Invalid repetition range near position " & $sc.pos &
    ", can't have a range greater than 100 " &
    "repetitions, but found: " & $(lastNum - firstNum))
  result = @[Node(
    kind: reRepRange,
    min: firstNum,
    max: lastNum)]

proc toFlag(r: Rune): Flag =
  case r
  of "i".toRune:
    result = flagCaseInsensitive
  of "m".toRune:
    result = flagMultiLine
  of "s".toRune:
    result = flagAnyMatchNewLine
  of "U".toRune:
    result = flagUnGreedy
  of "u".toRune:
    result = flagUnicode
  else:
    doAssert(false)

proc toNegFlag(r: Rune): Flag =
  case r
  of "i".toRune:
    result = flagNotCaseInsensitive
  of "m".toRune:
    result = flagNotMultiLine
  of "s".toRune:
    result = flagNotAnyMatchNewLine
  of "U".toRune:
    result = flagNotUnGreedy
  of "u".toRune:
    result = flagNotUnicode
  else:
    doAssert(false)

proc parseGroupTag(sc: Scanner[Rune]): seq[Node] =
  ## parse a special group (name, flags, non-captures).
  ## Return a regular ``reGroupStart``
  ## if it's not special enough
  # A regular group
  if sc.curr != "?".toRune:
    return @[initGroupStart()]
  discard sc.next()  # Consume "?"
  case sc.curr
  of ":".toRune:
    discard sc.next()
    result = @[initGroupStart(isCapturing = false)]
  of "P".toRune:
    discard sc.next()
    doAssert(sc.curr == "<".toRune)
    discard sc.next()  # Consume "<"
    var name = newStringOfCap(75)
    for r in sc:
      if r == ">".toRune:
        break
      name.add(r.toUTF8)
    doAssert(name.len > 0)
    result = @[initGroupStart(name)]
  of "i".toRune,
      "m".toRune,
      "s".toRune,
      "U".toRune,
      "u".toRune,
      "-".toRune:
    var
      flags: seq[Flag] = @[]
      isNegated = false
    for cp in sc:
      if cp == ":".toRune:
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
    result = @[initGroupStart(
      flags = flags,
      isCapturing = false)]
    if sc.peek == ")".toRune:
      result.add(Node(
        kind: reSkip,
        cp: "¿".toRune))  # todo: remove?
  else:
    doAssert(false)
    discard

proc subParse(sc: Scanner[Rune]): seq[Node] =
  let r = sc.prev
  case r
  of "[".toRune:
    sc.parseSet()
  of "{".toRune:
    sc.parseRepRange()
  of "(".toRune:
    sc.parseGroupTag()
  of "|".toRune:
    @[Node(kind: reOr, cp: r)]
  of "*".toRune:
    @[Node(kind: reZeroOrMore, cp: r)]
  of "+".toRune:
    @[Node(kind: reOneOrMore, cp: r)]
  of "?".toRune:
    @[Node(kind: reZeroOrOne, cp: r)]
  of ")".toRune:
    @[Node(kind: reGroupEnd, cp: r)]
  of "^".toRune:
    @[Node(kind: reStartSym, cp: r)]
  of "$".toRune:
    @[Node(kind: reEndSym, cp: r)]
  of ".".toRune:
    @[Node(kind: reAny, cp: r)]
  of "\\".toRune:
    @[sc.next().toEscapedNode]
  else:
    @[r.toCharNode]

proc parse(expression: string): seq[Node] =
  ## convert a ``string`` regex expression
  ## into a ``Node`` expression
  result = newSeqOfCap[Node](expression.len)
  let sc = expression.toRunes.scan()
  for _ in sc:
    result.add(sc.subParse())

proc greediness(expression: seq[Node]): seq[Node] =
  ## apply greediness to an expression
  result = newSeqOfCap[Node](expression.len)
  let sc = expression.scan()
  for nn in sc:
    var n = nn
    if (n.kind in repetitionKind or
        n.kind == reZeroOrOne) and
        sc.peek.kind == reZeroOrOne:
      n.isGreedy = true
      discard sc.next
    result.add(n)

type
  GroupsCapture = tuple
    count: int16
    names: Table[string, int]

proc fillGroups(expression: var seq[Node]): GroupsCapture =
  ## populate group indices, names and capturing mark
  var
    groups = newSeq[int]()
    nonCapt = 0
    names = initTable[string, int]()
    count = 0'i16
  for i, n in expression.mpairs:
    case n.kind
    of reGroupStart:
      groups.add(i)
      if n.isCapturing:
        n.idx = count
        inc count
      else:
        inc nonCapt
      if not n.name.isNil:
        assert n.isCapturing
        names[n.name] = n.idx
    of reGroupEnd:
      let start = groups.pop()
      n.isCapturing = expression[start].isCapturing
      n.idx = expression[start].idx
      if not n.isCapturing:
        dec nonCapt
    else:
      discard
    doAssert(count < int16.high)
  doAssert(groups.len == 0)
  result = (
    count: count,
    names: names)

proc toAsciiKind(k: NodeKind): NodeKind =
  case k
  of reWordBoundary:
    reWordBoundaryAscii
  of reNotWordBoundary:
    reNotWordBoundaryAscii
  of reAlphaNum:
    reAlphaNumAscii
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

proc toggle(f: Flag): Flag =
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

proc squash(flags: seq[seq[Flag]]): set[Flag] =
  result = {}
  for ff in flags:
    for f in ff:
      result.excl(f.toggle())
      result.incl(f)

proc applyFlags(expression: seq[Node]): seq[Node] =
  ## apply flags to each group
  result = newSeqOfCap[Node](expression.len)
  let sc = expression.scan()
  var flags = newSeq[seq[Flag]]()  # todo: ofCap(groupsCount)
  for nn in sc:
    var n = nn
    for f in flags.squash():
      # todo: move to its own function
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
        # todo: support sets?
        if n.kind == reChar and n.cp != n.cp.swapCase():
          n.kind = reCharCI
      of flagUnGreedy:
        if n.kind in opKind:
          n.isGreedy = not n.isGreedy
      of flagNotUnicode:
        n.kind = n.kind.toAsciiKind()
        # todo: remove, add reSetAscii and reNotSetAscii
        if n.kind in {reSet, reNotSet}:
          for nn in n.shorthands.mitems:
            nn.kind = nn.kind.toAsciiKind()
      else:
        doAssert(f in {
          flagNotAnyMatchNewLine,
          flagNotMultiLine,
          flagNotCaseInsensitive,
          flagNotUnGreedy,
          flagUnicode})
    case n.kind
    # (?flags)
    # Orphan flags are added to current group
    of reGroupStart:
      if n.flags.isNil:
        flags.add(@[])
        result.add(n)
        continue
      if sc.peek.kind == reSkip:
        discard sc.next()  # SKIP
        doAssert(sc.peek.kind == reGroupEnd)
        discard sc.next()  # )
        if flags.len > 0:
          flags[^1].add(n.flags)
        else:
          flags.add(n.flags)
        continue  # skip (
      flags.add(n.flags)
    of reGroupEnd:
      discard flags.pop()
    else:
      discard
    result.add(n)

proc expandOneRepRange(subExpr: seq[Node], n: Node): seq[Node] =
  ## expand a repetition-range expression
  ## into the equivalent repeated expression
  doAssert(n.kind == reRepRange)
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
    doAssert(n.min < n.max)
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

proc expandRepRange(expression: seq[Node]): seq[Node] =
  ## expand every repetition range
  result = newSeqOfCap[Node](expression.len)
  var i: int
  for n in expression:
    if n.kind != reRepRange:
      result.add(n)
      continue
    doAssert(result.len > 0)
    case result[^1].kind
    of reGroupEnd:
      i = 0
      for ne in result.reversed:
        inc i
        if ne.kind == reGroupStart:
          break
      assert result[result.len - i].kind == reGroupStart
      result.add(result[result.len - i .. ^1].expandOneRepRange(n))
    of matchableKind:
      result.add(@[result[^1]].expandOneRepRange(n))
    else:
      doAssert(
        false,
        "either char or shorthand (i.e: \\w) " &
        "expected before repetition range")

proc joinAtoms(expression: seq[Node]): seq[Node] =
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
      doAssert(false, "Unhandled node")
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

proc opsPA(nk: NodeKind): OpsPA =
  ## return the precedence and
  ## associativity of a given node kind
  doAssert(nk in opKind)
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
    doAssert(false, "Unhandled nodeKind")

proc hasPrecedence(a: NodeKind, b: NodeKind): bool =
  ## Check ``b`` has precedence over ``a``.
  ## Both ``a`` and ``b`` are expected to
  ## be valid operators. Unary operators such
  ## as: ``*``, ``?`` and ``+`` have right-to-left
  ## associativity. Binary operators
  ## such as: ``|`` (or) and ``~`` (joiner) have
  ## left-to-right associativity
  result = (
    (opsPA(b).associativity == asyRight and
      opsPA(b).precedence <= opsPA(a).precedence) or
    (opsPA(b).associativity == asyLeft and
      opsPA(b).precedence < opsPA(a).precedence))

proc popGreaterThan(ops: var seq[Node], op: Node): seq[Node] =
  doAssert(op.kind in opKind)
  result = newSeqOfCap[Node](ops.len)
  while (ops.len > 0 and
      ops[^1].kind in opKind and
      ops[^1].kind.hasPrecedence(op.kind)):
    result.add(ops.pop())

proc popUntilGroupStart(ops: var seq[Node]): seq[Node] =
  result = newSeqOfCap[Node](ops.len)
  while true:
    let op = ops.pop()
    result.add(op)
    if op.kind == reGroupStart:
      break

proc rpn(expression: seq[Node]): seq[Node] =
  ## An adaptation of the Shunting-yard algorithm
  ## for producing `Reverse Polish Notation` out of
  ## an expression specified in infix notation.
  ## It supports regex primitives including groups.
  ## The point of doing this is greatly simplifying
  ## the parsing of the regular expression into an NFA.
  ## Suffix notation removes nesting and so it can
  ## be parsed in a linear way instead of recursively
  result = newSeqOfCap[Node](expression.len)
  var ops: seq[Node] = @[]
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
      doAssert(false, "unhandled node")
  ops.reverse()
  result.add(ops)

type
  End = seq[int32]
    ## store all the last
    ## states of a given state.
    ## Avoids having to recurse
    ## a state to find its ends,
    ## but have to keep them up-to-date

template combine(
    nfa: var seq[Node],
    ends: var seq[End],
    org: int32,
    target: int32) =
  ## combine ends of ``org``
  ## with ``target``
  for e in ends[org]:
    if nfa[e].outA == 0:
      nfa[e].outA = target
    if nfa[e].outB == 0:
      nfa[e].outB = target
  ends[org] = ends[target]

template update(
    ends: var seq[End],
    ni: int32,
    outA: int32,
    outB: int32) =
  ## update the ends of Node ``ni``
  ## to point to ends of ``n.outA``
  ## and ``n.outB``. If either outA
  ## or outB are ``0`` (EOE),
  ## the ends will point to itself
  ends[ni].setLen(0)
  if outA == 0:
    ends[ni].add(ni)
  elif outA > 0:
    ends[ni].add(ends[outA])
  if outB == 0:
    ends[ni].add(ni)
  elif outB > 0:
    ends[ni].add(ends[outB])

proc nfa(expression: seq[Node]): seq[Node] =
  ## A stack machine to convert a
  ## expression (in RPN) to an NFA, linearly.
  # The e-transitions are kept,
  # removing them showed minimal speed improvements
  result = newSeqOfCap[Node](expression.len + 1)
  result.add(initEOENode())
  var
    ends = newSeq[End](expression.len + 1)
    states = newSeq[int32]()
  ends.fill(@[])
  if expression.len == 0:
    states.add(0)
  for nn in expression:
    doAssert(result.high + 1 <= int32.high)
    var n = nn
    let ni = int32(result.high + 1)
    case n.kind
    of matchableKind, assertionKind:
      n.outA = 0
      n.outB = -1
      ends.update(ni, n.outA, n.outB)
      result.add(n)
      states.add(ni)
    of reJoiner:
      let
        stateB = states.pop()
        stateA = states.pop()
      result.combine(ends, stateA, stateB)
      states.add(stateA)
    of reOr:
      n.outB = states.pop()
      n.outA = states.pop()
      ends.update(ni, n.outA, n.outB)
      result.add(n)
      states.add(ni)
    of reZeroOrMore:
      n.outA = states.pop()
      n.outB = 0
      ends.update(ni, n.outA, n.outB)
      result.combine(ends, n.outA, ni)
      result.add(n)
      states.add(ni)
      if n.isGreedy:
        swap(result[^1].outA, result[^1].outB)
    of reOneOrMore:
      n.outA = states.pop()
      n.outB = 0
      ends.update(ni, n.outA, n.outB)
      result.combine(ends, n.outA, ni)
      result.add(n)
      states.add(n.outA)
      if n.isGreedy:
        swap(result[^1].outA, result[^1].outB)
    of reZeroOrOne:
      n.outA = states.pop()
      n.outB = 0
      ends.update(ni, n.outA, n.outB)
      result.add(n)
      states.add(ni)
      if n.isGreedy:
        swap(result[^1].outA, result[^1].outB)
    of reGroupStart:
      n.outA = states.pop()
      n.outB = -1
      ends.update(ni, n.outA, n.outB)
      result.add(n)
      states.add(ni)
    of reGroupEnd:
      n.outA = 0
      n.outB = -1
      ends.update(ni, n.outA, n.outB)
      let stateA = states.pop()
      result.combine(ends, stateA, ni)
      result.add(n)
      states.add(stateA)
    else:
      doAssert(false, "Unhandled node: $#" % $n.kind)
  doAssert(states.len == 1)
  result.add(Node(
    kind: reSkip,
    cp: "¿".toRune,
    outA: states[0],
    outB: -1'i32))

type
  Regex* = object
    ## a compiled regular expression
    states: seq[Node]
    groupsCount: int16
    namedGroups: Table[string, int]
  RegexMatch* = object
    ## result from matching operations
    captures: seq[Slice[int]]
    groups: seq[Slice[int]] # todo: remove, merge with captures
    namedGroups: Table[string, int]
  CaptureKind = enum
    captStart
    captEnd
  Capture = object
    kind: CaptureKind
    prev: int
    idx: int16
    cpIdx: int
  State = tuple
    ## temporary state to store node's
    ## index and capture's index while matching
    ni: int32
    ci: int

iterator group(m: RegexMatch, i: int): Slice[int] =
  ## return slices for a given group.
  ## Slices of end > start are empty
  ## matches (i.e.: ``re"(\d?)"``)
  ## and they are included same as in PCRE.
  for idx in m.groups[i]:
    yield m.captures[idx]

proc group(m: RegexMatch, i: int): seq[Slice[int]] =
  ## return slices for a given group.
  ## Use the iterator version if you care about performance
  m.captures[m.groups[i]]

iterator group(m: RegexMatch, s: string): Slice[int] =
  ## return slices for a given group
  for idx in m.groups[m.namedGroups[s]]:
    yield m.captures[idx]

proc group(m: RegexMatch, s: string): seq[Slice[int]] =
  ## return slices for a given group.
  ## Use the iterator version if you care about performance
  m.group(m.namedGroups[s])

proc groupsCount(m: RegexMatch): int =
  ## return the number of capturing groups
  ##
  ## .. code-block::
  ##   for gi in 0 ..< m.groupsCount:
  ##     for slice in m.group(gi):
  ##       echo text[slice]
  ##
  m.groups.len

# todo: remove?
proc groups2(m: RegexMatch): seq[seq[Slice[int]]] =
  ## return slices for each group.
  result = newSeq[seq[Slice[int]]](m.groups.len)
  var j = 0
  for i, g in m.groups:
    result[i] = newSeq[Slice[int]](m.groups[i].b - m.groups[i].a + 1)
    j = 0
    for idx in g:
      result[i][j] = m.captures[idx]
      inc j

proc stringify(pattern: Regex, nIdx: int, visited: var set[int16]): string =
  ## NFA to string representation.
  ## For debugging purposes
  if nIdx.int16 in visited:
    return "[...]"
  visited.incl(nIdx.int16)
  let n = pattern.states[nIdx]
  result = "["
  result.add($n)
  if n.outA != -1:
    result.add(", ")
    result.add(pattern.stringify(n.outA, visited))
  if n.outB != -1:
    result.add(", ")
    result.add(pattern.stringify(n.outB, visited))
  result.add("]")

proc `$`(pattern: Regex): string =
  ## NFA to string representation.
  ## For debugging purposes
  var visited: set[int16] = {}
  result = pattern.stringify(pattern.states.high, visited)

type
  BitSet = object
    ## a bitset is a seq
    ## with set capabilities.
    ## It doesn't allow duplicates.
    ## It's O(1) time and O(n)
    ## space complexity. Unlike
    ## regular bitsets, it offers
    ## amortized O(1) time ``clear``
    ## at the cost of memory space
    s: seq[int32]
    key: int32

proc initBitSet(size: int): BitSet =
  BitSet(s: newSeq[int32](size), key: 1)

proc clear(bss: var BitSet) =
  if bss.key == bss.key.high:
    bss.s.fill(0)
    bss.key = 0
  inc bss.key

proc incl(bss: var BitSet, x: int) =
  bss.s[x] = bss.key

proc contains(bss: var BitSet, x: int): bool =
  bss.s[x] == bss.key

type
  ElasticSeq[T] = object
    ## a seq that can grow and shrink
    ## avoiding excessive allocations
    s: seq[T]
    pos: int

proc initElasticSeq[T](size = 16): ElasticSeq[T] =
  ElasticSeq[T](s: newSeq[T](size), pos: 0)

proc isInitialized[T](ls: ElasticSeq[T]): bool =
  not ls.s.isNil

proc `[]`[T](ls: ElasticSeq[T], i: int): T =
  ls.s[i]

proc len[T](ls: ElasticSeq[T]): int =
  ls.pos

proc clear[T](ls: var ElasticSeq[T]) =
  ls.pos = 0

proc add[T](ls: var ElasticSeq[T], x: T) =
  if ls.pos > ls.s.high:
    ls.s.setLen(ls.s.len * 2)
  ls.s[ls.pos] = x
  inc ls.pos

proc pop[T](ls: var ElasticSeq[T]): T =
  dec ls.pos
  ls.s[ls.pos]

iterator items[T](ls: ElasticSeq[T]): T {.inline.} =
  var i = 0
  while i < ls.pos:
    yield ls.s[i]
    inc i

type
  States = tuple
    states: ElasticSeq[State]
    ids: BitSet

proc initStates(size: int): States =
  result = (
    states: initElasticSeq[State](),
    ids: initBitSet(size))

proc `[]`(ss: States, i: int): State =
  ss.states[i]

proc len(ss: States): int =
  ss.states.len

proc clear(ss: var States) =
  ss.states.clear()
  ss.ids.clear()

proc add(ss: var States, s: State) =
  if s.ni notin ss.ids:
    ss.states.add(s)
    ss.ids.incl(s.ni)

iterator items(ss: States): State {.inline.} =
  for s in ss.states.items:
    yield s

proc populateCaptures(
    result: var RegexMatch,
    captured: ElasticSeq[Capture],
    cIdx: int,
    gc: int) =
  # calculate slices for every group,
  # then calculate slices for each match
  # (a group can have multiple matches).
  # Note the given `capture` is in reverse order (leaf to root)
  result.groups = newSeq[Slice[int]](gc)
  var
    curr = cIdx
    ci = 0
  while curr != 0:
    let c = captured[curr]
    inc result.groups[c.idx].b
    inc ci
    curr = c.prev
  var gi = 0
  for g in result.groups.mitems:
    if g.b > 0:
      gi.inc(g.b div 2)
      g.a = gi
      g.b = gi - 1
    else:
      g.b = -1  # 0 .. -1
  assert ci mod 2 == 0
  result.captures = newSeq[Slice[int]](ci div 2)
  curr = cIdx
  while curr != 0:
    let
      c = captured[curr]
      g = result.groups[c.idx]
    case c.kind
    of captEnd:
      result.captures[g.a - 1].b = c.cpIdx
    of captStart:
      result.captures[g.a - 1].a = c.cpIdx
      dec result.groups[c.idx].a
    curr = c.prev

iterator runesIt(s: string): (int, Rune) {.inline.} =
  ## yield current Rune and
  ## the ending index/start of next rune
  var
    i = 0
    result: Rune
  while i < len(s):
    fastRuneAt(s, i, result, true)
    yield (i, result)

iterator runesIt(s: seq[Rune]): (int, Rune) {.inline.} =
  # no-op
  for i, r in s:
    yield (i, r)

iterator peek[T: seq[Rune] | string](s: T): (int, Rune, Rune) {.inline.} =
  ## iterates over unicode characters yielding
  ## next rune index, current rune and next rune
  var
    prev = invalidRune
    j = 0
  for i, r in s.runesIt:
    yield (j, prev, r)
    prev = r
    j = i
  yield (j, prev, invalidRune)

proc toVisitStep(
    result: var ElasticSeq[State],
    n: Node,
    cIdx: int) {.inline.} =
  if n.outB != -1:
    result.add((ni: n.outB, ci: cIdx))
  if n.outA != -1:
    result.add((ni: n.outA, ci: cIdx))

proc step(
    result: var States,
    pattern: Regex,
    nIdx: int32,
    captured: var ElasticSeq[Capture],
    cIdx: int,
    visited: var BitSet,
    toVisit: var ElasticSeq[State],
    cpIdx: int,
    cp: Rune,
    nxt: Rune) =
  assert toVisit.len == 0
  toVisit.add((ni: nIdx, ci: cIdx))
  while toVisit.len > 0:
    let state = toVisit.pop()
    if state.ni in visited:
      continue
    visited.incl(state.ni)
    let n = pattern.states[state.ni]
    case n.kind
    of matchableKind, reEOE:
      result.add(state)
    of assertionKind:
      if n.match(cp, nxt):
        toVisitStep(toVisit, n, state.ci)
    of reGroupStart:
      if not (n.isCapturing and
          captured.isInitialized):
        toVisitStep(toVisit, n, state.ci)
        continue
      captured.add(Capture(
        kind: captStart,
        cpIdx: cpIdx,
        prev: state.ci,
        idx: n.idx))
      toVisitStep(toVisit, n, captured.len - 1)
    of reGroupEnd:
      if not (n.isCapturing and
          captured.isInitialized):
        toVisitStep(toVisit, n, state.ci)
        continue
      captured.add(Capture(
        kind: captEnd,
        cpIdx: cpIdx - 1,
        prev: state.ci,
        idx: n.idx))
      toVisitStep(toVisit, n, captured.len - 1)
    else:
      toVisitStep(toVisit, n, state.ci)

proc stepFrom(
    result: var States,
    n: Node,
    pattern: Regex,
    captured: var ElasticSeq[Capture],
    cIdx: int,
    visited: var BitSet,
    toVisit: var ElasticSeq[State],
    cpIdx: int,
    cp: Rune,
    nxt: Rune) {.inline.} =
  ## go to next states
  if n.outA != -1:
    result.step(
      pattern, n.outA, captured, cIdx, visited, toVisit, cpIdx, cp, nxt)
  if n.outB != -1:
    result.step(
      pattern, n.outB, captured, cIdx, visited, toVisit, cpIdx, cp, nxt)

proc setRegexMatch(
    result: var Option[RegexMatch],
    states: States,
    pattern: Regex,
    captured: ElasticSeq[Capture]) {.inline.} =
  for ni, ci in states.items:
    if pattern.states[ni].kind == reEOE:
      var m = RegexMatch()
      if pattern.groupsCount > 0:
        m.populateCaptures(captured, ci, pattern.groupsCount)
        m.namedGroups = pattern.namedGroups
      result = some(m)
      return

template initDataSets(withCaptures) {.dirty.} =
  var
    visited = initBitSet(pattern.states.len)
    toVisit = initElasticSeq[State]()
    captured: ElasticSeq[Capture]
    currStates = initStates(pattern.states.len)
    nextStates = initStates(pattern.states.len)
  when withCaptures:
    if pattern.groupsCount > 0:
      captured = initElasticSeq[Capture]()
      captured.add(Capture())

proc match*(s: string | seq[Rune], pattern: Regex): Option[RegexMatch] =
  ## return a match if the whole string
  ## matches the regular expression. This
  ## is similar to ``find(text, re"^regex$")``
  ## but has better performance
  ##
  ## .. code-block::
  ##   assert "abcd".match(re"abcd").isSome == true
  ##   assert "abcd".match(re"abc").isSome == false
  ##
  initDataSets(true)
  let statesCount = pattern.states.high.int32
  for i, cp, nxt in s.peek:
    if cp == invalidRune:
      assert currStates.len == 0
      assert i == 0
      currStates.step(
        pattern, statesCount, captured, 0,
        visited, toVisit, i, cp, nxt)
      continue
    if currStates.len == 0:
      break
    for ni, ci in currStates.items:
      let n = pattern.states[ni]
      if not n.match(cp):
        continue
      visited.clear()
      nextStates.stepFrom(
        n, pattern, captured, ci, visited, toVisit, i, cp, nxt)
    swap(currStates, nextStates)
    nextStates.clear()
  setRegexMatch(result, currStates, pattern, captured)

proc contains*(s: string | seq[Rune], pattern: Regex): bool =
  ##  search for the pattern anywhere
  ##  in the string. It returns as soon
  ##  as there is a match, even when the
  ##  expression has repetitions. Use
  ##  `re"^regex$"` to match the whole
  ##  string instead of searching
  ##
  ## .. code-block::
  ##   assert re"bc" in "abcd"
  ##   assert re"(23)+" in "23232"
  ##   assert re"^(23)+$" notin "23232"
  ##
  initDataSets(false)
  let statesCount = pattern.states.high.int32
  for _, cp, nxt in s.peek:
    visited.clear()
    currStates.step(
      pattern, statesCount, captured, 0,
      visited, toVisit, 0, cp, nxt)
    if cp == invalidRune:
      continue
    for ni, _ in currStates.items:
      let n = pattern.states[ni]
      result = n.kind == reEOE
      if result:
        return
      if not n.match(cp):
        continue
      visited.clear()
      nextStates.stepFrom(
        n, pattern, captured, 0, visited, toVisit, 0, cp, nxt)
    swap(currStates, nextStates)
    nextStates.clear()
  for ni, _ in currStates.items:
    result = pattern.states[ni].kind == reEOE
    if result:
      return

proc find*(s: string | seq[Rune], pattern: Regex): Option[RegexMatch] =
  ## search through the string looking for the first
  ## location where there is a match
  ##
  ## .. code-block::
  ##   assert "abcd".find(re"bc").isSome == true
  ##   assert "abcd".find(re"de").isSome == false
  ##   assert("2222".find(re"(22)*").get().group(0) ==
  ##     @[Slice[int](a: 0, b: 1), Slice[int](a: 2, b: 3)])
  ##
  initDataSets(true)
  let statesCount = pattern.states.high.int32
  for i, cp, nxt in s.peek:
    visited.clear()
    currStates.step(
      pattern, statesCount, captured, 0,
      visited, toVisit, i, cp, nxt)
    if currStates.len > 0 and
        pattern.states[currStates[0].ni].kind == reEOE:
      break
    if cp == invalidRune:
      continue
    for ni, ci in currStates.items:
      let n = pattern.states[ni]
      if n.kind == reEOE:
        nextStates.add((ni: ni, ci: ci))
        continue
      if not n.match(cp):
        continue
      visited.clear()
      nextStates.stepFrom(
        n, pattern, captured, ci, visited, toVisit, i, cp, nxt)
    swap(currStates, nextStates)
    nextStates.clear()
  setRegexMatch(result, currStates, pattern, captured)

proc toPattern*(s: string): Regex =
  ## Parse and compile a regular expression.
  ## Use the ``re`` template if you
  ## care about performance.
  var ns = s.parse
  let gc = ns.fillGroups()
  var names: Table[string, int]
  if gc.names.len > 0:
    names = gc.names
  result = Regex(
    states: ns.greediness.applyFlags.expandRepRange.joinAtoms.rpn.nfa,
    groupsCount: gc.count,
    namedGroups: names)

template re*(s: string): Regex =
  ## Parse and compile a regular
  ## expression at compile-time
  const pattern = toPattern(s)
  pattern

when isMainModule:
  proc isMatch(s: string, pattern: Regex): bool =
    s.match(pattern).isSome

  proc toAtoms(s: string): string =
    s.parse.greediness.applyFlags.expandRepRange.joinAtoms.`$`

  proc toNfaStr(s: string): string =
    let pattern = Regex(
      states: s.parse.greediness.applyFlags.expandRepRange.joinAtoms.rpn.nfa)
    result = $pattern

  proc toStrCaptures(
      m: Option[RegexMatch],
      s: string): seq[seq[string]] =
    assert m.isSome
    result = @[]
    for g in 0 ..< m.get().groupsCount:
      result.add(@[])
      for slice in m.get().group(g):
        result[^1].add(s[slice])

  proc matchWithCapt(s: string, pattern: Regex): seq[seq[string]] =
    s.match(pattern).toStrCaptures(s)

  proc findWithCapt(s: string, pattern: Regex): seq[seq[string]] =
    s.find(pattern).toStrCaptures(s)

  proc raises(pattern: string): bool =
    result = false
    try:
      discard pattern.toPattern()
    except RegexError:
      result = true

  doAssert(toAtoms(r"a(b|c)*d") == r"a~(b|c)*~d")
  doAssert(toAtoms(r"abc") == r"a~b~c")
  doAssert(toAtoms(r"(abc|def)") == r"(a~b~c|d~e~f)")
  doAssert(toAtoms(r"(abc|def)*xyz") == r"(a~b~c|d~e~f)*~x~y~z")
  doAssert(toAtoms(r"a*b") == r"a*~b")
  doAssert(toAtoms(r"(a)b") == r"(a)~b")
  doAssert(toAtoms(r"(a)(b)") == r"(a)~(b)")
  doAssert(toAtoms(r"\y") == r"y")
  doAssert(toAtoms(r"a\*b") == r"a~*~b")
  doAssert(toAtoms(r"\(a\)") == r"(~a~)")
  doAssert(toAtoms(r"\w") == r"\w")
  doAssert(toAtoms(r"\d") == r"\d")
  doAssert(toAtoms(r"[a-z]") == r"[a-z]")
  doAssert(toAtoms(r"[aa-zz]") == r"[aza-z]")
  doAssert(toAtoms(r"[aa\-zz]") == r"[-az]")
  doAssert(toAtoms(r"[^a]") == r"[^a]")
  doAssert(toAtoms(r"(a*)*") != toAtoms(r"a*"))
  doAssert(toAtoms(r"(a*|b*)*") != toAtoms(r"(a|b)*"))
  doAssert(toAtoms(r"(a*b*)*") != toAtoms(r"(a|b)*"))
  doAssert(toAtoms(r"(a*|b*)") != toAtoms(r"(a|b)*"))

  # tfull_match
  doAssert("".isMatch(re""))
  doAssert("a".isMatch(re"a"))
  doAssert("ab".isMatch(re"(a)b"))
  doAssert("aa".isMatch(re"(a)*"))
  doAssert("aab".isMatch(re"((a)*b)"))
  doAssert("abbbbccccd".isMatch(re"a(b|c)*d"))
  doAssert("abbb".isMatch(re"((a)*(b)*)"))
  doAssert("abbb".isMatch(re"((a(b)*)*(b)*)"))
  doAssert("a".isMatch(re"a|b"))
  doAssert("b".isMatch(re"a|b"))
  doAssert(not "ab".isMatch(re"a(b|c)*d"))
  doAssert(not "a".isMatch(re"b"))
  doAssert(not "a".isMatch(re""))

  # trepetition_cycle
  doAssert("aaa".isMatch(re"a**"))
  doAssert("aaa".isMatch(re"(a*)*"))
  doAssert("aaabbbaaa".isMatch(re"((a*|b*))*"))
  doAssert("aaa".isMatch(re"a*****"))
  #doAssert("aaa".isMatch(re"a*{,}"))  # must raise
  doAssert("aaa".isMatch(re"(a?)*"))
  doAssert("aaaa".isMatch(re"((a)*(a)*)*"))

  # tcaptures
  doAssert("ab".matchWithCapt(re"(a)b") == @[@["a"]])
  doAssert("aa".matchWithCapt(re"(a)*") == @[@["a", "a"]])
  doAssert(
    "aab".matchWithCapt(re"((a)*b)") ==
    @[@["aab"], @["a", "a"]])
  doAssert(
    "abbbbccccd".matchWithCapt(re"a(b|c)*d") ==
    @[@["b", "b", "b", "b", "c", "c", "c", "c"]])
  doAssert(
    "abbb".matchWithCapt(re"((a)*(b)*)") ==
    @[@["abbb"], @["a"], @["b", "b", "b"]])
  doAssert(
    "abbb".matchWithCapt(re"((a(b)*)*(b)*)") ==
    @[@["abbb"], @["abbb"], @["b", "b", "b"], @[]])
  doAssert("aa".matchWithCapt(re"(a)+") == @[@["a", "a"]])
  doAssert("abab".matchWithCapt(re"(ab)+") == @[@["ab", "ab"]])
  doAssert("a".matchWithCapt(re"(a)?") == @[@["a"]])
  doAssert("ab".matchWithCapt(re"(ab)?") == @[@["ab"]])
  doAssert(
    "aaabbbaaa".matchWithCapt(re"(a*|b*)*") ==
    @[@["aaa", "bbb", "aaa"]])
  doAssert(
    "abab".matchWithCapt(re"(a(b))*") ==
    @[@["ab", "ab"], @["b", "b"]])
  # Following two should match the same
  doAssert(
    "aaanasdnasd".matchWithCapt(re"((a)*n?(asd)*)*") ==
    @[@["aaanasd", "nasd"], @["a", "a", "a"], @["asd", "asd"]])
  doAssert(
    "aaanasdnasd".matchWithCapt(re"((a)*n?(asd))*") ==
    @[@["aaanasd", "nasd"], @["a", "a", "a"], @["asd", "asd"]])
  doAssert(
    "b".matchWithCapt(re"(a)?b") ==
    @[newSeq[string]()])
  doAssert(
    "ฅa".matchWithCapt(re"(\w)(a)") ==
    @[@["ฅ"], @["a"]])

  # tone_or_more_op
  doAssert("aaaa".isMatch(re"a+"))
  doAssert("abb".isMatch(re"ab+"))
  doAssert("abaa".isMatch(re"aba+"))
  doAssert(not "".isMatch(re"a+"))
  doAssert(not "b".isMatch(re"a+"))
  doAssert(not "aab".isMatch(re"b+"))

  # tzero_or_one_op
  doAssert("a".isMatch(re"a?"))
  doAssert("".isMatch(re"a?"))
  doAssert("a".isMatch(re"ab?"))
  doAssert("ab".isMatch(re"ab?"))
  doAssert("aba".isMatch(re"ab?a"))
  doAssert("aa".isMatch(re"ab?a"))
  doAssert(not "aa".isMatch(re"a?"))
  doAssert(not "b".isMatch(re"a?"))
  doAssert(not "abb".isMatch(re"ab?"))

  # tescape
  doAssert("(a)".isMatch(re"\(a\)"))
  doAssert("a*b".isMatch(re"a\*b"))
  doAssert("a*bbb".isMatch(re"a\*b*"))
  doAssert("y".isMatch(re"\y"))
  doAssert("\\".isMatch(re"\\"))
  doAssert("\\\\".isMatch(re"\\\\"))

  # talphanum_shorthand
  doAssert("a".isMatch(re"\w"))
  doAssert("abc123".isMatch(re"\w*"))
  doAssert("a".matchWithCapt(re"(\w)") == @[@["a"]])

  # tdigit
  doAssert("1".isMatch(re"\d"))
  doAssert("123".isMatch(re"\d*"))
  doAssert("۲".isMatch(re"\d"))  # Kharosthi numeral
  doAssert("⅕".isMatch(re"\d"))

  # twhite_space_shorthand
  doAssert(" ".isMatch(re"\s"))
  doAssert("   ".isMatch(re"\s*"))
  doAssert(" \t\r\f\v".isMatch(re"\s*"))
  doAssert("\u20".isMatch(re"\s"))  # New Line
  doAssert("\u2028".isMatch(re"\s"))  # Line separator

  # talphanum_not_shorthand
  doAssert(not "a".isMatch(re"\W"))
  doAssert(not "abc123".isMatch(re"\W*"))
  doAssert("!@#".isMatch(re"\W+"))

  # tnot_digit
  doAssert(not "1".isMatch(re"\D"))
  doAssert(not "123".isMatch(re"\D*"))
  doAssert(not "۲".isMatch(re"\D"))  # Kharosthi numeral
  doAssert(not "⅕".isMatch(re"\D"))
  doAssert("!@#".isMatch(re"\D+"))
  doAssert("a".isMatch(re"\D"))

  # tnot_white_space_shorthand
  doAssert("asd123!@#".isMatch(re"\S*"))
  doAssert(not " ".isMatch(re"\S"))
  doAssert(not "   ".isMatch(re"\S*"))
  doAssert(not "\t".isMatch(re"\S"))
  doAssert(not "\u20".isMatch(re"\S"))
  doAssert(not "\r".isMatch(re"\S"))
  doAssert(not "\f".isMatch(re"\S"))
  doAssert(not "\v".isMatch(re"\S"))
  doAssert(not "\u2028".isMatch(re"\S"))  # Line separator

  # tset
  doAssert("a".isMatch(re"[a]"))
  doAssert("a".isMatch(re"[abc]"))
  doAssert("b".isMatch(re"[abc]"))
  doAssert("c".isMatch(re"[abc]"))
  doAssert(not "d".isMatch(re"[abc]"))
  doAssert("a".isMatch(re"[\w]"))
  doAssert("1".isMatch(re"[\w]"))
  doAssert("1".isMatch(re"[\d]"))
  doAssert("*".isMatch(re"[*]"))
  doAssert("*".isMatch(re"[\*]"))
  doAssert("*".isMatch(re"[a*]"))
  doAssert("a".isMatch(re"[a*]"))
  doAssert("a".isMatch(re"[a-z]"))
  doAssert("f".isMatch(re"[a-z]"))
  doAssert("z".isMatch(re"[a-z]"))
  doAssert(not "A".isMatch(re"[a-z]"))
  doAssert("0".isMatch(re"[0-9]"))
  doAssert("5".isMatch(re"[0-9]"))
  doAssert("9".isMatch(re"[0-9]"))
  doAssert(not "a".isMatch(re"[0-9]"))
  doAssert("(".isMatch(re"[()[\]{}]"))
  doAssert(")".isMatch(re"[()[\]{}]"))
  doAssert("}".isMatch(re"[()[\]{}]"))
  doAssert("{".isMatch(re"[()[\]{}]"))
  doAssert("[".isMatch(re"[()[\]{}]"))
  doAssert("]".isMatch(re"[()[\]{}]"))
  doAssert("(".isMatch(re"[]()[{}]"))
  doAssert(")".isMatch(re"[]()[{}]"))
  doAssert("}".isMatch(re"[]()[{}]"))
  doAssert("{".isMatch(re"[]()[{}]"))
  doAssert("[".isMatch(re"[]()[{}]"))
  doAssert("]".isMatch(re"[]()[{}]"))
  doAssert("\\".isMatch(re"[\\]"))
  doAssert("\\".isMatch(re"[\\\]]"))
  doAssert("]".isMatch(re"[\\\]]"))
  doAssert("00".isMatch(re"[0-5][0-9]"))
  doAssert("59".isMatch(re"[0-5][0-9]"))
  doAssert(not "95".isMatch(re"[0-5][0-9]"))
  doAssert("1".isMatch(re"[0-57-9]"))
  doAssert("8".isMatch(re"[0-57-9]"))
  doAssert(not "6".isMatch(re"[0-57-9]"))
  doAssert("4".isMatch(re"[0-9A-Fa-f]"))
  doAssert("b".isMatch(re"[0-9A-Fa-f]"))
  doAssert("B".isMatch(re"[0-9A-Fa-f]"))
  doAssert(not "-".isMatch(re"[0-9A-Fa-f]"))
  doAssert("-".isMatch(re"[a\-z]"))
  doAssert("a".isMatch(re"[a\-z]"))
  doAssert("z".isMatch(re"[a\-z]"))
  doAssert(not "b".isMatch(re"[a\-z]"))
  doAssert("a".isMatch(re"[a-]"))
  doAssert("-".isMatch(re"[a-]"))
  doAssert("+".isMatch(re"[(+*)]"))
  doAssert("*".isMatch(re"[(+*)]"))
  doAssert("(".isMatch(re"[(+*)]"))
  doAssert("[".isMatch(re"[[-\]]"))
  doAssert("]".isMatch(re"[[-\]]"))
  doAssert(not "-".isMatch(re"[[-\]]"))
  doAssert("(".isMatch(re"[(-\)]"))
  doAssert(")".isMatch(re"[(-\)]"))
  doAssert(not "-".isMatch(re"[(-\)]"))
  doAssert("\\".isMatch(re"[\\-\\)]"))
  doAssert(not "-".isMatch(re"[\\-\\)]"))
  doAssert("-".isMatch(re"[-]"))
  doAssert("-".isMatch(re"[\-]"))
  doAssert("-".isMatch(re"[\-\-]"))
  doAssert("-".isMatch(re"[\--]"))
  doAssert("-".isMatch(re"[\--\-]"))
  doAssert("-".isMatch(re"[\---]"))
  doAssert("b".isMatch(re"[\--\-a-z]"))
  doAssert("b".isMatch(re"[\---a-z]"))
  doAssert("b".isMatch(re"[-a-z]"))
  doAssert("-".isMatch(re"[-a-z]"))
  doAssert("a".isMatch(re"[-a]"))
  doAssert("-".isMatch(re"[-a]"))
  doAssert("b".isMatch(re"[a-d-z]"))
  doAssert("-".isMatch(re"[a-d-z]"))
  doAssert("z".isMatch(re"[a-d-z]"))
  doAssert(not "e".isMatch(re"[a-d-z]"))
  doAssert("]".isMatch(re"[]]"))
  doAssert("]".isMatch(re"[\]]"))
  doAssert(not "[".isMatch(re"[]]"))
  doAssert(not "]]".isMatch(re"[]]"))
  doAssert(not "-".isMatch(re"[[-\]]"))
  doAssert(not "b".isMatch(re"[c-d]"))
  doAssert("-".isMatch(re"[a\w-\wz]"))
  doAssert("-".isMatch(re"[\w-a]"))
  doAssert("-".isMatch(re"[\w-]"))
  doAssert("a".isMatch(re"[\w-a]"))
  doAssert("1".isMatch(re"[\w-a]"))
  doAssert("-".isMatch(re"[db-c-f]"))
  doAssert(not "e".isMatch(re"[db-c-f]"))
  doAssert(not "-".isMatch(re"[=-_]"))
  doAssert("A".isMatch(re"[\A]"))
  doAssert("b".isMatch(re"[\b]"))
  doAssert("zz".isMatch(re"[\z][\z]"))
  doAssert(not "z".isMatch(re"[\z][\z]"))
  # range out of order in character class (must raise)
  # doAssert("b".isMatch(re"[d-c]"))
  # invalid range in character class (must raise)
  # doAssert("-".isMatch(re"[a-\w]"))

  # tnot_set
  doAssert("a".matchWithCapt(re"([^b])") == @[@["a"]])
  doAssert("asd".matchWithCapt(re"([^b]*)") == @[@["asd"]])
  doAssert("ab".matchWithCapt(re"([^b]*b)") == @[@["ab"]])
  doAssert(
    "asd123".matchWithCapt(re"([^\d]*)(\d*)") ==
    @[@["asd"], @["123"]])
  doAssert(
    "asd123".matchWithCapt(re"([asd]*)([^asd]*)") ==
    @[@["asd"], @["123"]])
  doAssert(
    "<asd123!@#>".matchWithCapt(re"(<[^>]*>)") ==
    @[@["<asd123!@#>"]])
  doAssert(not "a".isMatch(re"[^a]"))
  #doAssert("^".isMatch(re"[^]"))  # should raise missing end
  doAssert("^".isMatch(re"[\^]"))
  doAssert("a".isMatch(re"[\^a]"))
  doAssert(not "^".isMatch(re"[^^]"))
  doAssert("a".isMatch(re"[^^]"))
  doAssert("a".isMatch(re"[^-]"))
  doAssert(not "-".isMatch(re"[^-]"))

  # trepetition_range_expand
  doAssert(r"a{0}".toNfaStr == r"a".toNfaStr)
  doAssert(r"a{0}b".toNfaStr == r"ab".toNfaStr)
  doAssert(r"a{1}".toNfaStr == r"a".toNfaStr)
  doAssert(r"a{10}".toNfaStr == r"aaaaaaaaaa".toNfaStr)
  doAssert(r"a{1,}".toNfaStr == r"aa*".toNfaStr)
  doAssert(r"a{10,}".toNfaStr == r"aaaaaaaaaaa*".toNfaStr)
  doAssert(r"a{10,10}".toNfaStr == r"aaaaaaaaaa".toNfaStr)
  doAssert(r"a{0,0}".toNfaStr == r"a".toNfaStr)
  doAssert(r"a{1,2}".toNfaStr == r"aa?".toNfaStr)
  doAssert(r"a{2,4}".toNfaStr == r"aaa?a?".toNfaStr)
  doAssert(r"a{,10}".toNfaStr == r"a?a?a?a?a?a?a?a?a?a?".toNfaStr)
  doAssert(r"a{0,10}".toNfaStr == r"a?a?a?a?a?a?a?a?a?a?".toNfaStr)
  doAssert(r"a{,}".toNfaStr == r"a*".toNfaStr)

  #trepetition_range
  doAssert(not "".isMatch(re"a{0}"))
  doAssert(not "".isMatch(re"a{0,0}"))
  doAssert(not "".isMatch(re"a{,0}"))
  doAssert("".isMatch(re"a{,2}"))
  doAssert("a".isMatch(re"a{0}"))
  doAssert("a".isMatch(re"a{0,0}"))
  doAssert("a".isMatch(re"a{,0}"))
  doAssert("a".isMatch(re"a{1}"))
  doAssert("aa".isMatch(re"a{2}"))
  doAssert("aaa".isMatch(re"a{3}"))
  doAssert(not "aaaa".isMatch(re"a{3}"))
  doAssert(not "".isMatch(re"a{1}"))
  doAssert("a".isMatch(re"a{1,1}"))
  doAssert("a".isMatch(re"a{1,2}"))
  doAssert("aa".isMatch(re"a{1,2}"))
  doAssert(not "aaa".isMatch(re"a{1,2}"))
  doAssert(not "a".isMatch(re"a{2,4}"))
  doAssert("a".isMatch(re"a{1,}"))
  doAssert("aa".isMatch(re"a{1,}"))
  doAssert("aaa".isMatch(re"a{1,}"))
  doAssert("aaaaaaaaaa".isMatch(re"a{1,}"))
  doAssert("aa".isMatch(re"a{2,}"))
  doAssert("a".isMatch(re"a{,}"))
  doAssert("aa".isMatch(re"a{,}"))
  doAssert("aaaaaaaaaa".isMatch(re"a{,}"))
  doAssert("".isMatch(re"a{,}"))
  doAssert("aaaaaaaaaa".isMatch(re"a{0,}"))
  doAssert("".isMatch(re"a{0,}"))
  doAssert(not "a".isMatch(re"a{2,}"))
  #doAssert("aaa".isMatch(re"a*{,}"))  # must raise
  #doAssert("aaa".isMatch(re"a*{0}"))  # must raise
  #doAssert("aaa".isMatch(re"a*{1}"))  # must raise
  doAssert(
    "aaa".matchWithCapt(re"(a){,}") ==
    @[@["a", "a", "a"]])
  doAssert("aaa".matchWithCapt(re"(a{,}){,}") == @[@["aaa"]])
  doAssert(
    "aaaaa".matchWithCapt(re"(a){5}") ==
    @[@["a", "a", "a", "a", "a"]])
  doAssert("a".matchWithCapt(re"(a){1,5}") == @[@["a"]])
  doAssert(
    "aaa".matchWithCapt(re"(a){1,5}") ==
    @[@["a", "a", "a"]])
  doAssert(
    "".matchWithCapt(re"(a){,}") ==
    @[newSeq[string]()])
  doAssert("aaa".matchWithCapt(re"(a{,}){,}") == @[@["aaa"]])
  doAssert(
    "aaa".matchWithCapt(re"(a{1}){,}") ==
    @[@["a", "a", "a"]])
  doAssert(
    "aaaa".matchWithCapt(re"(a{2}){,}") ==
    @[@["aa", "aa"]])
  doAssert(
    "aaaa".matchWithCapt(re"(a{,3}){,}") ==
    @[@["aaa", "a"]])
  doAssert(
    "".matchWithCapt(re"(a{,3}){,}") ==
    @[newSeq[string]()])
  doAssert(
    "aaa".matchWithCapt(re"(a{1,}){,}") ==
    @[@["aaa"]])
  doAssert(
    "".matchWithCapt(re"(a{1,}){,}") ==
    @[newSeq[string]()])
  doAssert(not "".isMatch(re"(a{1,})"))
  doAssert("a".matchWithCapt(re"(a{1,})") == @[@["a"]])
  doAssert("aaa".matchWithCapt(re"(a{1,})") == @[@["aaa"]])
  doAssert(raises(r"a{bad}"))
  doAssert(raises(r"a{1111111111}"))
  doAssert(raises(r"a{0,101}"))
  doAssert(not raises(r"a{0,100}"))
  doAssert(raises(r"a{0,a}"))
  doAssert(raises(r"a{a,1}"))
  doAssert(raises(r"a{-1}"))

  #tnon_capturing_groups
  doAssert("a".matchWithCapt(re"(?:a)") == @[])
  doAssert("aaa".matchWithCapt(re"(?:aaa)") == @[])
  doAssert(
    "abab".matchWithCapt(re"(a(b))*") ==
    @[@["ab", "ab"], @["b", "b"]])
  doAssert(
    "abab".matchWithCapt(re"(?:a(b))*") ==
    @[@["b", "b"]])
  doAssert(
    "abab".matchWithCapt(re"(a(?:b))*") ==
    @[@["ab", "ab"]])
  doAssert(")".matchWithCapt(re"(\))") == @[@[")"]])

  #tgreediness
  doAssert(
    "a".matchWithCapt(re"(a)??") ==
    @[@["a"]])
  doAssert(
    "aaa".matchWithCapt(re"(a)*(a)*(a)*") ==
    @[@["a", "a", "a"], @[], @[]])
  doAssert(
    "aaa".matchWithCapt(re"(a)*?(a)*(a)*?") ==
    @[@[], @["a", "a", "a"], @[]])
  doAssert(
    "aaa".matchWithCapt(re"(a)*?(a)*?(a)*") ==
    @[@[], @[], @["a", "a", "a"]])
  doAssert(
    "aaa".matchWithCapt(re"(a)*?(a)*?(a)*?") ==
    @[@[], @[], @["a", "a", "a"]])
  doAssert(
    "aaaa".matchWithCapt(re"(a)*?(a)*?(a)*?") ==
    @[@[], @[], @["a", "a", "a", "a"]])
  doAssert(
    "aa".matchWithCapt(re"(a)?(aa?)") ==
    @[@["a"], @["a"]])
  doAssert(
    "aa".matchWithCapt(re"(a)??(a)") ==
    @[@["a"], @["a"]])
  doAssert(
    "aa".matchWithCapt(re"(a)??(aa?)") ==
    @[@[], @["aa"]])
  doAssert(
    "aaa".matchWithCapt(re"(a)+(a)+(a)?") ==
    @[@["a", "a"], @["a"], @[]])
  doAssert(
    "aaa".matchWithCapt(re"(a)+?(a)+(a)?") ==
    @[@["a"], @["a", "a"], @[]])
  doAssert(
    "aaa".matchWithCapt(re"(a)+?(a)+?(a)?") ==
    @[@["a"], @["a"], @["a"]])
  doAssert(
    "aaa".matchWithCapt(re"(a){,}(a){,}(a){,}") ==
    @[@["a", "a", "a"], @[], @[]])
  doAssert(
    "aaa".matchWithCapt(re"(a){,}?(a){,}(a){,}?") ==
    @[@[], @["a", "a", "a"], @[]])
  doAssert(
    "aaa".matchWithCapt(re"(a){,}?(a){,}?(a){,}") ==
    @[@[], @[], @["a", "a", "a"]])
  doAssert(
    "aaa".matchWithCapt(re"(a){,}?(a){,}?(a){,}?") ==
    @[@[], @[], @["a", "a", "a"]])
  doAssert(
    "aaa".matchWithCapt(re"(a){1,}(a){1,}(a)?") ==
    @[@["a", "a"], @["a"], @[]])
  doAssert(
    "aaa".matchWithCapt(re"(a){1,}?(a){1,}(a)?") ==
    @[@["a"], @["a", "a"], @[]])
  doAssert(
    "aaa".matchWithCapt(re"(a){1,}?(a){1,}?(a)?") ==
    @[@["a"], @["a"], @["a"]])
  doAssert(
    "aaaa".match(re"(a*?)(a*?)(a*)").get().groups2() ==
    @[@[0 .. -1], @[0 .. -1], @[0 .. 3]])  # note the empty slices
  doAssert(
    "aaaa".match(re"(a*)(a*?)(a*?)").get().groups2() ==
    @[@[0 .. 3], @[4 .. 3], @[4 .. 3]])  # note the empty slices

  # tassertions
  doAssert(
    "bbaa aa".matchWithCapt(re"([\w ]*?)(\baa\b)") ==
    @[@["bbaa "], @["aa"]])
  doAssert(
    "aa bbaa".matchWithCapt(re"(\baa\b)([\w ]*)") ==
    @[@["aa"], @[" bbaa"]])
  doAssert(
    "This island is great".matchWithCapt(
      re"([\w ]*?)(\bis\b)([\w ]*?)") ==
    @[@["This island "], @["is"], @[" great"]])
  doAssert(
    "bbaabb".matchWithCapt(re"([\w ]*?)(\Baa\B)([\w ]*?)") ==
    @[@["bb"], @["aa"], @["bb"]])
  doAssert(
    "This is my sister".matchWithCapt(
      re"([\w ]*?)(\Bis\B)([\w ]*?)") ==
    @[@["This is my s"], @["is"], @["ter"]])
  doAssert("aa".isMatch(re"\b\b\baa\b\b\b"))
  doAssert("bb".isMatch(re"^^^^bb$$$$"))
  doAssert("bb".isMatch(re"\A\A\A\Abb\z\z\z\z"))

  # tdot_any_matcher
  doAssert("a".isMatch(re"."))
  doAssert("asd123!@#".isMatch(re".*"))
  doAssert("| (•□•) | (❍ᴥ❍ʋ)".isMatch(re".*"))
  doAssert(
    "ฅ^•ﻌ•^ฅ".matchWithCapt(re"(.*)") ==
    @[@["ฅ^•ﻌ•^ฅ"]])
  doAssert("\t".isMatch(re"."))
  doAssert(not "\L".isMatch(re".*"))

  # tgroup
  doAssert("foobar".match(re"(\w*)").get().group(0) == @[0..5])
  doAssert(
    "foobar".match(re"(?P<foo>\w*)").get().group(0) == @[0..5])
  doAssert(
    "ab".match(re"(a)(b)").get().group(0) == @[0..0])
  doAssert(
    "ab".match(re"(a)(b)").get().group(1) == @[1..1])
  doAssert(
    "ab".match(re"(a)(b)").get().groups2() ==
    @[@[0..0], @[1..1]])

  # tnamed_groups
  doAssert(
    "foobar".match(re"(?P<foo>\w*)").get().group("foo") ==
    @[0..5])
  doAssert(
    "foobar".match(re"(?P<foo>(?P<bar>\w*))").get().group("foo") ==
    @[0..5])
  doAssert(
    "foobar".match(re"(?P<foo>(?P<bar>\w*))").get().group("bar") ==
    @[0..5])
  doAssert(
    "aab".match(re"(?P<foo>(?P<bar>a)*b)").get().group("foo") ==
    @[0..2])
  doAssert(
    "aab".match(re"(?P<foo>(?P<bar>a)*b)").get().group("bar") ==
    @[0..0, 1..1])
  doAssert(
    "aab".match(re"((?P<bar>a)*b)").get().group("bar") ==
    @[0..0, 1..1])

  #tflags
  doAssert("foo\Lbar".isMatch(re"(?s).*"))
  doAssert("foo\Lbar".isMatch(re"(?s:.*)"))
  doAssert("foo\Lbar".isMatch(re"(?ssss).*"))
  doAssert(not "foo\Lbar".isMatch(re"(?s-s).*"))
  doAssert(not "foo\Lbar".isMatch(re"(?-s-s-s).*"))
  doAssert(not "foo\Lbar".isMatch(re"(?-ss).*"))
  doAssert(not "foo\Lbar".isMatch(re"(?-ss-ss).*"))
  doAssert(not "foo\Lbar".isMatch(re"(?-sssss-s).*"))
  doAssert(not "foo\Lbar".isMatch(re"(?s-s:.*)"))
  doAssert(not "foo\Lbar".isMatch(re"(?------s----s:.*)"))
  doAssert(
    "foo\Lbar".matchWithCapt(re"((?s:.*))") ==
    @[@["foo\Lbar"]])
  doAssert("a".matchWithCapt(re"((?i:a))") == @[@["a"]])
  doAssert("A".matchWithCapt(re"((?i:a))") == @[@["A"]])
  doAssert(
    "ABC".matchWithCapt(re"((?i:aBc))") ==
    @[@["ABC"]])
  doAssert("a".matchWithCapt(re"((?-i:a))") == @[@["a"]])
  doAssert(not "A".isMatch(re"((?-i:a))"))
  doAssert(not "A".isMatch(re"((?-ii-i:a))"))
  doAssert("a".matchWithCapt(re"((?i)a)") == @[@["a"]])
  doAssert("A".matchWithCapt(re"((?i)a)") == @[@["A"]])
  doAssert("a".matchWithCapt(re"((?-i)a)") == @[@["a"]])
  doAssert(not "A".isMatch(re"((?-i)a)"))
  doAssert("AaA".isMatch(re"(?i)a+"))
  doAssert("AaA".isMatch(re"(?i)A+"))
  doAssert("AbC".isMatch(re"(?i)abc"))
  doAssert(not "b".isMatch(re"(?i)a"))
  doAssert("A".isMatch(re"(?-i)(?i)a"))
  doAssert(not "A".isMatch(re"(?i)(?-i)a"))
  doAssert(
    "AaA".matchWithCapt(re"((?i)a+)") ==
    @[@["AaA"]])

  doAssert(
    "aa".matchWithCapt(re"((?U)a*)(a*)") ==
    @[@[""], @["aa"]])
  doAssert(
    "aa".matchWithCapt(re"((?U)a*?)(a*)") ==
    @[@["aa"], @[""]])
  doAssert(
    "aa".matchWithCapt(re"((?U-U)a*)(a*)") ==
    @[@["aa"], @[""]])
  # no empty matches
  doAssert(
    "aa".matchWithCapt(re"(?U:(a)*)(a)*") ==
    @[@[], @["a", "a"]])
  doAssert(
    "aa".matchWithCapt(re"((?U:a*))(a*)") ==
    @[@[""], @["aa"]])
  doAssert(
    "aa".matchWithCapt(re"((?U:a*?))(a*)") ==
    @[@["aa"], @[""]])
  doAssert(
    "aa".matchWithCapt(re"((?U-U:a*))(a*)") ==
    @[@["aa"], @[""]])

  doAssert(not "a\Lb\L".isMatch(re"(?sm)a.b(?-sm:.)"))
  doAssert("a\Lb\L".isMatch(re"(?ms)a.b(?s-m:.)"))
  doAssert("a\L".isMatch(re"(?s)a."))
  doAssert(not "a\L\L".isMatch(re"(?s)a.$."))
  doAssert("a\L\L".isMatch(re"(?sm)a.$."))
  doAssert(not "a\L\L".isMatch(re"(?-sm)a.$."))
  doAssert(not "a\L\L".isMatch(re"(?s-m)a.$."))
  doAssert("a\L\L".isMatch(re"(?s-m)(?m:a.$.)"))
  doAssert(not "a\L\L".isMatch(re"(?i-sm)(?s:a.$.)"))
  doAssert("a\L\L".isMatch(re"(?i-sm)(?sm:a.$.)"))
  doAssert(not "a\L\L".isMatch(re"(?-sm)(?sm)(?-sm:a.$.)"))
  doAssert("a\L\L".isMatch(re"(?sm)(?-sm)(?sm:a.$.)"))
  doAssert(not "a\L\L".isMatch(re"(?-sm)(?sm:(?-sm:a.$.))"))
  doAssert("a\L\L".isMatch(re"(?sm)(?-sm:(?sm:a.$.))"))

  doAssert("Ǝ".isMatch(re"\w"))
  doAssert("Ǝ".isMatch(re"(?u)\w"))
  doAssert(not "Ǝ".isMatch(re"(?-u)\w"))
  doAssert("abczABCZ0129".isMatch(re"(?-u)\w*"))
  doAssert(not "\t".isMatch(re"(?-u)\w"))
  # todo: test every ascii kind
  doAssert("Ǝ".isMatch(re"(?u)[\w]"))
  doAssert(not "Ǝ".isMatch(re"(?u)[^\w]"))
  doAssert("Ǝ".isMatch(re"(?-u)[^\w]"))
  doAssert(not "Ǝ".isMatch(re"(?-u)[\w]"))
  doAssert(not "\t".isMatch(re"(?-u)[\w]"))

  # tescaped_sequences
  doAssert("\x07".isMatch(re"\a"))
  doAssert("\x0C".isMatch(re"\f"))
  doAssert("\t".isMatch(re"\t"))
  doAssert("\L".isMatch(re"\n"))
  doAssert("\r".isMatch(re"\r"))
  doAssert("\x0B".isMatch(re"\v"))
  doAssert(not "a".isMatch(re"\a"))
  doAssert(".+*?()|[]{}^$".isMatch(re"\.\+\*\?\(\)\|\[\]\{\}\^\$"))

  doAssert("\x07".isMatch(re"[\a]"))
  doAssert("\x07".isMatch(re"[\a-\a]"))
  doAssert(not "0".isMatch(re"[\a-\a]"))
  #doAssert("|".isMatch(re"[a|b]"))  # ????

  # tfind
  doAssert("abcd".find(re"bc").isSome)
  doAssert(not "abcd".find(re"ac").isSome)
  doAssert("abcd".find(re"^abcd$").isSome)
  doAssert("2222".findWithCapt(re"(22)*") ==
    @[@["22", "22"]])
  doAssert("2222".find(re"(22)*").get().group(0) ==
    @[Slice[int](a: 0, b: 1), Slice[int](a: 2, b: 3)])

  # tcontains
  doAssert(re"bc" in "abcd")
  doAssert(re"bd" notin "abcd")
  doAssert(re"(23)+" in "2323")
  doAssert(re"(23)+" in "23232")
  doAssert(re"^(23)+$" notin "23232")
