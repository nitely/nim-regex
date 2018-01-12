##[
A library for parsing, compiling, and executing
regular expressions. The match time is linear
with respect to the length of the input and
the regular expression. So, it can handle
untrusted users input. Its syntax is similar to PCRE
but lacks a few features that can't be implemented
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

import unicodedb
import unicodeplus except isUpper, isLower

const
  # This is used as start
  # and end of string. It should
  # be invalid code, but while it
  # works it simplifies things a bit.
  # An alternative would be opt[Rune]
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
    outA, outB: int  # todo: int16
    isGreedy: bool
    # reGroupStart, reGroupEnd
    idx: int  # todo: rename?
    isCapturing: bool
    name: string
    flags: seq[Flag]
    # reRepRange
    min: int
    max: int
    # reSet, reNotSet
    cps: seq[Rune]  # todo: use set
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

proc initSetNodeCommon(kind: NodeKind): Node =
  ## base node
  doAssert(kind in {reSet, reNotSet})
  Node(
    kind: kind,
    cp: "¿".toRune,
    cps: @[],
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
      isCapturing = true
    ): Node =
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
  r.int in {
    'A'.ord .. 'Z'.ord,
    'a'.ord .. 'z'.ord,
    '0'.ord .. '9'.ord}

proc isWordBoundary(r: Rune, nxt: Rune): bool {.inline.} =
  ## check if current match
  ## form a boundary (i.e the end of a word)
  let
    isWord = r != invalidRune and r.isalnum()
    isNxtWord = nxt != invalidRune and nxt.isalnum()
  result = (
    (isWord and not isNxtWord) or
    (not isWord and isNxtWord))

proc isWordBoundaryAscii(r: Rune, nxt: Rune): bool {.inline.} =
  let
    isWord = r != invalidRune and r.isAlphaNumAscii()
    isNxtWord = nxt != invalidRune and nxt.isAlphaNumAscii()
  result = (
    (isWord and not isNxtWord) or
    (not isWord and isNxtWord))

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

# todo: can't use unicodeplus due to
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

proc `$`(n: Node): string =
  ## return the string representation
  ## of a `Node`. The string is always
  ## equivalent to the original
  ## expression but not necessarily equal
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
    for cp in n.cps:
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
  while true:
    inc sc.pos
    yield sc.s[sc.pos - 1]
    if sc.pos >= sc.s.len:
      break

proc prev[T](sc: Scanner[T]): T =
  sc.s[sc.pos - 1]

proc curr[T](sc: Scanner[T]): T =
  sc.s[sc.pos]

proc next[T](sc: Scanner[T]): T =
  ## return current item and consume it
  result = sc.s[sc.pos]
  inc sc.pos

proc peek[T](sc: Scanner[T], default: T): T =
  ## same as ``curr`` except it
  ## returns a default/invalid value when
  ## the data is fully consumed
  if sc.pos >= sc.s.high:
    return default
  result = sc.s[sc.pos]

proc peek(sc: Scanner[Rune]): Rune =
  sc.peek(invalidRune)

proc peek(sc: Scanner[Node]): Node =
  sc.peek(initEOENode())

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
  ## the given character must be a
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
  ## ``Node`` of ``reSet`` or ``reNotSet`` kind
  var n = case sc.curr
  of "^".toRune:
    discard sc.next()
    initNotSetNode()
  else:
    initSetNode()
  var isRange, isEscaped, hasEnd = false
  for cp, nxt in sc.peek:
    if cp == "]".toRune and
        not isEscaped and
        not n.isEmpty:
      hasEnd = true
      break
    if cp == "\\".toRune and not isEscaped:
      isEscaped = true
      continue
    if isRange:
      doAssert(n.cps.len > 0)
      var cp = cp
      if isEscaped:
        let nn = cp.toSetEscapedNode()
        doAssert(nn.kind == reChar,
          "invalid range in character class")
        cp = nn.cp
      isRange = false
      isEscaped = false
      let start = n.cps.pop()
      doAssert(start <= cp)
      n.ranges.add((
        rangeStart: start,
        rangeEnd: cp))
      if nxt == "-".toRune:
        n.cps.add(nxt)
        discard sc.next()
      continue
    if isEscaped:
      isEscaped = false
      let nn = cp.toSetEscapedNode()
      if nn.kind == reChar:
        n.cps.add(nn.cp)
        continue
      doAssert(nn.kind in shorthandKind)
      n.shorthands.add(nn)
      if nxt == "-".toRune:
        n.cps.add(nxt)
        discard sc.next()
      continue
    if cp == "-".toRune and n.cps.len > 0:
      isRange = true
      continue
    n.cps.add(cp)
  if isRange:
    n.cps.add("-".toRune)
  doAssert(not isEscaped)
  doAssert(not n.isEmpty)
  doAssert(hasEnd)
  result = @[n]

proc parseRepRange(sc: Scanner[Rune]): seq[Node] =
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
    doAssert(cp.int in '0'.ord .. '9'.ord)
    curr.add(char(cp.int))
  if first.isNil:  # {n}
    first = curr
  if first.len == 0:  # {,m} or {,}
    first.add('0')
  if last.len == 0:  # {n,} or {,}
    last = "-1"
  doAssert(first.len > 0)
  doAssert(last.len > 0)
  result = @[Node(
    kind: reRepRange,
    min: first.parseInt,
    max: last.parseInt)]

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
    if sc.curr == ")".toRune:
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
  else:
    @[r.toCharNode]

proc parse(expression: string): seq[Node] =
  ## convert a ``string`` regex expression
  ## into a ``Node`` expression
  result = newSeqOfCap[Node](expression.len)
  let sc = expression.toRunes.scan()
  var isEscaped = false
  for cp in sc:
    if isEscaped:
      isEscaped = false
      result.add(cp.toEscapedNode)
      continue
    if cp == "\\".toRune:
      isEscaped = true
      continue
    result.add(sc.subParse())

proc greediness(expression: seq[Node]): seq[Node] =
  ## apply greediness to an expression
  result = newSeqOfCap[Node](expression.len)
  let sc = expression.scan()
  for n in sc:
    var n = n
    if (n.kind in repetitionKind or
        n.kind == reZeroOrOne) and
        sc.peek.kind == reZeroOrOne:
      n.isGreedy = true
      discard sc.next
    result.add(n)

type
  NamedGroup = tuple
    name: string
    idx: int
  GroupsCapture = tuple
    count: int
    names: seq[NamedGroup]

proc fillGroups(expression: var seq[Node]): GroupsCapture =
  ## populate group indices, names and capturing mark
  var
    groups: seq[int] = @[]
    nonCapt = 0
    names: seq[NamedGroup] = @[]
    count = 0
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
        names.add((name: n.name, idx: n.idx))
    of reGroupEnd:
      let start = groups.pop()
      n.isCapturing = expression[start].isCapturing
      n.idx = expression[start].idx
      if not n.isCapturing:
        dec nonCapt
    else:
      discard
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
  for n in sc:
    result.add(n)
    for f in flags.squash():
      # todo: move to its own function
      case f
      of flagAnyMatchNewLine:
        if n.kind == reAny:
          result[^1].kind = reAnyNL
      of flagMultiLine:
        case n.kind
        of reStartSym:
          result[^1].kind = reStartSymML
        of reEndSym:
          result[^1].kind = reEndSymML
        else:
          discard
      of flagCaseInsensitive:
        # todo: support sets?
        if n.kind == reChar and n.cp != n.cp.swapCase():
          result[^1].kind = reCharCI
      of flagUnGreedy:
        if n.kind in opKind:
          result[^1].isGreedy = not n.isGreedy
      of flagNotUnicode:
        result[^1].kind = n.kind.toAsciiKind()
        # todo: remove, add reSetAscii and reNotSetAscii
        if result[^1].kind in {reSet, reNotSet}:
          for nn in result[^1].shorthands.mitems:
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
        continue
      if sc.peek.kind == reSkip:
        discard sc.next()  # SKIP
        doAssert(sc.peek.kind == reGroupEnd)
        discard sc.next()  # )
        if flags.len > 0:
          flags[^1].add(n.flags)
          continue
      flags.add(n.flags)
    of reGroupEnd:
      discard flags.pop()
    else:
      discard

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
        "either char or shorthand (i.e: \\w) expected before repetition range")

proc joinAtoms(expression: seq[Node]): seq[Node] =
  ## Put a `~` joiner between atoms
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
    asyRight,
    asyLeft
  OpsPA = tuple
    precedence: int
    associativity: Associativity

proc opsPA(nk: NodeKind): OpsPA =
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

# todo: keep track of org/target end instead of doing this recursion BS
proc combine(nfa: var seq[Node], org: int, target: int, visited: var seq[int]) =
  if org in visited:
    return
  visited.add(org)
  if nfa[org].outA != -1:
    if nfa[nfa[org].outA].kind == reEOE:
      nfa[org].outA = target
    else:
      nfa.combine(nfa[org].outA, target, visited)
  if nfa[org].outB != -1:
    if nfa[nfa[org].outB].kind == reEOE:
      nfa[org].outB = target
    else:
      nfa.combine(nfa[org].outB, target, visited)

proc combine(nfa: var seq[Node], org: int, target: int) =
  var visited: seq[int] = @[]
  nfa.combine(org, target, visited)

proc nfa(expression: seq[Node]): seq[Node] =
  ## A stack machine to convert a
  ## expression (in RPN) to an NFA
  result = newSeqOfCap[Node](expression.len)
  result.add(initEOENode())
  var states: seq[int] = @[]
  for n in expression:
    var n = n
    case n.kind
    of matchableKind, assertionKind:
      n.outA = 0
      n.outB = -1
      result.add(n)
      states.add(result.high)
    of reJoiner:
      let
        stateB = states.pop()
        stateA = states.pop()
      result.combine(stateA, stateB)
      states.add(stateA)
    of reOr:
      n.outB = states.pop()
      n.outA = states.pop()
      result.add(n)
      states.add(result.high)
    of reZeroOrMore:
      n.outA = states.pop()
      n.outB = 0
      result.combine(n.outA, result.high + 1)
      result.add(n)
      states.add(result.high)
      if n.isGreedy:
        swap(result[^1].outA, result[^1].outB)
    of reOneOrMore:
      n.outA = states.pop()
      n.outB = 0
      result.combine(n.outA, result.high + 1)
      result.add(n)
      states.add(n.outA)
      if n.isGreedy:
        swap(result[^1].outA, result[^1].outB)
    of reZeroOrOne:
      n.outA = states.pop()
      n.outB = 0
      result.add(n)
      states.add(result.high)
      if n.isGreedy:
        swap(result[^1].outA, result[^1].outB)
    of reGroupStart:
      n.outA = states.pop()
      n.outB = -1
      result.add(n)
      states.add(result.high)
    of reGroupEnd:
      n.outA = 0
      n.outB = -1
      let stateA = states.pop()
      result.combine(stateA, result.high + 1)
      result.add(n)
      states.add(stateA)  # ???
    else:
      doAssert(false, "Unhandled node: $#" % $n.kind)
  doAssert(states.len == 1)
  result.add(Node(
    kind: reSkip,
    cp: "¿".toRune,
    outA: states[0],
    outB: -1))
  # This limitation may be removed
  # in the future if someone asks for it
  doAssert(
    result.high <= int16.high,
    "regex is too long")

# todo: remove
proc stringify(nfa: seq[Node], n: Node, visited: var seq[Node]): string =
  ## AST to string representation
  if n in visited:
    return "[...]"
  visited.add(n)
  result = "["
  result.add($n)
  if n.outA != -1:
    result.add(", ")
    result.add(nfa.stringify(nfa[n.outA], visited))
  if n.outB != -1:
    result.add(", ")
    result.add(nfa.stringify(nfa[n.outB], visited))
  result.add("]")

proc stringify(nfa: seq[Node]): string =
  var visited: seq[Node] = @[]
  result = nfa.stringify(nfa[^1], visited)

type
  NFA = tuple  # todo: rename to Regex
    state: seq[Node]
    groupsCount: int
    namedGroups: seq[NamedGroup]  # todo: use table
  Match = object  # todo: rename to RegexMatch
    isMatch*: bool
    captures*: seq[Slice[int]]
    groups*: seq[Slice[int]] # todo: remove, merge with captures
    namedGroups*: seq[NamedGroup]  # todo: use table
  CaptureKind = enum
    captStart
    captEnd
  Capture = object
    kind*: CaptureKind
    prev*: int
    idx*: int
    cpIdx*: int
  State = tuple
    ni: int  # todo: int16
    c: int

proc group(m: Match, i: int): seq[Slice[int]] =
  ## return slices for a given group.
  ## Use the iterator version if you care about performance
  m.captures[m.groups[i]]

proc group(m: Match, s: string): seq[Slice[int]] =
  ## return slices for a given group.
  ## Use the iterator version if you care about performance
  for ng in m.namedGroups:
    if s == ng.name:
      return m.captures[m.groups[ng.idx]]

# todo: rename to groups
proc groups2(m: Match): seq[seq[Slice[int]]] =
  ## return slices for each group.
  ## Use the iterator version if you care about performance
  result = newSeq[seq[Slice[int]]](m.groups.len)
  var j = 0
  for i, g in m.groups:
    result[i] = newSeq[Slice[int]](m.groups[i].b - m.groups[i].a + 1)
    j = 0
    for idx in g:
      result[i][j] = m.captures[idx]
      inc j

proc `$`(n: NFA): string =
  ## AST to string representation
  n.state.stringify

proc populateCaptures(
      result: var Match,
      captured: seq[Capture],
      cIdx: int,
      gc: int
    ) =
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

type
  LeakySeq[T] = object
    ## a seq that can only grow
    s: seq[T]
    pos: int

proc initLeakySeq[T](size = 16): LeakySeq[T] =
  LeakySeq[T](s: newSeq[T](size), pos: 0)

proc `[]`[T](ls: LeakySeq[T], i: int): T =
  ls.s[i]

proc len[T](ls: LeakySeq[T]): int =
  ls.pos

proc clear[T](ls: var LeakySeq[T]) =
  ls.pos = 0

proc add[T](ls: var LeakySeq[T], x: T) =
  if ls.pos > ls.s.high:
    ls.s.setLen(ls.s.len * 2)
  ls.s[ls.pos] = x
  inc ls.pos

iterator items[T](ls: LeakySeq[T]): T =
  var i = 0
  while i < ls.pos:
    yield ls.s[i]
    inc i

type
  States = tuple
    states: LeakySeq[State]
    ids: set[int16]  # todo: use seq and make the algo O(regex) space?

proc initStates(): States =
  result = (
    states: initLeakySeq[State](),
    ids: {})

proc `[]`(ss: States, i: int): State =
  ss.states[i]

proc len(ss: States): int =
  ss.states.len

proc clear(ss: var States) =
  ss.states.clear()
  ss.ids = {}

proc add(ss: var States, s: State) =
  if s.ni.int16 in ss.ids:
    return
  ss.states.add(s)
  ss.ids.incl(s.ni.int16)

iterator items(ss: States): State =
  for s in ss.states.items:
    yield s

# todo: make it iterative
proc step(
      result: var States,
      nfa: NFA,
      nIdx: int,
      captured: var seq[Capture],
      cIdx: int,
      visited: var set[int16],
      cpIdx: int,
      cp: Rune,
      nxt: Rune
    ) =
  if nIdx.int16 in visited:
    return
  visited.incl(nIdx.int16)
  let n = nfa.state[nIdx]
  if n.kind == reEOE:
    result.add((ni: nIdx, c: cIdx))
    return
  if n.kind in matchableKind:
    result.add((ni: nIdx, c: cIdx))
    return
  if n.kind in assertionKind and
      not n.match(cp, nxt):
    return
  var cIdx = cIdx
  if n.kind in {reGroupStart, reGroupEnd} and
      n.isCapturing and
      not captured.isNil:
    case n.kind
    of reGroupStart:
      captured.add(Capture(
        kind: captStart,
        cpIdx: cpIdx,
        prev: cIdx,
        idx: n.idx))
    of reGroupEnd:
      captured.add(Capture(
        kind: captEnd,
        cpIdx: cpIdx - 1,
        prev: cIdx,
        idx: n.idx))
    else:
      doAssert(false)
    cIdx = captured.high
  if n.outA != -1:
    result.step(
      nfa, n.outA, captured, cIdx, visited, cpIdx, cp, nxt)
  if n.outB != -1:
    result.step(
      nfa, n.outB, captured, cIdx, visited, cpIdx, cp, nxt)

template stepFrom(
      result: var States,
      n: Node,
      nfa: NFA,
      captured: var seq[Capture],
      cIdx: int,
      visited: var set[int16],
      cpIdx: int,
      cp: Rune,
      nxt: Rune
    ) =
  ## go to next states
  if n.outA != -1:
    result.step(
      nfa, n.outA, captured, cIdx, visited, cpIdx, cp, nxt)
  if n.outB != -1:
    result.step(
      nfa, n.outB, captured, cIdx, visited, cpIdx, cp, nxt)

proc fullMatch*(s: string | seq[Rune], nfa: NFA): Match =
  var
    visited: set[int16] = {}
    captured: seq[Capture]  # todo: grow seq by X factor when needed
    currStates = initStates()
    nextStates = initStates()
  if nfa.groupsCount > 0:
    captured = @[Capture()]
  for i, cp, nxt in s.peek:
    if cp == invalidRune:
      assert currStates.len == 0
      assert i == 0
      currStates.step(
        nfa, nfa.state.high, captured, 0, visited, i, cp, nxt)
      continue
    if currStates.len == 0:
      break
    for ni, c in currStates.items:
      let n = nfa.state[ni]
      if not n.match(cp):
        continue
      visited = {}
      nextStates.stepFrom(
        n, nfa, captured, c, visited, i, cp, nxt)
    swap(currStates, nextStates)
    nextStates.clear()
  for ni, c in currStates.items:
    if nfa.state[ni].kind == reEOE:
      result.isMatch = true
      if nfa.groupsCount > 0:
        result.populateCaptures(captured, c, nfa.groupsCount)
        result.namedGroups = nfa.namedGroups
      break

proc contains*(s: string | seq[Rune], nfa: NFA): bool =
  var
    visited: set[int16] = {}
    currStates = initStates()
    nextStates = initStates()
  for i, cp, nxt in s.peek:
    visited = {}
    currStates.step(
      nfa, nfa.state.high, nil, 0, visited, i, cp, nxt)
    for ni, _ in currStates.items:
      let n = nfa.state[ni]
      if n.kind == reEOE:
        return true
      if not n.match(cp):
        continue
      visited = {}
      nextStates.stepFrom(
        n, nfa, nil, 0, visited, i, cp, nxt)
    swap(currStates, nextStates)
    nextStates.clear()
  for ni, _ in currStates.items:
    if nfa.state[ni].kind == reEOE:
      return true

proc search*(s: string | seq[Rune], nfa: NFA): Match =
  ## search through the string looking for the first
  ## location where there is a match
  var
    visited: set[int16] = {}
    captured: seq[Capture]
    currStates = initStates()
    nextStates = initStates()
  if nfa.groupsCount > 0:
    captured = @[Capture()]
  for i, cp, nxt in s.peek:
    visited = {}
    currStates.step(
      nfa, nfa.state.high, captured, 0, visited, i, cp, nxt)
    if currStates.len > 0 and
        nfa.state[currStates[0].ni].kind == reEOE:
      break
    if cp == invalidRune:
      continue
    for ni, c in currStates.items:
      let n = nfa.state[ni]
      if n.kind == reEOE:
        nextStates.add((ni: ni, c: c))
        continue
      if not n.match(cp):
        continue
      visited = {}
      nextStates.stepFrom(
        n, nfa, captured, c, visited, i, cp, nxt)
    swap(currStates, nextStates)
    nextStates.clear()
  for ni, c in currStates.items:
    if nfa.state[ni].kind == reEOE:
      result.isMatch = true
      if nfa.groupsCount > 0:
        result.populateCaptures(captured, c, nfa.groupsCount)
        result.namedGroups = nfa.namedGroups
      break

proc toPattern(s: string): NFA =
  var ns = s.parse.greediness
  let gc = ns.fillGroups()
  result = (
    state: ns.applyFlags.expandRepRange.joinAtoms.rpn.nfa,
    groupsCount: gc.count,
    namedGroups: gc.names)

template re*(s: string): NFA =
  ## Parse and compile a regular
  ## expression at compile-time
  const pattern = s.toPattern
  pattern

when isMainModule:
  proc isFullMatch(s: string, nfa: NFA): bool =
    s.fullMatch(nfa).isMatch

  proc toAtoms(s: string): string =
    s.parse.greediness.applyFlags.expandRepRange.joinAtoms.`$`

  proc toNfaStr(s: string): string =
    let n: NFA = (
      state: s.parse.greediness.applyFlags.expandRepRange.joinAtoms.rpn.nfa,
      groupsCount: 0,
      namedGroups: @[])
    result = $n

  proc fullMatchWithCapt(s: string, nfa: NFA): seq[seq[string]] =
    let m = s.fullMatch(nfa)
    result = @[]
    for g in m.groups:
      result.add(@[])
      for idx in g:
        let capt = m.captures[idx]
        if capt.b >= capt.a:
          result[^1].add(s[capt])

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
  doAssert(toAtoms(r"[aa\-zz]") == r"[aa-zz]")
  doAssert(toAtoms(r"[^a]") == r"[^a]")
  doAssert(toAtoms(r"(a*)*") != toAtoms(r"a*"))
  doAssert(toAtoms(r"(a*|b*)*") != toAtoms(r"(a|b)*"))
  doAssert(toAtoms(r"(a*b*)*") != toAtoms(r"(a|b)*"))
  doAssert(toAtoms(r"(a*|b*)") != toAtoms(r"(a|b)*"))

  # tfull_match
  #doAssert("".isFullMatch(re""))
  doAssert("a".isFullMatch(re"a"))
  doAssert("ab".isFullMatch(re"(a)b"))
  doAssert("aa".isFullMatch(re"(a)*"))
  doAssert("aab".isFullMatch(re"((a)*b)"))
  doAssert("abbbbccccd".isFullMatch(re"a(b|c)*d"))
  doAssert("abbb".isFullMatch(re"((a)*(b)*)"))
  doAssert("abbb".isFullMatch(re"((a(b)*)*(b)*)"))
  doAssert("a".isFullMatch(re"a|b"))
  doAssert("b".isFullMatch(re"a|b"))
  doAssert(not "ab".isFullMatch(re"a(b|c)*d"))
  doAssert(not "a".isFullMatch(re"b"))
  #doAssert(not "a".isFullMatch(re""))

  # trepetition_cycle
  doAssert("aaa".isFullMatch(re"a**"))
  doAssert("aaa".isFullMatch(re"(a*)*"))
  doAssert("aaabbbaaa".isFullMatch(re"((a*|b*))*"))
  doAssert("aaa".isFullMatch(re"a*****"))
  #doAssert("aaa".isFullMatch(re"a*{,}"))  # must raise
  doAssert("aaa".isFullMatch(re"(a?)*"))
  doAssert("aaaa".isFullMatch(re"((a)*(a)*)*"))

  # tcaptures
  doAssert("ab".fullMatchWithCapt(re"(a)b") == @[@["a"]])
  doAssert("aa".fullMatchWithCapt(re"(a)*") == @[@["a", "a"]])
  doAssert(
    "aab".fullMatchWithCapt(re"((a)*b)") ==
    @[@["aab"], @["a", "a"]])
  doAssert(
    "abbbbccccd".fullMatchWithCapt(re"a(b|c)*d") ==
    @[@["b", "b", "b", "b", "c", "c", "c", "c"]])
  doAssert(
    "abbb".fullMatchWithCapt(re"((a)*(b)*)") ==
    @[@["abbb"], @["a"], @["b", "b", "b"]])
  doAssert(
    "abbb".fullMatchWithCapt(re"((a(b)*)*(b)*)") ==
    @[@["abbb"], @["abbb"], @["b", "b", "b"], @[]])
  doAssert("aa".fullMatchWithCapt(re"(a)+") == @[@["a", "a"]])
  doAssert("abab".fullMatchWithCapt(re"(ab)+") == @[@["ab", "ab"]])
  doAssert("a".fullMatchWithCapt(re"(a)?") == @[@["a"]])
  doAssert("ab".fullMatchWithCapt(re"(ab)?") == @[@["ab"]])
  doAssert(
    "aaabbbaaa".fullMatchWithCapt(re"(a*|b*)*") ==
    @[@["aaa", "bbb", "aaa"]])
  doAssert(
    "abab".fullMatchWithCapt(re"(a(b))*") ==
    @[@["ab", "ab"], @["b", "b"]])
  # Following two should match the same
  doAssert(
    "aaanasdnasd".fullMatchWithCapt(re"((a)*n?(asd)*)*") ==
    @[@["aaanasd", "nasd"], @["a", "a", "a"], @["asd", "asd"]])
  doAssert(
    "aaanasdnasd".fullMatchWithCapt(re"((a)*n?(asd))*") ==
    @[@["aaanasd", "nasd"], @["a", "a", "a"], @["asd", "asd"]])
  doAssert(
    "b".fullMatchWithCapt(re"(a)?b") ==
    @[newSeq[string]()])
  doAssert(
    "ฅa".fullMatchWithCapt(re"(\w)(a)") ==
    @[@["ฅ"], @["a"]])

  # tone_or_more_op
  doAssert("aaaa".isFullMatch(re"a+"))
  doAssert("abb".isFullMatch(re"ab+"))
  doAssert("abaa".isFullMatch(re"aba+"))
  doAssert(not "".isFullMatch(re"a+"))
  doAssert(not "b".isFullMatch(re"a+"))
  doAssert(not "aab".isFullMatch(re"b+"))

  # tzero_or_one_op
  doAssert("a".isFullMatch(re"a?"))
  doAssert("".isFullMatch(re"a?"))
  doAssert("a".isFullMatch(re"ab?"))
  doAssert("ab".isFullMatch(re"ab?"))
  doAssert("aba".isFullMatch(re"ab?a"))
  doAssert("aa".isFullMatch(re"ab?a"))
  doAssert(not "aa".isFullMatch(re"a?"))
  doAssert(not "b".isFullMatch(re"a?"))
  doAssert(not "abb".isFullMatch(re"ab?"))

  # tescape
  doAssert("(a)".isFullMatch(re"\(a\)"))
  doAssert("a*b".isFullMatch(re"a\*b"))
  doAssert("a*bbb".isFullMatch(re"a\*b*"))
  doAssert("y".isFullMatch(re"\y"))
  doAssert("\\".isFullMatch(re"\\"))
  doAssert("\\\\".isFullMatch(re"\\\\"))

  # talphanum_shorthand
  doAssert("a".isFullMatch(re"\w"))
  doAssert("abc123".isFullMatch(re"\w*"))
  doAssert("a".fullMatchWithCapt(re"(\w)") == @[@["a"]])

  # tdigit
  doAssert("1".isFullMatch(re"\d"))
  doAssert("123".isFullMatch(re"\d*"))
  doAssert("۲".isFullMatch(re"\d"))  # Kharosthi numeral
  doAssert("⅕".isFullMatch(re"\d"))

  # twhite_space_shorthand
  doAssert(" ".isFullMatch(re"\s"))
  doAssert("   ".isFullMatch(re"\s*"))
  doAssert(" \t\r\f\v".isFullMatch(re"\s*"))
  doAssert("\u20".isFullMatch(re"\s"))  # New Line
  doAssert("\u2028".isFullMatch(re"\s"))  # Line separator

  # talphanum_not_shorthand
  doAssert(not "a".isFullMatch(re"\W"))
  doAssert(not "abc123".isFullMatch(re"\W*"))
  doAssert("!@#".isFullMatch(re"\W+"))

  # tnot_digit
  doAssert(not "1".isFullMatch(re"\D"))
  doAssert(not "123".isFullMatch(re"\D*"))
  doAssert(not "۲".isFullMatch(re"\D"))  # Kharosthi numeral
  doAssert(not "⅕".isFullMatch(re"\D"))
  doAssert("!@#".isFullMatch(re"\D+"))
  doAssert("a".isFullMatch(re"\D"))

  # tnot_white_space_shorthand
  doAssert("asd123!@#".isFullMatch(re"\S*"))
  doAssert(not " ".isFullMatch(re"\S"))
  doAssert(not "   ".isFullMatch(re"\S*"))
  doAssert(not "\t".isFullMatch(re"\S"))
  doAssert(not "\u20".isFullMatch(re"\S"))
  doAssert(not "\r".isFullMatch(re"\S"))
  doAssert(not "\f".isFullMatch(re"\S"))
  doAssert(not "\v".isFullMatch(re"\S"))
  doAssert(not "\u2028".isFullMatch(re"\S"))  # Line separator

  # tset
  doAssert("a".isFullMatch(re"[a]"))
  doAssert("a".isFullMatch(re"[abc]"))
  doAssert("b".isFullMatch(re"[abc]"))
  doAssert("c".isFullMatch(re"[abc]"))
  doAssert(not "d".isFullMatch(re"[abc]"))
  doAssert("a".isFullMatch(re"[\w]"))
  doAssert("1".isFullMatch(re"[\w]"))
  doAssert("1".isFullMatch(re"[\d]"))
  doAssert("*".isFullMatch(re"[*]"))
  doAssert("*".isFullMatch(re"[\*]"))
  doAssert("*".isFullMatch(re"[a*]"))
  doAssert("a".isFullMatch(re"[a*]"))
  doAssert("a".isFullMatch(re"[a-z]"))
  doAssert("f".isFullMatch(re"[a-z]"))
  doAssert("z".isFullMatch(re"[a-z]"))
  doAssert(not "A".isFullMatch(re"[a-z]"))
  doAssert("0".isFullMatch(re"[0-9]"))
  doAssert("5".isFullMatch(re"[0-9]"))
  doAssert("9".isFullMatch(re"[0-9]"))
  doAssert(not "a".isFullMatch(re"[0-9]"))
  doAssert("(".isFullMatch(re"[()[\]{}]"))
  doAssert(")".isFullMatch(re"[()[\]{}]"))
  doAssert("}".isFullMatch(re"[()[\]{}]"))
  doAssert("{".isFullMatch(re"[()[\]{}]"))
  doAssert("[".isFullMatch(re"[()[\]{}]"))
  doAssert("]".isFullMatch(re"[()[\]{}]"))
  doAssert("(".isFullMatch(re"[]()[{}]"))
  doAssert(")".isFullMatch(re"[]()[{}]"))
  doAssert("}".isFullMatch(re"[]()[{}]"))
  doAssert("{".isFullMatch(re"[]()[{}]"))
  doAssert("[".isFullMatch(re"[]()[{}]"))
  doAssert("]".isFullMatch(re"[]()[{}]"))
  doAssert("\\".isFullMatch(re"[\\]"))
  doAssert("\\".isFullMatch(re"[\\\]]"))
  doAssert("]".isFullMatch(re"[\\\]]"))
  doAssert("00".isFullMatch(re"[0-5][0-9]"))
  doAssert("59".isFullMatch(re"[0-5][0-9]"))
  doAssert(not "95".isFullMatch(re"[0-5][0-9]"))
  doAssert("1".isFullMatch(re"[0-57-9]"))
  doAssert("8".isFullMatch(re"[0-57-9]"))
  doAssert(not "6".isFullMatch(re"[0-57-9]"))
  doAssert("4".isFullMatch(re"[0-9A-Fa-f]"))
  doAssert("b".isFullMatch(re"[0-9A-Fa-f]"))
  doAssert("B".isFullMatch(re"[0-9A-Fa-f]"))
  doAssert(not "-".isFullMatch(re"[0-9A-Fa-f]"))
  doAssert("-".isFullMatch(re"[a\-z]"))
  doAssert("a".isFullMatch(re"[a\-z]"))
  doAssert("z".isFullMatch(re"[a\-z]"))
  doAssert(not "b".isFullMatch(re"[a\-z]"))
  doAssert("a".isFullMatch(re"[a-]"))
  doAssert("-".isFullMatch(re"[a-]"))
  doAssert("+".isFullMatch(re"[(+*)]"))
  doAssert("*".isFullMatch(re"[(+*)]"))
  doAssert("(".isFullMatch(re"[(+*)]"))
  doAssert("[".isFullMatch(re"[[-\]]"))
  doAssert("]".isFullMatch(re"[[-\]]"))
  doAssert(not "-".isFullMatch(re"[[-\]]"))
  doAssert("(".isFullMatch(re"[(-\)]"))
  doAssert(")".isFullMatch(re"[(-\)]"))
  doAssert(not "-".isFullMatch(re"[(-\)]"))
  doAssert("\\".isFullMatch(re"[\\-\\)]"))
  doAssert(not "-".isFullMatch(re"[\\-\\)]"))
  doAssert("-".isFullMatch(re"[-]"))
  doAssert("-".isFullMatch(re"[\-]"))
  doAssert("-".isFullMatch(re"[\-\-]"))
  doAssert("-".isFullMatch(re"[\--]"))
  doAssert("-".isFullMatch(re"[\--\-]"))
  doAssert("-".isFullMatch(re"[\---]"))
  doAssert("b".isFullMatch(re"[\--\-a-z]"))
  doAssert("b".isFullMatch(re"[\---a-z]"))
  doAssert("b".isFullMatch(re"[-a-z]"))
  doAssert("-".isFullMatch(re"[-a-z]"))
  doAssert("a".isFullMatch(re"[-a]"))
  doAssert("-".isFullMatch(re"[-a]"))
  doAssert("b".isFullMatch(re"[a-d-z]"))
  doAssert("-".isFullMatch(re"[a-d-z]"))
  doAssert("z".isFullMatch(re"[a-d-z]"))
  doAssert(not "e".isFullMatch(re"[a-d-z]"))
  doAssert("]".isFullMatch(re"[]]"))
  doAssert("]".isFullMatch(re"[\]]"))
  doAssert(not "[".isFullMatch(re"[]]"))
  doAssert(not "]]".isFullMatch(re"[]]"))
  doAssert(not "-".isFullMatch(re"[[-\]]"))
  doAssert(not "b".isFullMatch(re"[c-d]"))
  doAssert("-".isFullMatch(re"[a\w-\wz]"))
  doAssert("-".isFullMatch(re"[\w-a]"))
  doAssert("-".isFullMatch(re"[\w-]"))
  doAssert("a".isFullMatch(re"[\w-a]"))
  doAssert("1".isFullMatch(re"[\w-a]"))
  doAssert("-".isFullMatch(re"[db-c-f]"))
  doAssert(not "e".isFullMatch(re"[db-c-f]"))
  doAssert(not "-".isFullMatch(re"[=-_]"))
  doAssert("A".isFullMatch(re"[\A]"))
  doAssert("b".isFullMatch(re"[\b]"))
  doAssert("zz".isFullMatch(re"[\z][\z]"))
  doAssert(not "z".isFullMatch(re"[\z][\z]"))
  # range out of order in character class (must raise)
  # doAssert("b".isFullMatch(re"[d-c]"))
  # invalid range in character class (must raise)
  # doAssert("-".isFullMatch(re"[a-\w]"))

  # tnot_set
  doAssert("a".fullMatchWithCapt(re"([^b])") == @[@["a"]])
  doAssert("asd".fullMatchWithCapt(re"([^b]*)") == @[@["asd"]])
  doAssert("ab".fullMatchWithCapt(re"([^b]*b)") == @[@["ab"]])
  doAssert(
    "asd123".fullMatchWithCapt(re"([^\d]*)(\d*)") ==
    @[@["asd"], @["123"]])
  doAssert(
    "asd123".fullMatchWithCapt(re"([asd]*)([^asd]*)") ==
    @[@["asd"], @["123"]])
  doAssert(
    "<asd123!@#>".fullMatchWithCapt(re"(<[^>]*>)") ==
    @[@["<asd123!@#>"]])
  doAssert(not "a".isFullMatch(re"[^a]"))
  #doAssert("^".isFullMatch(re"[^]"))  # should raise missing end
  doAssert("^".isFullMatch(re"[\^]"))
  doAssert("a".isFullMatch(re"[\^a]"))
  doAssert(not "^".isFullMatch(re"[^^]"))
  doAssert("a".isFullMatch(re"[^^]"))
  doAssert("a".isFullMatch(re"[^-]"))
  doAssert(not "-".isFullMatch(re"[^-]"))

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
  doAssert(not "".isFullMatch(re"a{0}"))
  doAssert(not "".isFullMatch(re"a{0,0}"))
  doAssert(not "".isFullMatch(re"a{,0}"))
  doAssert("".isFullMatch(re"a{,2}"))
  doAssert("a".isFullMatch(re"a{0}"))
  #doAssert("".isFullMatch(re"")) #?
  doAssert("a".isFullMatch(re"a{0,0}"))
  doAssert("a".isFullMatch(re"a{,0}"))
  doAssert("a".isFullMatch(re"a{1}"))
  doAssert("aa".isFullMatch(re"a{2}"))
  doAssert("aaa".isFullMatch(re"a{3}"))
  doAssert(not "aaaa".isFullMatch(re"a{3}"))
  doAssert(not "".isFullMatch(re"a{1}"))
  doAssert("a".isFullMatch(re"a{1,1}"))
  doAssert("a".isFullMatch(re"a{1,2}"))
  doAssert("aa".isFullMatch(re"a{1,2}"))
  doAssert(not "aaa".isFullMatch(re"a{1,2}"))
  doAssert(not "a".isFullMatch(re"a{2,4}"))
  doAssert("a".isFullMatch(re"a{1,}"))
  doAssert("aa".isFullMatch(re"a{1,}"))
  doAssert("aaa".isFullMatch(re"a{1,}"))
  doAssert("aaaaaaaaaa".isFullMatch(re"a{1,}"))
  doAssert("aa".isFullMatch(re"a{2,}"))
  doAssert("a".isFullMatch(re"a{,}"))
  doAssert("aa".isFullMatch(re"a{,}"))
  doAssert("aaaaaaaaaa".isFullMatch(re"a{,}"))
  doAssert("".isFullMatch(re"a{,}"))
  doAssert("aaaaaaaaaa".isFullMatch(re"a{0,}"))
  doAssert("".isFullMatch(re"a{0,}"))
  doAssert(not "a".isFullMatch(re"a{2,}"))
  #doAssert("aaa".isFullMatch(re"a*{,}"))  # must raise
  #doAssert("aaa".isFullMatch(re"a*{0}"))  # must raise
  #doAssert("aaa".isFullMatch(re"a*{1}"))  # must raise
  doAssert(
    "aaa".fullMatchWithCapt(re"(a){,}") ==
    @[@["a", "a", "a"]])
  doAssert("aaa".fullMatchWithCapt(re"(a{,}){,}") == @[@["aaa"]])
  doAssert(
    "aaaaa".fullMatchWithCapt(re"(a){5}") ==
    @[@["a", "a", "a", "a", "a"]])
  doAssert("a".fullMatchWithCapt(re"(a){1,5}") == @[@["a"]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a){1,5}") ==
    @[@["a", "a", "a"]])
  doAssert(
    "".fullMatchWithCapt(re"(a){,}") ==
    @[newSeq[string]()])
  doAssert("aaa".fullMatchWithCapt(re"(a{,}){,}") == @[@["aaa"]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a{1}){,}") ==
    @[@["a", "a", "a"]])
  doAssert(
    "aaaa".fullMatchWithCapt(re"(a{2}){,}") ==
    @[@["aa", "aa"]])
  doAssert(
    "aaaa".fullMatchWithCapt(re"(a{,3}){,}") ==
    @[@["aaa", "a"]])
  doAssert(
    "".fullMatchWithCapt(re"(a{,3}){,}") ==
    @[newSeq[string]()])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a{1,}){,}") ==
    @[@["aaa"]])
  doAssert(
    "".fullMatchWithCapt(re"(a{1,}){,}") ==
    @[newSeq[string]()])
  doAssert(not "".isFullMatch(re"(a{1,})"))
  doAssert("a".fullMatchWithCapt(re"(a{1,})") == @[@["a"]])
  doAssert("aaa".fullMatchWithCapt(re"(a{1,})") == @[@["aaa"]])

  #tnon_capturing_groups
  doAssert("a".fullMatchWithCapt(re"(?:a)") == @[])
  doAssert("aaa".fullMatchWithCapt(re"(?:aaa)") == @[])
  doAssert(
    "abab".fullMatchWithCapt(re"(a(b))*") ==
    @[@["ab", "ab"], @["b", "b"]])
  doAssert(
    "abab".fullMatchWithCapt(re"(?:a(b))*") ==
    @[@["b", "b"]])
  doAssert(
    "abab".fullMatchWithCapt(re"(a(?:b))*") ==
    @[@["ab", "ab"]])
  doAssert(")".fullMatchWithCapt(re"(\))") == @[@[")"]])

  #tgreediness
  doAssert(
    "a".fullMatchWithCapt(re"(a)??") ==
    @[@["a"]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a)*(a)*(a)*") ==
    @[@["a", "a", "a"], @[], @[]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a)*?(a)*(a)*?") ==
    @[@[], @["a", "a", "a"], @[]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a)*?(a)*?(a)*") ==
    @[@[], @[], @["a", "a", "a"]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a)*?(a)*?(a)*?") ==
    @[@[], @[], @["a", "a", "a"]])
  doAssert(
    "aaaa".fullMatchWithCapt(re"(a)*?(a)*?(a)*?") ==
    @[@[], @[], @["a", "a", "a", "a"]])
  doAssert(
    "aa".fullMatchWithCapt(re"(a)?(aa?)") ==
    @[@["a"], @["a"]])
  doAssert(
    "aa".fullMatchWithCapt(re"(a)??(a)") ==
    @[@["a"], @["a"]])
  doAssert(
    "aa".fullMatchWithCapt(re"(a)??(aa?)") ==
    @[@[], @["aa"]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a)+(a)+(a)?") ==
    @[@["a", "a"], @["a"], @[]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a)+?(a)+(a)?") ==
    @[@["a"], @["a", "a"], @[]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a)+?(a)+?(a)?") ==
    @[@["a"], @["a"], @["a"]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a){,}(a){,}(a){,}") ==
    @[@["a", "a", "a"], @[], @[]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a){,}?(a){,}(a){,}?") ==
    @[@[], @["a", "a", "a"], @[]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a){,}?(a){,}?(a){,}") ==
    @[@[], @[], @["a", "a", "a"]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a){,}?(a){,}?(a){,}?") ==
    @[@[], @[], @["a", "a", "a"]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a){1,}(a){1,}(a)?") ==
    @[@["a", "a"], @["a"], @[]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a){1,}?(a){1,}(a)?") ==
    @[@["a"], @["a", "a"], @[]])
  doAssert(
    "aaa".fullMatchWithCapt(re"(a){1,}?(a){1,}?(a)?") ==
    @[@["a"], @["a"], @["a"]])
  doAssert(
    "aaaa".fullMatch(re"(a*?)(a*?)(a*)").groups2() ==
    @[@[0 .. -1], @[0 .. -1], @[0 .. 3]])  # note the empty slices
  doAssert(
    "aaaa".fullMatch(re"(a*)(a*?)(a*?)").groups2() ==
    @[@[0 .. 3], @[4 .. 3], @[4 .. 3]])  # note the empty slices

  # tassertions
  doAssert(
    "bbaa aa".fullMatchWithCapt(re"([\w ]*?)(\baa\b)") ==
    @[@["bbaa "], @["aa"]])
  doAssert(
    "aa bbaa".fullMatchWithCapt(re"(\baa\b)([\w ]*)") ==
    @[@["aa"], @[" bbaa"]])
  doAssert(
    "This island is great".fullMatchWithCapt(
      re"([\w ]*?)(\bis\b)([\w ]*?)") ==
    @[@["This island "], @["is"], @[" great"]])
  doAssert(
    "bbaabb".fullMatchWithCapt(re"([\w ]*?)(\Baa\B)([\w ]*?)") ==
    @[@["bb"], @["aa"], @["bb"]])
  doAssert(
    "This is my sister".fullMatchWithCapt(
      re"([\w ]*?)(\Bis\B)([\w ]*?)") ==
    @[@["This is my s"], @["is"], @["ter"]])
  doAssert("aa".isFullMatch(re"\b\b\baa\b\b\b"))
  doAssert("bb".isFullMatch(re"^^^^bb$$$$"))
  doAssert("bb".isFullMatch(re"\A\A\A\Abb\z\z\z\z"))

  # tdot_any_matcher
  doAssert("a".isFullMatch(re"."))
  doAssert("asd123!@#".isFullMatch(re".*"))
  doAssert("| (•□•) | (❍ᴥ❍ʋ)".isFullMatch(re".*"))
  doAssert(
    "ฅ^•ﻌ•^ฅ".fullMatchWithCapt(re"(.*)") ==
    @[@["ฅ^•ﻌ•^ฅ"]])
  doAssert("\t".isFullMatch(re"."))
  doAssert(not "\L".isFullMatch(re".*"))

  # tgroup
  doAssert("foobar".fullMatch(re"(\w*)").group(0) == @[0..5])
  doAssert(
    "foobar".fullMatch(re"(?P<foo>\w*)").group(0) == @[0..5])
  doAssert(
    "ab".fullMatch(re"(a)(b)").group(0) == @[0..0])
  doAssert(
    "ab".fullMatch(re"(a)(b)").group(1) == @[1..1])
  doAssert(
    "ab".fullMatch(re"(a)(b)").groups2() ==
    @[@[0..0], @[1..1]])

  # tnamed_groups
  doAssert(
    "foobar".fullMatch(re"(?P<foo>\w*)").group("foo") ==
    @[0..5])
  doAssert(
    "foobar".fullMatch(re"(?P<foo>(?P<bar>\w*))").group("foo") ==
    @[0..5])
  doAssert(
    "foobar".fullMatch(re"(?P<foo>(?P<bar>\w*))").group("bar") ==
    @[0..5])
  doAssert(
    "aab".fullMatch(re"(?P<foo>(?P<bar>a)*b)").group("foo") ==
    @[0..2])
  doAssert(
    "aab".fullMatch(re"(?P<foo>(?P<bar>a)*b)").group("bar") ==
    @[0..0, 1..1])
  doAssert(
    "aab".fullMatch(re"((?P<bar>a)*b)").group("bar") ==
    @[0..0, 1..1])

  #tflags
  doAssert("foo\Lbar".isFullMatch(re"(?s).*"))
  doAssert("foo\Lbar".isFullMatch(re"(?s:.*)"))
  doAssert("foo\Lbar".isFullMatch(re"(?ssss).*"))
  doAssert(not "foo\Lbar".isFullMatch(re"(?s-s).*"))
  doAssert(not "foo\Lbar".isFullMatch(re"(?-s-s-s).*"))
  doAssert(not "foo\Lbar".isFullMatch(re"(?-ss).*"))
  doAssert(not "foo\Lbar".isFullMatch(re"(?-ss-ss).*"))
  doAssert(not "foo\Lbar".isFullMatch(re"(?-sssss-s).*"))
  doAssert(not "foo\Lbar".isFullMatch(re"(?s-s:.*)"))
  doAssert(not "foo\Lbar".isFullMatch(re"(?------s----s:.*)"))
  doAssert(
    "foo\Lbar".fullMatchWithCapt(re"((?s:.*))") ==
    @[@["foo\Lbar"]])
  doAssert("a".fullMatchWithCapt(re"((?i:a))") == @[@["a"]])
  doAssert("A".fullMatchWithCapt(re"((?i:a))") == @[@["A"]])
  doAssert(
    "ABC".fullMatchWithCapt(re"((?i:aBc))") ==
    @[@["ABC"]])
  doAssert("a".fullMatchWithCapt(re"((?-i:a))") == @[@["a"]])
  doAssert(not "A".isFullMatch(re"((?-i:a))"))
  doAssert(not "A".isFullMatch(re"((?-ii-i:a))"))
  doAssert("a".fullMatchWithCapt(re"((?i)a)") == @[@["a"]])
  doAssert("A".fullMatchWithCapt(re"((?i)a)") == @[@["A"]])
  doAssert("a".fullMatchWithCapt(re"((?-i)a)") == @[@["a"]])
  doAssert(not "A".isFullMatch(re"((?-i)a)"))
  doAssert("AaA".isFullMatch(re"(?i)a+"))
  doAssert("AaA".isFullMatch(re"(?i)A+"))
  doAssert("AbC".isFullMatch(re"(?i)abc"))
  doAssert(not "b".isFullMatch(re"(?i)a"))
  doAssert("A".isFullMatch(re"(?-i)(?i)a"))
  doAssert(not "A".isFullMatch(re"(?i)(?-i)a"))
  doAssert(
    "AaA".fullMatchWithCapt(re"((?i)a+)") ==
    @[@["AaA"]])

  doAssert(
    "aa".fullMatchWithCapt(re"((?U)a*)(a*)") ==
    @[@[], @["aa"]])
  doAssert(
    "aa".fullMatchWithCapt(re"((?U)a*?)(a*)") ==
    @[@["aa"], @[]])
  doAssert(
    "aa".fullMatchWithCapt(re"((?U-U)a*)(a*)") ==
    @[@["aa"], @[]])
  doAssert(
    "aa".fullMatchWithCapt(re"((?U:a*))(a*)") ==
    @[@[], @["aa"]])
  doAssert(
    "aa".fullMatchWithCapt(re"((?U:a*?))(a*)") ==
    @[@["aa"], @[]])
  doAssert(
    "aa".fullMatchWithCapt(re"((?U-U:a*))(a*)") ==
    @[@["aa"], @[]])

  doAssert(not "a\Lb\L".isFullMatch(re"(?sm)a.b(?-sm:.)"))
  doAssert("a\Lb\L".isFullMatch(re"(?ms)a.b(?s-m:.)"))
  doAssert("a\L".isFullMatch(re"(?s)a."))
  doAssert(not "a\L\L".isFullMatch(re"(?s)a.$."))
  doAssert("a\L\L".isFullMatch(re"(?sm)a.$."))
  doAssert(not "a\L\L".isFullMatch(re"(?-sm)a.$."))
  doAssert(not "a\L\L".isFullMatch(re"(?s-m)a.$."))
  doAssert("a\L\L".isFullMatch(re"(?s-m)(?m:a.$.)"))
  doAssert(not "a\L\L".isFullMatch(re"(?i-sm)(?s:a.$.)"))
  doAssert("a\L\L".isFullMatch(re"(?i-sm)(?sm:a.$.)"))
  doAssert(not "a\L\L".isFullMatch(re"(?-sm)(?sm)(?-sm:a.$.)"))
  doAssert("a\L\L".isFullMatch(re"(?sm)(?-sm)(?sm:a.$.)"))
  doAssert(not "a\L\L".isFullMatch(re"(?-sm)(?sm:(?-sm:a.$.))"))
  doAssert("a\L\L".isFullMatch(re"(?sm)(?-sm:(?sm:a.$.))"))

  doAssert("Ǝ".isFullMatch(re"\w"))
  doAssert("Ǝ".isFullMatch(re"(?u)\w"))
  doAssert(not "Ǝ".isFullMatch(re"(?-u)\w"))
  doAssert("abczABCZ0129".isFullMatch(re"(?-u)\w*"))
  doAssert(not "\t".isFullMatch(re"(?-u)\w"))
  # todo: test every ascii kind
  doAssert("Ǝ".isFullMatch(re"(?u)[\w]"))
  doAssert(not "Ǝ".isFullMatch(re"(?u)[^\w]"))
  doAssert("Ǝ".isFullMatch(re"(?-u)[^\w]"))
  doAssert(not "Ǝ".isFullMatch(re"(?-u)[\w]"))
  doAssert(not "\t".isFullMatch(re"(?-u)[\w]"))

  # tescaped_sequences
  doAssert("\x07".isFullMatch(re"\a"))
  doAssert("\x0C".isFullMatch(re"\f"))
  doAssert("\t".isFullMatch(re"\t"))
  doAssert("\L".isFullMatch(re"\n"))
  doAssert("\r".isFullMatch(re"\r"))
  doAssert("\x0B".isFullMatch(re"\v"))
  doAssert(not "a".isFullMatch(re"\a"))
  doAssert(".+*?()|[]{}^$".isFullMatch(re"\.\+\*\?\(\)\|\[\]\{\}\^\$"))

  doAssert("\x07".isFullMatch(re"[\a]"))
  doAssert("\x07".isFullMatch(re"[\a-\a]"))
  doAssert(not "0".isFullMatch(re"[\a-\a]"))

  # tsearch
  doAssert("abcd".search(re"bc").isMatch)
  doAssert("abcd".search(re"^abcd$").isMatch)
