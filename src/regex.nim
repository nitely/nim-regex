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
  \u007F     hex character code (exactly four digits)
  \U0010FFFF hex character code (exactly eight digits)

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
import parseutils

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

proc `%%`(formatstr: string, a: openArray[string]): string
    {.noSideEffect, raises: [].} =
  ## same as ``"$#" % ["foo"]`` but
  ## returns empty string on error
  try:
    formatstr % a
  except ValueError:
    ""

proc `%%`(formatstr: string, a: string): string =
  formatstr %% [a]

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
    flagNotUnicode,  # -u
    flagVerbose,  # x
    flagNotVerbose  # -x
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
    reUCC,  # \pN or \p{Nn}
    reNotAlphaNum,  # \W
    reNotDigit,  # \D
    reNotWhiteSpace,  # \S
    reNotUCC,  # \PN or \P{Nn}
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
    outA, outB: int16
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
    ranges: seq[Slice[Rune]]  # todo: interval tree
    shorthands: seq[Node]
    # reUCC, reNotUCC
    cc: string

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

template initSetNodeImpl(k: NodeKind): Node =
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
  initSetNodeImpl(reSet)

proc initNotSetNode(): Node =
  ## return a negated set ``Node``,
  ## parsed from an expression such as ``[^a-z]``
  initSetNodeImpl(reNotSet)

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

proc isAlphaNum(r: Rune): bool {.inline.} =
  utmWord in unicodeTypes(r)

proc isAlphaNumAscii(r: Rune): bool {.inline.} =
  ## return ``true`` if the given
  ## rune is in ``[A-Za-z0-9]`` range
  case r.int
  of 'A'.ord .. 'Z'.ord,
      'a'.ord .. 'z'.ord,
      '0'.ord .. '9'.ord:
    true
  else:
    false

template isWordBoundaryImpl(r, nxt, alnumProc): bool =
  let
    isWord = r != invalidRune and alnumProc(r)
    isNxtWord = nxt != invalidRune and alnumProc(nxt)
  ((isWord and not isNxtWord) or
   (not isWord and isNxtWord))

proc isWordBoundary(r: Rune, nxt: Rune): bool {.inline.} =
  ## check if current match
  ## is a boundary (i.e the end of a word)
  isWordBoundaryImpl(r, nxt, isAlphaNum)

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
    assert false
    false

proc `<=`(x, y: Rune): bool =
  x.int <= y.int

proc contains(sr: seq[Slice[Rune]], r: Rune): bool =
  result = false
  for sl in sr:
    result = r in sl
    if result:
      break

proc isWhiteSpace(r: Rune): bool {.inline.} =
  utmWhiteSpace in unicodeTypes(r)

proc isWhiteSpaceAscii(r: Rune): bool {.inline.} =
  case r.int
  of ' '.ord,
      '\t'.ord,
      '\L'.ord,
      '\r'.ord,
      '\f'.ord,
      '\v'.ord:
    true
  else:
    false

proc isDigitAscii(r: Rune): bool {.inline.} =
  case r.int
  of '0'.ord .. '9'.ord:
    true
  else:
    false

proc isAnyAscii(r: Rune): bool {.inline.} =
  (r.int <= int8.high and
   r != lineBreakRune)

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

proc cmpCharClass(r: Rune, cc: string): bool =
  if cc.len == 1:
    cc[0] == r.category()[0]
  else:
    cc == r.category()

proc match(n: Node, r: Rune): bool =
  ## match for ``Node`` of matchable kind.
  ## Return whether the node matches
  ## the current character or not
  assert r != invalidRune
  case n.kind
  of reEOE:
    false
  of reAlphaNum:
    r.isAlphaNum()
  of reNotAlphaNum:
    not r.isAlphaNum()
  of reDigit:
    r.isDecimal()
  of reNotDigit:
    not r.isDecimal()
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
        if matches: break
    ((matches and n.kind == reSet) or
     (not matches and n.kind == reNotSet))
  of reAny:
    r != lineBreakRune
  of reAnyNL:
    true
  of reCharCI:
    r == n.cp or r == n.cp.swapCase()
  of reAlphaNumAscii:
    r.isAlphaNumAscii()
  of reDigitAscii:
    r.isDigitAscii()
  of reWhiteSpaceAscii:
    r.isWhiteSpaceAscii()
  of reUCC:
    cmpCharClass(r, n.cc)
  of reNotAlphaNumAscii:
    not r.isAlphaNumAscii()
  of reNotDigitAscii:
    not r.isDigitAscii()
  of reNotWhiteSpaceAscii:
    not r.isWhiteSpaceAscii()
  of reNotUCC:
    not cmpCharClass(r, n.cc)
  of reAnyAscii:
    r.isAnyAscii()
  of reAnyNLAscii:
    r.isAnyAscii() or r == lineBreakRune
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
    reUCC,
    reNotAlphaNum,
    reNotDigit,
    reNotWhiteSpace,
    reNotUCC,
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
    reUCC,
    reNotAlphaNum,
    reNotDigit,
    reNotWhiteSpace,
    reNotUCC,
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

proc `$`(n: seq[Node]): string =
  result = newStringOfCap(n.len)
  for nn in n:
    result.add($nn)

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
  assert i < ls.pos
  ls.s[i]

proc `[]`[T](ls: var ElasticSeq[T], i: int): var T =
  assert i < ls.pos
  ls.s[i]

proc `[]=`[T](ls: var ElasticSeq[T], i: int, x: T) =
  assert i < ls.pos
  ls.s[i] = x

#[
# todo: fixme supported in nim >= 0.18
proc `[]`[T](ls: ElasticSeq[T], i: BackwardsIndex): T =
  `[]`(ls, ls.len - int(i))

proc `[]`[T](ls: var ElasticSeq[T], i: BackwardsIndex): T =
  `[]`(ls, ls.len - int(i))

proc `[]=`[T](ls: var ElasticSeq[T], i: BackwardsIndex, x: T) =
  ls.s[ls.len - int(i)] = x
]#

proc len[T](ls: ElasticSeq[T]): int =
  ls.pos

proc high[T](ls: ElasticSeq[T]): int =
  ls.pos - 1

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

proc finished[T](sc: Scanner[T]): bool =
  sc.pos > sc.s.high

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
  if sc.pos > sc.s.high:
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

proc find(sc: Scanner[Rune], r: Rune): int =
  ## return number of consumed chars.
  ## The scanner's position is not moved.
  ## ``-1`` is returned when char is not found
  result = 0
  let pos = sc.pos
  while true:
    if sc.finished:
      result = -1
      break
    if sc.curr == r:
      break
    discard sc.next()
    inc result
  sc.pos = pos

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

proc parseUnicodeLit(sc: Scanner[Rune], size: int): Node =
  let startPos = sc.pos
  var rawCP = newString(size)
  for i in 0 ..< size:
    check(
      not sc.finished,
      ("Invalid unicode literal near position $#. " &
       "Expected $# hex digits, but found $#") %%
      [$startPos, $size, $i])
    check(
      sc.curr.int in {
        '0'.ord .. '9'.ord,
        'a'.ord .. 'z'.ord,
        'A'.ord .. 'Z'.ord},
      ("Invalid unicode literal near position $#. " &
       "Expected hex digit, but found $#") %%
      [$startPos, $sc.curr])
    rawCP[i] = sc.next().int.char
  var cp = 0
  discard parseHex(rawCp, cp)
  check(
    cp != -1 and cp <= int32.high,
    ("Invalid unicode literal near position $#. " &
     "$# value is too big.") %% [$startPos, rawCp])
  result = Rune(cp).toCharNode

proc parseUnicodeLitX(sc: Scanner[Rune]): Node =
  assert sc.peek == "{".toRune
  discard sc.next()
  let litEnd = sc.find("}".toRune)
  check(
    litEnd != -1,
    ("Invalid unicode literal near position $#, " &
     "missing `}`") %% $sc.pos)
  check(
    litEnd <= 8,
    ("Invalid unicode literal near position $#, " &
     "expected at most 8 chars, found $#") %%
    [$sc.pos, $litEnd])
  result = parseUnicodeLit(sc, litEnd)
  check(
    sc.peek == "}".toRune,
    ("Invalid unicode literal, " &
     "`}` expected at position $#") %% $(sc.pos + 1))
  discard sc.next()

proc parseOctalLit(sc: Scanner[Rune]): Node =
  let startPos = sc.pos
  var rawCP = newString(3)
  for i in 0 ..< 3:
    check(
      not sc.finished,
      ("Invalid octal literal near position $#. " &
       "Expected 3 octal digits, but found $#") %%
      [$startPos, $i])
    check(
      sc.curr.int in {'0'.ord .. '7'.ord},
      ("Invalid octal literal near position $#. " &
       "Expected octal digit, but found $#") %%
      [$startPos, $sc.curr])
    rawCP[i] = sc.next().int.char
  var cp = 0
  discard parseOct(rawCp, cp)
  result = Rune(cp).toCharNode

proc parseUnicodeNameX(sc: Scanner[Rune]): Node =
  let startPos = sc.pos
  assert sc.peek == "{".toRune
  discard sc.next()
  let nameEnd = sc.find("}".toRune)
  check(
    nameEnd != -1,
    ("Invalid unicode name near position $#, " &
     "missing `}`") %% $sc.pos)
  var name = newString(nameEnd)
  for i in 0 ..< nameEnd:
    check(
      sc.curr.int in {
        'a'.ord .. 'z'.ord,
        'A'.ord .. 'Z'.ord},
      ("Invalid unicode name, expected char in range " &
       "a-z, A-Z at position $#" %% $(sc.pos + 1)))
    name[i] = sc.next().int.char
  check(
    sc.peek == "}".toRune,
    ("Invalid unicode name, " &
     "`}` expected at position $#") %% $(sc.pos + 1))
  discard sc.next()
  check(
    name in [
      "Cn", "Lu", "Ll", "Lt", "Mn", "Mc", "Me", "Nd", "Nl",
      "No", "Zs", "Zl", "Zp", "Cc", "Cf", "Cs", "Co", "Cn",
      "Lm", "Lo", "Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po",
      "Sm", "Sc", "Sk", "So", "C", "L", "M", "N",
      "Z", "P", "S"],
    ("Invalid unicode name near position $#. " &
     "Found $#") %% [$startPos, name])
  result = Node(
    kind: reUCC,
    cp: "¿".toRune,
    cc: name)

proc parseUnicodeName(sc: Scanner[Rune]): Node =
  let startPos = sc.pos
  case sc.peek
  of "{".toRune:
    result = parseUnicodeNameX(sc)
  else:
    check(
      sc.peek in [
        "C".toRune, "L".toRune, "M".toRune, "N".toRune,
        "Z".toRune, "P".toRune, "S".toRune],
      ("Invalid unicode name near position $#. " &
       "Found $#") %% [$startPos, sc.peek.toUTF8])
    result = Node(
      kind: reUCC,
      cp: "¿".toRune,
      cc: sc.next().toUTF8)

proc parseEscapedSeq(sc: Scanner[Rune]): Node =
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

proc parseSetEscapedSeq(sc: Scanner[Rune]): Node =
  ## Just like regular ``parseEscapedSeq``
  ## but treats assertions as chars (ignore escaping)
  let cp = sc.peek
  result = parseEscapedSeq(sc)
  if result.kind in assertionKind:
    result = cp.toCharNode

proc parseAsciiSet(sc: Scanner[Rune]): Node =
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
  check(
    sc.peek == "]".toRune,
    ("Invalid ascii set near position $#, " &
     "expected [:name:]") %% $startPos)
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
    result.cps.incl(toSet([
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
    result.cps.incl(toSet([
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
    raise newException(RegexError,
      ("Invalid ascii set near position $#. " &
       "`$#` is not a valid name") %% [$startPos, name])

proc parseSet(sc: Scanner[Rune]): Node =
  ## parse a set atom (i.e ``[a-z]``) into a
  ## ``Node`` of ``reSet`` or ``reNotSet`` kind.
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
          ("Invalid set range near position $#, " &
           "range can't contain a character-class " &
           "or assertion") %% $sc.pos)
        last = nn.cp
      else:
        assert(not sc.finished)
        last = sc.next()
      let first = cps.pop()
      check(
        first <= last,
        ("Invalid set range near position $#, " &
         "start must be lesser than end") %% $sc.pos)
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
  # todo: use ref and set to nil when empty !!
  result.cps.incl(cps.toSet)
  check(
    hasEnd,
    ("Invalid set near position $#, " &
     "missing close symbol") %% $startPos)

proc parseRepRange(sc: Scanner[Rune]): Node =
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
      ("Invalid repetition range near position $#, " &
       "can only contain [0-9]") %% $sc.pos)
    curr.add(char(cp.int))
  if first.isNil:  # {n}
    first = curr
  if first.len == 0:  # {,m} or {,}
    first.add('0')
  if last.len == 0:  # {n,} or {,}
    last = "-1"
  var
    firstNum: int
    lastNum: int
  try:
    discard parseInt(first, firstNum)
    discard parseInt(last, lastNum)
  except OverflowError:
    raise newException(RegexError,
      ("Invalid repetition " &
       "range near position $#, " &
       "max value is $#, but found: $#, $#") %%
      [$sc.pos, $int16.high, first, last])
  check(
    firstNum <= int16.high and
    lastNum <= int16.high,
    ("Invalid repetition " &
     "range near position $#, " &
     "max value is $#, but found: $#, $#") %%
    [$sc.pos, $int16.high, first, last])
  # for perf reasons. This becomes a?a?a?...
  # too many parallel states
  check(
    not (lastNum - firstNum > 100),
    ("Invalid repetition range near position $#, " &
     "can't have a range greater than 100 " &
     "repetitions, but found: $#") %%
     [$sc.pos, $(lastNum - firstNum)])
  result = Node(
    kind: reRepRange,
    min: firstNum.int16,
    max: lastNum.int16)

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
  of "x".toRune:
    result = flagVerbose
  else:
    raise newException(RegexError,
      ("Invalid group flag, found $# " &
       "but expected one of: i, m, s, U or u") %% $r)

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
  of "x".toRune:
    result = flagNotVerbose
  else:
    raise newException(RegexError,
      ("Invalid group flag, found -$# " &
       "but expected one of: -i, -m, -s, -U or -u") %% $r)

template checkEmptyGroup() =
  check(
    peek(sc) != toRune(")"),
    ("Invalid group near position $#, " &
     "empty group is not allowed") %% $startPos)

proc parseGroupTag(sc: Scanner[Rune]): Node =
  ## parse a special group (name, flags, non-captures).
  ## Return a regular ``reGroupStart``
  ## if it's not special enough
  # A regular group
  let startPos = sc.pos
  if sc.curr != "?".toRune:
    checkEmptyGroup()
    return initGroupStart()
  discard sc.next()  # Consume "?"
  case sc.curr
  of ":".toRune:
    discard sc.next()
    checkEmptyGroup()
    result = initGroupStart(isCapturing = false)
  of "P".toRune:
    discard sc.next()
    check(
      sc.curr == "<".toRune,
      ("Invalid group name near position $#, " &
       "< opening symbol was expected") %% $startPos)
    discard sc.next()  # Consume "<"
    var name = newStringOfCap(75)
    for r in sc:
      if r == ">".toRune:
        break
      check(
        r.int in {
          'a'.ord .. 'z'.ord,
          'A'.ord .. 'Z'.ord,
          '0'.ord .. '9'.ord,
          '-'.ord, '_'.ord},
        ("Invalid group name near position $#. " &
         "Expected: a-z, A-Z, 0-9, " &
         "-, or _. But found `$#`") %% [$startPos, $r])
      name.add(r.int.char)
    check(
      name.len > 0,
      ("Invalid group name near position $#, " &
       "name can't be empty") %% $startPos)
    check(
      sc.prev == ">".toRune,
      ("Invalid group name near position $#, " &
       "> closing symbol was expected") %% $startPos)
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
  else:
    raise newException(RegexError,
      ("Invalid group near position $#, " &
       "unknown group type (?$#...)") %%
      [$startPos, $sc.curr])

proc subParse(sc: Scanner[Rune]): Node =
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

proc skip(sc: Scanner[Rune], vb: ElasticSeq[bool]): bool =
  ## skip white-spaces and comments on verbose mode
  result = false
  if vb.len == 0 or not vb[vb.high]:
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

proc verbosity(
    vb: var ElasticSeq[bool],
    sc: Scanner[Rune],
    n: Node) =
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

proc parse(expression: string): seq[Node] =
  ## convert a ``string`` regex expression
  ## into a ``Node`` expression
  result = newSeqOfCap[Node](expression.len)
  var vb = initElasticSeq[bool](64)
  let sc = expression.toRunes.scan()
  for _ in sc:
    if sc.skip(vb):
      continue
    result.add(sc.subParse())
    vb.verbosity(sc, result[^1])

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
    names: Table[string, int16]

proc fillGroups(expression: var seq[Node]): GroupsCapture =
  ## populate group indices, names and capturing mark
  var
    groups = initElasticSeq[int](64)
    nonCapt = 0
    names = initTable[string, int16]()
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
      check(
        groups.len > 0,
        "Invalid capturing group. " &
        "Found too many closing symbols")
      let start = groups.pop()
      n.isCapturing = expression[start].isCapturing
      n.idx = expression[start].idx
      if not n.isCapturing:
        dec nonCapt
    else:
      discard
    check(
      count < int16.high,
      ("Invalid number of capturing groups, " &
       "the limit is $#") %% $(int16.high - 1))
  check(
    groups.len == 0,
    "Invalid capturing group. " &
    "Found too many opening symbols")
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
  of flagVerbose:
    flagNotVerbose
  of flagNotVerbose:
    flagVerbose

proc squash(flags: ElasticSeq[seq[Flag]]): set[Flag] =
  result = {}
  for ff in flags:
    for f in ff:
      result.excl(f.toggle())
      result.incl(f)

proc applyFlags(expression: seq[Node]): seq[Node] =
  ## apply flags to each group
  result = newSeqOfCap[Node](expression.len)
  var flags = initElasticSeq[seq[Flag]](64)
  let sc = expression.scan()
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
        assert f in {
          flagNotAnyMatchNewLine,
          flagNotMultiLine,
          flagNotCaseInsensitive,
          flagNotUnGreedy,
          flagUnicode,
          flagVerbose,
          flagNotVerbose}
    case n.kind
    # (?flags)
    # Orphan flags are added to current group
    of reGroupStart:
      if n.flags.isNil:
        flags.add(@[])
        result.add(n)
        continue
      if sc.peek.kind == reGroupEnd:
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
      discard
    result.add(n)

proc expandOneRepRange(subExpr: seq[Node], n: Node): seq[Node] =
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

proc expandRepRange(expression: seq[Node]): seq[Node] =
  ## expand every repetition range
  result = newSeqOfCap[Node](expression.len)
  var i: int
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
      for ne in result.reversed:
        inc i
        if ne.kind == reGroupStart:
          break
      assert result[result.len - i].kind == reGroupStart
      result.add(result[result.len - i .. ^1].expandOneRepRange(n))
    of matchableKind:
      result.add(@[result[^1]].expandOneRepRange(n))
    else:
      raise newException(RegexError, (
        "Invalid repetition range, either " &
        "char, shorthand (i.e: \\w), group, or set " &
        "expected before repetition range"))

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

proc opsPA(nk: NodeKind): OpsPA =
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

proc popGreaterThan(ops: var ElasticSeq[Node], op: Node): seq[Node] =
  assert op.kind in opKind
  result = newSeqOfCap[Node](ops.len)
  while (ops.len > 0 and
      ops[ops.len - 1].kind in opKind and
      ops[ops.len - 1].kind.hasPrecedence(op.kind)):
    result.add(ops.pop())

proc popUntilGroupStart(ops: var ElasticSeq[Node]): seq[Node] =
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
  var ops = initElasticSeq[Node](64)
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

type
  End = seq[int16]
    ## store all the last
    ## states of a given state.
    ## Avoids having to recurse
    ## a state to find its ends,
    ## but have to keep them up-to-date

proc combine(
    nfa: var seq[Node],
    ends: var seq[End],
    org: int16,
    target: int16) =
  ## combine ends of ``org``
  ## with ``target``
  for e in ends[org]:
    if nfa[e].outA == 0:
      nfa[e].outA = target
    if nfa[e].outB == 0:
      nfa[e].outB = target
  ends[org] = ends[target]

proc update(
    ends: var seq[End],
    ni: int16,
    outA: int16,
    outB: int16) =
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
  result = newSeqOfCap[Node](expression.len + 2)
  result.add(initEOENode())
  var
    ends = newSeq[End](expression.len + 1)
    states = initElasticSeq[int16](64)
  ends.fill(@[])
  if expression.len == 0:
    states.add(0)
  for nn in expression:
    check(
      result.high < int16.high,
      ("The expression is too long, " &
       "limit is ~$#") %% $int16.high)
    var n = nn
    let ni = int16(result.high + 1)
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
      check(
        states.len >= 2,
        "Invalid OR conditional, nothing to " &
        "match at right/left side of the condition")
      n.outB = states.pop()
      n.outA = states.pop()
      ends.update(ni, n.outA, n.outB)
      result.add(n)
      states.add(ni)
    of reZeroOrMore:
      check(
        states.len >= 1,
        "Invalid `*` operator, " &
        "nothing to repeat")
      n.outA = states.pop()
      n.outB = 0
      ends.update(ni, n.outA, n.outB)
      result.combine(ends, n.outA, ni)
      result.add(n)
      states.add(ni)
      if n.isGreedy:
        swap(result[^1].outA, result[^1].outB)
    of reOneOrMore:
      check(
        states.len >= 1,
        "Invalid `+` operator, " &
        "nothing to repeat")
      n.outA = states.pop()
      n.outB = 0
      ends.update(ni, n.outA, n.outB)
      result.combine(ends, n.outA, ni)
      result.add(n)
      states.add(n.outA)
      if n.isGreedy:
        swap(result[^1].outA, result[^1].outB)
    of reZeroOrOne:
      check(
        states.len >= 1,
        "Invalid `?` operator, " &
        "nothing to make optional")
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
      assert(false, "Unhandled node: $#" %% $n.kind)
  assert states.len == 1
  result.add(Node(
    kind: reSkip,
    cp: "¿".toRune,
    outA: states[0],
    outB: -1))

type
  Regex* = object
    ## a compiled regular expression
    states: seq[Node]
    groupsCount: int16
    namedGroups: Table[string, int16]
  RegexMatch* = object
    ## result from matching operations
    captures: seq[Slice[int]]
    groups: seq[Slice[int]] # todo: remove, merge with captures
    namedGroups: Table[string, int16]
    boundaries*: Slice[int]
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
    ni: int16  # node idx
    ci: int  # capture idx
    si: int  # match start idx
    ei: int  # match end idx

iterator group*(m: RegexMatch, i: int): Slice[int] =
  ## return slices for a given group.
  ## Slices of start > end are empty
  ## matches (i.e.: ``re"(\d?)"``)
  ## and they are included same as in PCRE.
  for idx in m.groups[i]:
    yield m.captures[idx]

proc group*(m: RegexMatch, i: int): seq[Slice[int]] =
  ## return slices for a given group.
  ## Use the iterator version if you care about performance
  m.captures[m.groups[i]]

iterator group*(m: RegexMatch, s: string): Slice[int] =
  ## return slices for a given named group
  for idx in m.groups[m.namedGroups[s]]:
    yield m.captures[idx]

proc group*(m: RegexMatch, s: string): seq[Slice[int]] =
  ## return slices for a given named group.
  ## Use the iterator version if you care about performance
  m.group(m.namedGroups[s])

proc groupsCount*(m: RegexMatch): int =
  ## return the number of capturing groups
  ##
  ## .. code-block::
  ##   for gi in 0 ..< m.groupsCount:
  ##     for slice in m.group(gi):
  ##       echo text[slice]
  ##
  m.groups.len

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
  States = object
    states: ElasticSeq[State]
    ids: BitSet

proc initStates(size: int): States =
  result = States(
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

iterator runesIt(s: seq[Rune], start: int): (int, Rune) {.inline.} =
  var i = start
  while i < len(s):
    yield (i, s[i])
    inc i

iterator runesIt(s: string, start: int): (int, Rune) {.inline.} =
  ## yield current Rune and
  ## the ending index/start of next rune
  var
    i = start
    result: Rune
  while i < len(s):
    fastRuneAt(s, i, result, true)
    yield (i, result)

template peekImpl(s, start) =
  var
    prev = invalidRune
    j = 0
  for i, r in runesIt(s, start):
    yield (j, prev, r)
    prev = r
    j = i
  yield (j, prev, invalidRune)

iterator peek(s: string, start = 0): (int, Rune, Rune) {.inline.} =
  ## iterates over unicode characters yielding
  ## next rune index, current rune and next rune
  peekImpl(s, start)

#iterator peek(s: seq[Rune], start = 0): (int, Rune, Rune) {.inline.} =
#  ## iterates over unicode characters yielding
#  ## next rune index, current rune and next rune
#  peekImpl(s, start)

proc toVisitStep(
    result: var ElasticSeq[State],
    n: Node,
    ci: int,
    si: int,
    ei: int) {.inline.} =
  if n.outB != -1:
    result.add((ni: n.outB, ci: ci, si: si, ei: ei))
  if n.outA != -1:
    result.add((ni: n.outA, ci: ci, si: si, ei: ei))

proc step(
    result: var States,
    pattern: Regex,
    state: State,
    captured: var ElasticSeq[Capture],
    visited: var BitSet,
    toVisit: var ElasticSeq[State],
    cp: Rune,
    nxt: Rune) =
  assert toVisit.len == 0
  toVisit.add(state)
  while toVisit.len > 0:
    let s = toVisit.pop()
    if s.ni in visited:
      continue
    visited.incl(s.ni)
    let n = pattern.states[s.ni]
    case n.kind
    of matchableKind, reEOE:
      result.add(s)
    of assertionKind:
      if n.match(cp, nxt):
        toVisitStep(toVisit, n, s.ci, s.si, s.ei)
    of reGroupStart:
      if not (n.isCapturing and
          captured.isInitialized):
        toVisitStep(toVisit, n, s.ci, s.si, s.ei)
        continue
      captured.add(Capture(
        kind: captStart,
        cpIdx: state.ei,
        prev: s.ci,
        idx: n.idx))
      toVisitStep(toVisit, n, captured.high, s.si, s.ei)
    of reGroupEnd:
      if not (n.isCapturing and
          captured.isInitialized):
        toVisitStep(toVisit, n, s.ci, s.si, s.ei)
        continue
      captured.add(Capture(
        kind: captEnd,
        cpIdx: state.ei - 1,
        prev: s.ci,
        idx: n.idx))
      toVisitStep(toVisit, n, captured.high, s.si, s.ei)
    else:
      toVisitStep(toVisit, n, s.ci, s.si, s.ei)

proc stepFrom(
    result: var States,
    n: Node,
    pattern: Regex,
    captured: var ElasticSeq[Capture],
    ci: int,
    si: int,
    ei: int,
    visited: var BitSet,
    toVisit: var ElasticSeq[State],
    cp: Rune,
    nxt: Rune) {.inline.} =
  ## go to next states
  if n.outA != -1:
    let state = (ni: n.outA, ci: ci, si: si, ei: ei)
    result.step(
      pattern, state, captured, visited, toVisit, cp, nxt)
  if n.outB != -1:
    let state = (ni: n.outB, ci: ci, si: si, ei: ei)
    result.step(
      pattern, state, captured, visited, toVisit, cp, nxt)

proc setRegexMatch(
    result: var Option[RegexMatch],
    states: States,
    pattern: Regex,
    captured: ElasticSeq[Capture]) {.inline.} =
  for state in states:
    if pattern.states[state.ni].kind == reEOE:
      var m = RegexMatch()
      m.boundaries = state.si .. state.ei - 1
      if pattern.groupsCount > 0:
        m.populateCaptures(captured, state.ci, pattern.groupsCount)
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

proc match*(s: string, pattern: Regex, start=0): Option[RegexMatch] =
  ## return a match if the whole string
  ## matches the regular expression. This
  ## is similar to ``find(text, re"^regex$")``
  ## but has better performance
  ##
  ## .. code-block:: nim
  ##   doAssert("abcd".match(re"abcd").isSome)
  ##   doAssert(not "abcd".match(re"abc").isSome)
  ##
  initDataSets(true)
  let statesCount = pattern.states.high.int16
  for i, cp, nxt in s.peek(start):
    if cp == invalidRune:
      assert currStates.len == 0
      assert i == 0
      let state = (ni: statesCount, ci: 0, si: 0, ei: 0)
      currStates.step(
        pattern, state, captured, visited, toVisit, cp, nxt)
      continue
    if currStates.len == 0:
      break
    for st in currStates:
      let n = pattern.states[st.ni]
      if not n.match(cp):
        continue
      visited.clear()
      nextStates.stepFrom(
        n, pattern, captured, st.ci, st.si, i, visited, toVisit, cp, nxt)
    swap(currStates, nextStates)
    nextStates.clear()
  setRegexMatch(result, currStates, pattern, captured)

proc contains*(s: string, pattern: Regex): bool =
  ##  search for the pattern anywhere
  ##  in the string. It returns as soon
  ##  as there is a match, even when the
  ##  expression has repetitions. Use
  ##  ``re"^regex$"`` to match the whole
  ##  string instead of searching
  ##
  ## .. code-block:: nim
  ##   doAssert(re"bc" in "abcd")
  ##   doAssert(re"(23)+" in "23232")
  ##   doAssert(re"^(23)+$" notin "23232")
  ##
  initDataSets(false)
  let statesCount = pattern.states.high.int16
  for _, cp, nxt in s.peek:
    visited.clear()
    let state = (ni: statesCount, ci: 0, si: 0, ei: 0)
    currStates.step(
      pattern, state, captured, visited, toVisit, cp, nxt)
    if cp == invalidRune:
      continue
    for st in currStates:
      let n = pattern.states[st.ni]
      result = n.kind == reEOE
      if result:
        return
      if not n.match(cp):
        continue
      visited.clear()
      nextStates.stepFrom(
        n, pattern, captured, 0, 0, 0, visited, toVisit, cp, nxt)
    swap(currStates, nextStates)
    nextStates.clear()
  for state in currStates:
    result = pattern.states[state.ni].kind == reEOE
    if result:
      return

proc find*(s: string, pattern: Regex, start = 0): Option[RegexMatch] =
  ## search through the string looking for the first
  ## location where there is a match
  ##
  ## .. code-block:: nim
  ##   doAssert("abcd".find(re"bc").isSome)
  ##   doAssert(not "abcd".find(re"de").isSome)
  ##   doAssert("2222".find(re"(22)*").get().group(0) ==
  ##     @[0 .. 1, 2 .. 3])
  ##
  initDataSets(true)
  let statesCount = pattern.states.high.int16
  var si = start
  for i, cp, nxt in s.peek(start):
    # stop trying to match when there is a match
    if currStates.len == 0 or
        pattern.states[currStates[currStates.len - 1].ni].kind != reEOE:
      visited.clear()
      let state = (ni: statesCount, ci: 0, si: si, ei: si)
      currStates.step(
        pattern, state, captured, visited, toVisit, cp, nxt)
      si = i
    # break on longest match
    if currStates.len > 0 and
        pattern.states[currStates[0].ni].kind == reEOE:
      break
    if cp == invalidRune:
      continue
    for st in currStates:
      let n = pattern.states[st.ni]
      if n.kind == reEOE:
        # todo: put in var stateEOE?
        nextStates.add(st)
        break
      if not n.match(cp):
        continue
      visited.clear()
      nextStates.stepFrom(
        n, pattern, captured, st.ci, st.si, i, visited, toVisit, cp, nxt)
    swap(currStates, nextStates)
    nextStates.clear()
  setRegexMatch(result, currStates, pattern, captured)

proc runeIncAt(s: string, n: var int) {.inline.} =
  ## increment ``n`` up to
  ## next rune's index
  inc(n, runeLenAt(s, n))

iterator findAll*(
    s: string, pattern: Regex, start = 0): RegexMatch {.inline.} =
  ## search through the string and
  ## return each match. Empty matches
  ## (start > end) are included
  ##
  ## .. code-block:: nim
  ##   var i = 0
  ##   let expected = [1 .. 2, 4 .. 5]
  ##   for m in findAll("abcabc", re"bc"):
  ##     doAssert(m.boundaries == expected[i])
  ##     inc i
  ##
  # todo: pass/reuse data structures
  var i = start
  while i < s.len:
    let m = find(s, pattern, i)
    if not isSome(m): break
    let mm = m.get()
    yield mm
    let b = mm.boundaries
    if b.b == i or b.a > b.b: inc i  # todo: incRune and test
    else: i = b.b + 1

proc findAll*(s: string, pattern: Regex, start = 0): seq[RegexMatch] =
  ## search through the string and
  ## return each match. Empty matches
  ## (start > end) are included
  ##
  ## .. code-block:: nim
  ##   let
  ##     expected = [1 .. 2, 4 .. 5]
  ##     ms = findAll("abcabc", re"bc")
  ##   for i, m in ms:
  ##     doAssert(m.boundaries == expected[i])
  ##
  result = newSeqOfCap[RegexMatch](s.len)
  for slc in findAll(s, pattern, start):
    result.add(slc)

proc matchEnd(s: string, pattern: Regex, start = 0): int =
  ## return end index of the longest match.
  ## Return ``-1`` when there is no match.
  ## Pattern is anchored to the start of the string
  result = -1
  initDataSets(false)
  let statesCount = pattern.states.high.int16
  for i, cp, nxt in s.peek(start):
    if cp == invalidRune:
      assert currStates.len == 0
      assert i == 0
      let state = (ni: statesCount, ci: 0, si: 0, ei: 0)
      currStates.step(
        pattern, state, captured, visited, toVisit, cp, nxt)
      continue
    if currStates.len == 0:
      break
    if pattern.states[currStates[0].ni].kind == reEOE:
      break
    for st in currStates:
      let n = pattern.states[st.ni]
      if n.kind == reEOE:
        nextStates.add(st)
        break
      if not n.match(cp):
        continue
      visited.clear()
      nextStates.stepFrom(
        n, pattern, captured, st.ci, st.si, i, visited, toVisit, cp, nxt)
    swap(currStates, nextStates)
    nextStates.clear()
  for state in currStates:
    if pattern.states[state.ni].kind == reEOE:
      result = state.ei
      return

iterator split*(s: string, sep: Regex): string {.inline.} =
  ## return not matched substrings
  ##
  ## .. code-block:: nim
  ##   var i = 0
  ##   let expected = ["", "a", "Ϊ", "Ⓐ", "弢", ""]
  ##   for s in split("11a22Ϊ33Ⓐ44弢55", re"\d+"):
  ##     doAssert(s == expected[i])
  ##     inc i
  ##
  # todo: pass/reuse data structures
  var
    first = 0
    last = 0
    n = 0
  while last <= s.len:
    first = last
    while last <= s.len:
      n = matchEnd(s, sep, last)
      #if last == n: s.incRune(last)
      if n > 0: break
      s.runeIncAt(last)
    yield substr(s, first, last - 1)
    if n > 0: last = n

proc split*(s: string, sep: Regex): seq[string] =
  ## return not matched substrings
  ##
  ## .. code-block:: nim
  ##   doAssert(split("11a22Ϊ33Ⓐ44弢55", re"\d+") ==
  ##     @["", "a", "Ϊ", "Ⓐ", "弢", ""])
  ##
  result = newSeqOfCap[string](s.len)
  for w in s.split(sep):
    result.add(w)

proc startsWith*(s: string, pattern: Regex, start = 0): bool =
  ## return whether the string
  ## starts with the pattern or not
  ##
  ## .. code-block:: nim
  ##   doAssert("abc".startsWith(re"\w"))
  ##   doAssert(not "abc".startsWith(re"\d"))
  ##
  # todo: shortest match (matchEnd matches longest match), for perf
  matchEnd(s, pattern, start) != -1

proc endsWith*(s: string, pattern: Regex): bool =
  ## return whether the string
  ## ends with the pattern or not
  ##
  ## .. code-block:: nim
  ##   doAssert("abc".endsWith(re"\w"))
  ##   doAssert(not "abc".endsWith(re"\d"))
  ##
  # todo: pass/reuse data structures
  result = false
  var i = 0
  while i < s.len:
    result = isSome(match(s, pattern, i))
    if result: return
    s.runeIncAt(i)

proc flatCaptures(result: var seq[string], m: RegexMatch, s: string) =
  ## Concat capture repetitions
  var
    i, n = 0
    ss: string
  for g in 0 ..< m.groupsCount:
    n = 0
    for sl in m.group(g):
      if sl.a <= sl.b:
        n.inc(sl.b - sl.a + 1)
    ss = newString(n)
    i = 0
    for sl in m.group(g):
      for c in sl:
        ss[i] = s[c]
        inc i
    assert i == n
    result[g] = ss

proc addsubstr(result: var string, s: string, first, last: int) =
  let
    first = max(first, 0)
    last = min(last, s.high)
  if first > last: return
  let n = result.len
  result.setLen(result.len + (last - first) + 1)
  # todo: copyMem
  var j = 0
  for i in first .. last:
    result[n + j] = s[i]
    inc j

proc addsubstr(result: var string, s: string, first: int) =
  addsubstr(result, s, first, s.high)

proc replace*(
    s: string,
    pattern: Regex,
    by: string,
    limit = 0): string =
  ## Replace matched substrings.
  ##
  ## Matched groups can be accessed with ``$N``
  ## notation, where ``N`` is the group's index,
  ## starting at 1 (1-indexed). ``$$`` means
  ## literal ``$``.
  ##
  ## If ``limit`` is given, at most ``limit``
  ## replacements are done. ``limit`` of 0
  ## means there is no limit
  ##
  ## .. code-block:: nim
  ##   doAssert("aaa".replace(re"a", "b", 1) == "baa")
  ##   doAssert("abc".replace(re"(a(b)c)", "m($1) m($2)") ==
  ##     "m(abc) m(b)")
  ##   doAssert("Nim is awesome!".replace(re"(\w\B)", "$1_") ==
  ##     "N_i_m i_s a_w_e_s_o_m_e!")
  ##
  result = ""
  var
    i, j = 0
    capts = newSeq[string](pattern.groupsCount)
  for m in findAll(s, pattern):
    result.addsubstr(s, i, m.boundaries.a - 1)
    flatCaptures(capts, m, s)
    if capts.len > 0:
      result.addf(by, capts)
    else:
      result.add(by)
    i = m.boundaries.b + 1
    inc j
    if limit > 0 and j == limit: break
  result.addsubstr(s, i)

proc replace*(
    s: string,
    pattern: Regex,
    by: proc (m: RegexMatch, s: string): string,
    limit = 0): string =
  ## Replace matched substrings.
  ##
  ## If ``limit`` is given, at most ``limit``
  ## replacements are done. ``limit`` of 0
  ## means there is no limit
  ##
  ## .. code-block:: nim
  ##   proc removeEvenWords(m: RegexMatch, s: string): string =
  ##     result = ""
  ##     if m.group(1).len mod 2 != 0:
  ##       result = s[m.group(0)[0]]
  ##
  ##   let
  ##     text = "Es macht Spaß, alle geraden Wörter zu entfernen!"
  ##     expected = "macht , geraden entfernen!"
  ##   doAssert(text.replace(re"((\w)+\s*)", removeEvenWords) == expected)
  ##
  result = ""
  var i, j = 0
  for m in findAll(s, pattern):
    result.addsubstr(s, i, m.boundaries.a - 1)
    result.add(by(m, s))
    i = m.boundaries.b + 1
    inc j
    if limit > 0 and j == limit: break
  result.addsubstr(s, i)

proc toPattern*(s: string): Regex {.raises: [RegexError].} =
  ## Parse and compile a regular expression.
  ## Use the ``re`` template if you
  ## care about performance.
  ##
  ## .. code-block:: nim
  ##   # compiled at run-time
  ##   let patternA = toPattern(r"aa?")
  ##   # compiled at compile-time
  ##   const patternB = toPattern(r"aa?")
  ##
  var ns = s.parse
  let gc = ns.fillGroups()
  var names: Table[string, int16]
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
    let mm = m.get()
    result = newSeq[seq[string]](mm.groups.len)
    var j = 0
    for i, gbounds in mm.groups:
      result[i] = newSeq[string](gbounds.b - gbounds.a + 1)
      j = 0
      for cbounds in mm.group(i):
        result[i][j] = s[cbounds]
        inc j

  proc matchWithCapt(s: string, pattern: Regex): seq[seq[string]] =
    s.match(pattern).toStrCaptures(s)

  proc findWithCapt(s: string, pattern: Regex): seq[seq[string]] =
    s.find(pattern).toStrCaptures(s)

  proc findAllb(s: string, pattern: Regex): seq[Slice[int]] =
    result = newSeqOfCap[Slice[int]](s.len)
    for m in findAll(s, pattern):
      result.add(m.boundaries)

  proc raises(pattern: string): bool =
    result = false
    try:
      discard pattern.toPattern()
    except RegexError:
      result = true

  proc raisesMsg(pattern: string): string =
    try:
      discard pattern.toPattern()
    except RegexError as e:
      result = e.msg

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
  # raw string need double "" instead of \" to escape,
  # this is a Nim thing
  doAssert(" \"word\" ".isMatch(re"\s"".*""\s"))

  # trepetition_cycle
  doAssert("aaa".isMatch(re"a**"))
  doAssert("aaa".isMatch(re"(a*)*"))
  doAssert("aaabbbaaa".isMatch(re"((a*|b*))*"))
  doAssert("aaa".isMatch(re"a*****"))
  doAssert(raises(r"a*{,}"))
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

  # tzero_or_more_op
  doAssert(raisesMsg(r"*") ==
    "Invalid `*` operator, nothing to repeat")
  doAssert(raises(r"*abc"))
  doAssert(not raises(r"\b*"))

  # tone_or_more_op
  doAssert("aaaa".isMatch(re"a+"))
  doAssert("abb".isMatch(re"ab+"))
  doAssert("abaa".isMatch(re"aba+"))
  doAssert(not "".isMatch(re"a+"))
  doAssert(not "b".isMatch(re"a+"))
  doAssert(not "aab".isMatch(re"b+"))
  doAssert(raisesMsg(r"(+)") ==
    "Invalid `+` operator, nothing to repeat")
  doAssert(raises(r"+"))
  doAssert(raises(r"+abc"))
  doAssert(not raises(r"\b+"))

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
  doAssert(raisesMsg(r"?") ==
    "Invalid `?` operator, nothing to make optional")
  doAssert(raises(r"?abc"))
  doAssert(not raises(r"\b?"))

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
  doAssert(not "⅕".isMatch(re"\d"))

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
  doAssert("⅕".isMatch(re"\D"))
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
  doAssert(raisesMsg(r"[a-\w]") ==
    "Invalid set range near position 5, " &
    "range can't contain a character-class or assertion")
  doAssert(not raises(r"[a-\b]"))
  doAssert(raisesMsg(r"[d-c]") ==
    "Invalid set range near position 4, " &
    "start must be lesser than end")
  doAssert(raisesMsg(r"abc[]") ==
    "Invalid set near position 4, missing close symbol")
  doAssert(raisesMsg(r"[]abc") ==
    "Invalid set near position 1, missing close symbol")
  doAssert(raisesMsg(r"[abc") ==
    "Invalid set near position 1, missing close symbol")
  doAssert(raises(r"[a"))
  doAssert(raises(r"[a-"))
  doAssert(raises(r"[-a"))
  doAssert(raises(r"[\\"))
  doAssert(raises(r"[]"))
  doAssert(raises(r"[^]"))
  doAssert(raises(r"[]a"))
  doAssert(raises(r"[-"))
  doAssert("a".isMatch(re"[\u0061]"))
  doAssert(not "b".isMatch(re"[\u0061]"))
  doAssert("a".isMatch(re"[\U00000061]"))
  doAssert("a".isMatch(re"[\x61]"))
  doAssert("a".isMatch(re"[\x{61}]"))
  doAssert("abab".isMatch(re"[\x61-\x62]*"))
  doAssert("a".isMatch(re"[\141]"))

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
  doAssert(raisesMsg(r"[^]") ==
    "Invalid set near position 1, " &
    "missing close symbol")
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
  doAssert(raises(r"a*{,}"))
  doAssert(raises(r"a*{0}"))
  doAssert(raises(r"a*{1}"))
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
  doAssert(raisesMsg(r"a{bad}") ==
    "Invalid repetition range near " &
    "position 3, can only contain [0-9]")
  doAssert(raisesMsg(r"a{1111111111}") ==
    "Invalid repetition range near " &
    "position 13, max value is 32767, " &
    "but found: 1111111111, 1111111111")
  doAssert(raisesMsg(r"a{0,101}") ==
    "Invalid repetition range near " &
    "position 8, can't have a range " &
    "greater than 100 repetitions, but found: 101")
  doAssert(not raises(r"a{1,101}"))
  doAssert(raises(r"a{0,a}"))
  doAssert(raises(r"a{a,1}"))
  doAssert(raises(r"a{-1}"))
  doAssert(raisesMsg(r"{10}") ==
    "Invalid repeition range, " &
    "nothing to repeat")
  doAssert(raisesMsg(r"abc\A{10}") ==
    "Invalid repetition range, either " &
    "char, shorthand (i.e: \\w), group, or set " &
    "expected before repetition range")

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
    "aaaa".match(re"(a*?)(a*?)(a*)").toStrCaptures("aaaa") ==
    @[@[""], @[""], @["aaaa"]])
  doAssert(
    "aaaa".match(re"(a*)(a*?)(a*?)").toStrCaptures("aaaa") ==
    @[@["aaaa"], @[""], @[""]])

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
    "ab".match(re"(a)(b)").toStrCaptures("ab") ==
    @[@["a"], @["b"]])

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

  doAssert(raisesMsg(r"abc(?Pabc)") ==
    "Invalid group name near position " &
    "4, < opening symbol was expected")
  doAssert(raisesMsg(r"abc(?P<abc") ==
    "Invalid group name near position 4, " &
    "> closing symbol was expected")
  doAssert(raisesMsg(r"a(?P<>abc)") ==
    "Invalid group name near position 2, " &
    "name can't be empty")
  doAssert(raisesMsg(r"(a)b)") ==
    "Invalid capturing group. " &
    "Found too many closing symbols")
  doAssert(raisesMsg(r"(b(a)") ==
    "Invalid capturing group. " &
    "Found too many opening symbols")
  doAssert(raisesMsg(r"()") ==
    "Invalid group near position 1, " &
    "empty group is not allowed")
  doAssert(raisesMsg(r"a(?P<asd)") ==
    "Invalid group name near position 2. " &
    "Expected: a-z, A-Z, 0-9, -, or _. But found `)`")
  doAssert(not raises(r"(?P<abcdefghijklmnopqrstuvwxyz" &
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_>abc)"))
  doAssert(not raises(r"(\b)"))
  #[
  var manyGroups = newStringOfCap(int16.high * 3)
  for _ in 0 ..< int16.high - 1:
    manyGroups.add(r"(a)")
  doAssert(not raises(manyGroups))
  manyGroups.add(r"(a)")
  doAssert(raisesMsg(manyGroups) ==
    "Invalid number of capturing " &
    "groups, the limit is 32766")
  ]#

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
  doAssert("ƎƎ".isMatch(re"(?-u)[^\w](?u)\w"))

  doAssert("a".isMatch(re"(?x)a"))
  doAssert("a".isMatch(re"(?x)a "))
  doAssert("a".isMatch(re"(?x)a   "))
  doAssert("a".isMatch(re"(?x) a "))
  doAssert("a".isMatch(re("(?x)a\L   \L   \L")))
  doAssert("a".isMatch(re("(?x)\L a \L")))
  doAssert("a".isMatch(re"(?x: a )"))
  doAssert("a".isMatch(re"""(?x)a"""))
  doAssert("a".isMatch(re"""(?x)
    a
    """))
  doAssert("a".isMatch(re"""(?x:
    a
    )"""))
  doAssert("a".isMatch(re"""(?x)(
    a
    )"""))
  doAssert("a".isMatch(re"""(?x)
    a  # should ignore this comment
    """))
  doAssert("a".isMatch(re"""(?x:
    a  # should ignore this comment
    )"""))
  doAssert("aa ".isMatch(re"(?x)a  (?-x)a "))
  doAssert("a a".isMatch(re"a (?x)a  "))
  doAssert("aa".isMatch(re"((?x)a    )a"))
  doAssert("aa".isMatch(re"(?x:a    )a"))
  doAssert("a ".isMatch(re"(?x)a\ "))
  doAssert("a ".isMatch(re"(?x)a\   "))
  doAssert("a#".isMatch(re"(?x)a\#"))
  doAssert("a ".isMatch(re"(?x)a[ ]"))
  doAssert("a\n".isMatch(re"(?x)a\n"))
  doAssert("aa ".isMatch(re"""(?x)
    a    #    comment
    (?-x)a """))
  doAssert("aaa".isMatch(re"""(?x)  # comment
    a  # comment
    a  # comment
    a  # comment
    # comment"""))
  doAssert("12.0".isMatch(re"""(?x)
    \d +  # the integral part
    \.    # the decimal point
    \d *  # some fractional digits"""))
  doAssert(re"""(?x)    # verbose mode
    ^                   # beginning of string
    M{0,4}              # thousands - 0 to 4 M's
    (CM|CD|D?C{0,3})    # hundreds - 900 (CM), 400 (CD), 0-300 (0 to 3 C's),
                        #            or 500-800 (D, followed by 0 to 3 C's)
    (XC|XL|L?X{0,3})    # tens - 90 (XC), 40 (XL), 0-30 (0 to 3 X's),
                        #        or 50-80 (L, followed by 0 to 3 X's)
    (IX|IV|V?I{0,3})    # ones - 9 (IX), 4 (IV), 0-3 (0 to 3 I's),
                        #        or 5-8 (V, followed by 0 to 3 I's)
    $                   # end of string
    """ in "MMMMDCCCLXXXVIII")

  doAssert(raisesMsg(r"(?uq)") ==
    "Invalid group flag, found q but " &
    "expected one of: i, m, s, U or u")
  doAssert(raisesMsg(r"(?u-q)") ==
    "Invalid group flag, found -q but " &
    "expected one of: -i, -m, -s, -U or -u")
  doAssert(raisesMsg(r"abc(?q)") ==
    "Invalid group near position 4, " &
    "unknown group type (?q...)")

  # tor_op
  doAssert(raisesMsg(r"|") ==
    "Invalid OR conditional, nothing " &
    "to match at right/left side of the condition")
  doAssert(raises(r"abc|"))
  doAssert(raises(r"|abc"))

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
  doAssert("a".find(re"").isSome)
  doAssert("abcd".find(re"^abcd$").isSome)
  doAssert("2222".findWithCapt(re"(22)*") ==
    @[@["22", "22"]])
  doAssert("2222".find(re"(22)*").get().group(0) ==
    @[0 .. 1, 2 .. 3])
  doAssert("abcd".find(re"(ab)").get().group(0) == @[0 .. 1])
  doAssert("abcd".find(re"(bc)").get().group(0) == @[1 .. 2])
  doAssert("abcd".find(re"(cd)").get().group(0) == @[2 .. 3])
  doAssert("abcd".find(re"bc").get().boundaries == 1 .. 2)
  doAssert("aΪⒶ弢".find(re"Ϊ").get().boundaries == 1 .. 2)
  doAssert("aΪⒶ弢".find(re"Ⓐ").get().boundaries == 3 .. 5)
  doAssert("aΪⒶ弢".find(re"弢").get().boundaries == 6 .. 9)

  # tcontains
  doAssert(re"bc" in "abcd")
  doAssert(re"bd" notin "abcd")
  doAssert(re"(23)+" in "2323")
  doAssert(re"(23)+" in "23232")
  doAssert(re"^(23)+$" notin "23232")

  # tsplit
  doAssert(split("a,b,c", re",") == @["a", "b", "c"])
  doAssert(
    split("00232this02939is39an22example111", re"\d+") ==
    @["", "this", "is", "an", "example", ""])
  doAssert(
    split("AAA :   : BBB", re"\s*:\s*") ==
    @["AAA", "", "BBB"])
  doAssert(split("", re",") == @[""])
  doAssert(split(",,", re",") == @["", "", ""])
  doAssert(split("abc", re"") == @["abc"])
  doAssert(
    split(",a,Ϊ,Ⓐ,弢,", re",") ==
    @["", "a", "Ϊ", "Ⓐ", "弢", ""])
  doAssert(split("弢", re"\xAF") == @["弢"])  # "弢" == "\xF0\xAF\xA2\x94"
  block:
    var
      expected = ["", "a", "Ϊ", "Ⓐ", "弢", ""]
      i = 0
    for s in split("11a22Ϊ33Ⓐ44弢55", re"\d+"):
      doAssert(s == expected[i])
      inc i

  # tfindall
  doAssert(findAllb("abcabc", re"bc") == @[1 .. 2, 4 .. 5])
  doAssert(findAllb("aa", re"a") == @[0 .. 0, 1 .. 1])
  doAssert(findAllb("a", re"a") == @[0 .. 0])
  doAssert(findAllb("a", re"b") == @[])
  doAssert(findAllb("", re"b") == @[])
  doAssert(findAllb("a", re"") == @[0 .. -1])
  doAssert(findAllb("ab", re"") == @[0 .. -1, 1 .. 0])
  doAssert(findAllb("a", re"\b") == @[0 .. -1])
  doAssert(findAllb("aΪⒶ弢", re"Ϊ") == @[1 .. 2])
  doAssert(findAllb("aΪⒶ弢", re"Ⓐ") == @[3 .. 5])
  doAssert(findAllb("aΪⒶ弢", re"弢") == @[6 .. 9])
  doAssert(findAllb("aΪⒶ弢aΪⒶ弢", re"Ⓐ") == @[3 .. 5, 13 .. 15])
  doAssert(findAllb("aaa", re"a*") == @[0 .. 2])

  # tstarts_with
  doAssert("abc".startsWith(re"ab"))
  doAssert(not "abc".startsWith(re"bc"))
  doAssert(startsWith("弢ⒶΪ", re"弢Ⓐ"))
  doAssert(startsWith("弢", re("\xF0\xAF\xA2\x94")))
  doAssert(not startsWith("弢", re("\xF0\xAF\xA2")))
  doAssert("abc".startsWith(re"\w"))
  doAssert(not "abc".startsWith(re"\d"))
  doAssert("abc".startsWith(re"(a|b)"))
  doAssert("bc".startsWith(re"(a|b)"))
  doAssert(not "c".startsWith(re"(a|b)"))

  # tends_with
  doAssert("abc".endsWith(re"bc"))
  doAssert(not "abc".endsWith(re"ab"))
  doAssert(endsWith("弢ⒶΪ", re"ⒶΪ"))
  doAssert(endsWith("弢", re("\xF0\xAF\xA2\x94")))
  doAssert(not endsWith("弢", re("\xAF\xA2\x94")))
  doAssert("abc".endsWith(re"(b|c)"))
  doAssert("ab".endsWith(re"(b|c)"))
  doAssert(not "a".endsWith(re"(b|c)"))

  # tliterals
  doAssert("a".isMatch(re"\u0061"))
  doAssert(not "b".isMatch(re"\u0061"))
  doAssert("b".isMatch(re"\u0062"))
  doAssert("Ⓐ".isMatch(re"\u24b6"))
  doAssert("Ⓐ".isMatch(re"\u24B6"))
  doAssert(raisesMsg(r"\u123") ==
    "Invalid unicode literal near position 2. " &
    "Expected 4 hex digits, but found 3")
  doAssert(raisesMsg(r"\u123@abc") ==
    "Invalid unicode literal near position 2. " &
    "Expected hex digit, but found @")
  doAssert("a".isMatch(re"\U00000061"))
  doAssert(not "b".isMatch(re"\U00000061"))
  doAssert("b".isMatch(re"\U00000062"))
  doAssert("弢".isMatch(re"\U0002f894"))
  doAssert("弢".isMatch(re"\U0002F894"))
  doAssert(raisesMsg(r"\U123") ==
    "Invalid unicode literal near position 2. " &
    "Expected 8 hex digits, but found 3")
  doAssert(raisesMsg(r"\U123@a") ==
    "Invalid unicode literal near position 2. " &
    "Expected hex digit, but found @")
  doAssert(raisesMsg(r"\UFFFFFFFF") ==
    "Invalid unicode literal near position 2. " &
    "FFFFFFFF value is too big.")
  doAssert("a".isMatch(re"\x{61}"))
  doAssert("a".isMatch(re"\x{061}"))
  doAssert(not "b".isMatch(re"\x{61}"))
  doAssert("Ⓐ".isMatch(re"\x{24b6}"))
  doAssert("Ⓐ".isMatch(re"\x{000024b6}"))
  doAssert("弢".isMatch(re"\x{2f894}"))
  doAssert("弢".isMatch(re"\x{0002f894}"))
  doAssert(raises(r"\x{FFFFFFFF}"))
  doAssert(not raises(r"\x{7fffffff}"))
  doAssert(raisesMsg(r"\x{2f894") ==
    "Invalid unicode literal near position 3, missing `}`")
  doAssert(raisesMsg(r"\x{00000000A}") ==
    "Invalid unicode literal near position 3, " &
    "expected at most 8 chars, found 9")
  doAssert(raisesMsg(r"\x{61@}") ==
    "Invalid unicode literal near position 3. " &
    "Expected hex digit, but found @")
  doAssert("a".isMatch(re"\x61"))
  doAssert("aa".isMatch(re"\x61a"))
  doAssert("a".isMatch(re"\x61"))
  doAssert("a".isMatch(re"\141"))
  doAssert(not "b".isMatch(re"\141"))
  doAssert("aa".isMatch(re"\141a"))
  doAssert("\u1ff".isMatch(re"\777"))
  doAssert("888".isMatch(re"\888"))
  doAssert(raisesMsg(r"\12") ==
    "Invalid octal literal near position 1. " &
    "Expected 3 octal digits, but found 2")
  doAssert(raisesMsg(r"\12@") ==
    "Invalid octal literal near position 1. " &
    "Expected octal digit, but found @")

  # tchar_class
  doAssert("a".isMatch(re"\pL"))
  doAssert(not "a".isMatch(re"\PL"))
  doAssert(not "1".isMatch(re"\pL"))
  doAssert("1".isMatch(re"\PL"))
  doAssert("aa".isMatch(re"\pLa"))
  doAssert("1".isMatch(re"\pN"))
  doAssert("_".isMatch(re"\pP"))
  doAssert("+".isMatch(re"\pS"))
  doAssert(" ".isMatch(re"\pZ"))
  doAssert(raisesMsg(r"\pB") ==
    "Invalid unicode name near position 2. Found B")
  doAssert(raisesMsg(r"\p11") ==
    "Invalid unicode name near position 2. Found 1")
  doAssert("a".isMatch(re"\p{L}"))
  doAssert("ǅ".isMatch(re"\p{Lt}"))
  doAssert(not "ǅ".isMatch(re"\P{Lt}"))
  doAssert(not "a".isMatch(re"\p{Lt}"))
  doAssert("a".isMatch(re"\P{Lt}"))
  doAssert(raisesMsg(r"\p{Bb}") ==
    "Invalid unicode name near position 2. Found Bb")
  doAssert(raisesMsg(r"\p{11}") ==
    "Invalid unicode name, expected char in range " &
    "a-z, A-Z at position 4")

  # tascii_set
  doAssert(r"[[:alnum:]]".toAtoms == "[[0-9a-zA-Z]]")
  doAssert(r"[[:^alnum:]]".toAtoms == "[[^0-9a-zA-Z]]")
  doAssert(r"[[:alpha:]]".toAtoms == "[[a-zA-Z]]")
  doAssert(r"[[:ascii:]]".toAtoms == "[[\x00-\x7F]]")
  doAssert(r"[[:blank:]]".toAtoms == "[[\t ]]")
  doAssert(r"[[:cntrl:]]".toAtoms == "[[\x7F\x00-\x1F]]")
  doAssert(r"[[:digit:]]".toAtoms == "[[0-9]]")
  doAssert(r"[[:graph:]]".toAtoms == "[[!-~]]")
  doAssert(r"[[:lower:]]".toAtoms == "[[a-z]]")
  doAssert(r"[[:print:]]".toAtoms == "[[ -~]]")
  doAssert(r"[[:punct:]]".toAtoms == "[[!-/:-@[-`{-~]]")
  doAssert(r"[[:space:]]".toAtoms == "[[\t\n\v\f\r ]]")
  doAssert(r"[[:upper:]]".toAtoms == "[[A-Z]]")
  doAssert(r"[[:word:]]".toAtoms == "[[_0-9a-zA-Z]]")
  doAssert(r"[[:xdigit:]]".toAtoms == "[[0-9a-fA-F]]")
  doAssert(r"[[:alpha:][:digit:]]".toAtoms == "[[a-zA-Z][0-9]]")
  doAssert("d".isMatch(re"[[:alnum:]]"))
  doAssert("5".isMatch(re"[[:alnum:]]"))
  doAssert(not "{".isMatch(re"[[:alnum:]]"))
  doAssert("{".isMatch(re"[[:alnum:]{]"))
  doAssert("-".isMatch(re"[[:alnum:]-z]"))
  doAssert(raisesMsg(r"[z-[:alnum:]]") ==
    "Invalid set range near position 4, " &
    "start must be lesser than end")
  doAssert("a".isMatch(re"[[[[:alnum:]]"))
  doAssert("[".isMatch(re"[[[:alnum:]]"))
  doAssert(not ":".isMatch(re"[[:alnum:]]"))
  doAssert(":".isMatch(re"[:alnum:]"))
  doAssert(not "a".isMatch(re"[[:^alnum:]]"))
  doAssert("{".isMatch(re"[[:^alnum:]]"))
  doAssert(not "5".isMatch(re"[[:alpha:]]"))
  doAssert(not "a".isMatch(re"[[:digit:]]"))
  doAssert("5".isMatch(re"[[:alpha:][:digit:]]"))
  doAssert("a".isMatch(re"[[:alpha:][:digit:]]"))
  doAssert(raisesMsg(r"[[:abc:]]") ==
    "Invalid ascii set near position 2. " &
    "`abc` is not a valid name")
  doAssert(raisesMsg(r"[[:alnum]]") ==
    "Invalid ascii set near position 2, expected [:name:]")
  doAssert(raisesMsg(r"[[:alnum:") ==
    "Invalid ascii set near position 2, expected [:name:]")

  # treplace
  doAssert("a".replace(re"(a)", "m($1)") ==
    "m(a)")
  doAssert("a".replace(re"(a)", "m($1) m($1)") ==
    "m(a) m(a)")
  doAssert("aaa".replace(re"(a*)", "m($1)") ==
    "m(aaa)")
  doAssert("abc".replace(re"(a(b)c)", "m($1) m($2)") ==
    "m(abc) m(b)")
  doAssert("abc".replace(re"(a(b))(c)", "m($1) m($2) m($3)") ==
    "m(ab) m(b) m(c)")
  doAssert("abcabc".replace(re"(abc)*", "m($1)") ==
    "m(abcabc)")
  doAssert("abcabc".replace(re"(abc)", "m($1)") ==
    "m(abc)m(abc)")
  doAssert("abcabc".replace(re"(abc)", "m($1)") ==
    "m(abc)m(abc)")
  doAssert("abcab".replace(re"(abc)", "m($1)") ==
    "m(abc)ab")
  doAssert("abcabc".replace(re"((abc)*)", "m($1) m($2)") ==
    "m(abcabc) m(abcabc)")
  doAssert("abcabc".replace(re"((a)bc)*", "m($1) m($2)") ==
    "m(abcabc) m(aa)")
  doAssert("abc".replace(re"(b)", "m($1)") == "am(b)c")
  doAssert("abc".replace(re"d", "m($1)") == "abc")
  doAssert("abc".replace(re"(d)", "m($1)") == "abc")
  doAssert("aaa".replace(re"a", "b") == "bbb")
  doAssert("aaa".replace(re"a", "b", 1) == "baa")
  doAssert("Nim is awesome!".replace(re"(\w\B)", "$1_") ==
    "N_i_m i_s a_w_e_s_o_m_e!")

  block:
    proc by(m: RegexMatch, s: string): string =
      result = "m("
      for g in 0 ..< m.groupsCount:
        for sl in m.group(g):
          result.add(s[sl])
          result.add(',')
      result.add(')')

    doAssert("abc".replace(re"(b)", by) == "am(b,)c")
    doAssert("aaa".replace(re"(a*)", by) == "m(aaa,)")
    doAssert("aaa".replace(re"(a)*", by) == "m(a,a,a,)")

  block:
    proc removeEvenWords(m: RegexMatch, s: string): string =
      if m.group(1).len mod 2 != 0:
        result = s[m.group(0)[0]]
      else:
        result = ""

    let
      text = "Es macht Spaß, alle geraden Wörter zu entfernen!"
      expected = "macht , geraden entfernen!"
    doAssert(text.replace(re"((\w)+\s*)", removeEvenWords) == expected)
