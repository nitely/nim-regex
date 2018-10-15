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
import parseutils

import unicodedb/properties
import unicodedb/types
import unicodeplus except isUpper, isLower

type
  RegexError* = object of ValueError
  ## raised when the pattern
  ## is not a valid regex

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

proc check(cond: bool, msg: string) =
  if not cond:
    raise newException(RegexError, msg)

proc isAsciiPrintable(s: string): bool =
  result = true
  for c in s.runes:
    if c.int notin {' '.ord .. '~'.ord}:
      return false

proc check(cond: bool, msg: string, at: int, exp: string) =
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
    reLookahead,  # (?=...)
    reLookbehind,  # (?<=...)
    reNotLookahead,  # (?!...)
    reNotLookbehind,  # (?<!...)
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
    cc: UnicodeCategorySet

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

template initSetNodeImpl(result: var Node, k: NodeKind) =
  ## base node
  assert k in {reSet, reNotSet}
  result = Node(
    kind: k,
    cp: "¿".toRune,
    cps: initSet[Rune](),
    ranges: @[],
    shorthands: @[])

proc initSetNode(): Node =
  ## return a set ``Node``,
  ## parsed from an expression such as ``[a-z]``
  initSetNodeImpl(result, reSet)

proc initNotSetNode(): Node =
  ## return a negated set ``Node``,
  ## parsed from an expression such as ``[^a-z]``
  initSetNodeImpl(result, reNotSet)

proc initGroupStart(
    name: string = "",
    flags: seq[Flag] = @[],
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
  of reLookahead:
    n.cp == nxt
  of reNotLookahead:
    n.cp != nxt
  of reLookbehind:
    n.cp == r
  of reNotLookbehind:
    n.cp != r
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
    r.unicodeCategory() in n.cc
  of reNotAlphaNumAscii:
    not r.isAlphaNumAscii()
  of reNotDigitAscii:
    not r.isDigitAscii()
  of reNotWhiteSpaceAscii:
    not r.isWhiteSpaceAscii()
  of reNotUCC:
    r.unicodeCategory() notin n.cc
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
    reNotWordBoundaryAscii,
    reLookahead,
    reLookbehind,
    reNotLookahead,
    reNotLookbehind}
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

proc `$`(n: seq[Node]): string {.used.} =
  result = newStringOfCap(n.len)
  for nn in n:
    result.add($nn)

# todo: I think seqs already do this. So can use that + a few helpers
type
  ElasticSeq[T] = object
    ## a seq that can grow and shrink
    ## avoiding excessive allocations
    s: seq[T]
    pos: int

proc initElasticSeq[T](size = 16): ElasticSeq[T] =
  ElasticSeq[T](s: newSeq[T](size), pos: 0)

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
    raw: string
    s: seq[T]
    pos: int

proc newScanner[T](s: seq[T]): Scanner[T] =
  Scanner[T](s: s, pos: 0)

proc scan[T](s: seq[T]): Scanner[T] =
  newScanner(s)

proc scan(raw: string): Scanner[Rune] =
  Scanner[Rune](
    raw: raw,
    s: raw.toRunes,
    pos: 0)

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
  let startPos = sc.pos-1
  var rawCP = newString(size)
  for i in 0 ..< size:
    prettycheck(
      not sc.finished,
      ("Invalid unicode literal. " &
       "Expected $# hex digits, but found $#") %% [$size, $i])
    prettycheck(
      sc.curr.int in {
        '0'.ord .. '9'.ord,
        'a'.ord .. 'z'.ord,
        'A'.ord .. 'Z'.ord},
      ("Invalid unicode literal. " &
       "Expected hex digit, but found $#") %% $sc.curr)
    rawCP[i] = sc.next().int.char
  var cp = 0
  discard parseHex(rawCp, cp)
  prettycheck(
    cp != -1 and cp <= int32.high,
    "Invalid unicode literal. $# value is too big" %% rawCp)
  result = Rune(cp).toCharNode

proc parseUnicodeLitX(sc: Scanner[Rune]): Node =
  let startPos = sc.pos-1
  assert sc.peek == "{".toRune
  discard sc.next()
  let litEnd = sc.find("}".toRune)
  prettycheck(
    litEnd != -1,
    "Invalid unicode literal. Expected `}`")
  prettycheck(
    litEnd <= 8,
    ("Invalid unicode literal. " &
     "Expected at most 8 chars, found $#") %% $litEnd)
  result = parseUnicodeLit(sc, litEnd)
  assert sc.peek == "}".toRune
  discard sc.next()

proc parseOctalLit(sc: Scanner[Rune]): Node =
  let startPos = sc.pos
  var rawCP = newString(3)
  for i in 0 ..< 3:
    prettycheck(
      not sc.finished,
      ("Invalid octal literal. " &
       "Expected 3 octal digits, but found $#") %% $i)
    prettycheck(
      sc.curr.int in {'0'.ord .. '7'.ord},
      ("Invalid octal literal. " &
       "Expected octal digit, but found $#") %% $sc.curr)
    rawCP[i] = sc.next().int.char
  var cp = 0
  discard parseOct(rawCp, cp)
  result = Rune(cp).toCharNode

proc parseCC(s: string): UnicodeCategorySet =
  try:
    result = s.categoryMap.UnicodeCategorySet
  except ValueError:
    try:
      result = s.categorySetMap
    except ValueError:
      check(false, "Invalid unicode name?")

proc parseUnicodeNameX(sc: Scanner[Rune]): Node =
  let startPos = sc.pos-1
  assert sc.peek == "{".toRune
  discard sc.next()
  let nameEnd = sc.find("}".toRune)
  prettycheck(
    nameEnd != -1,
    "Invalid unicode name. Expected `}`")
  var name = newString(nameEnd)
  for i in 0 ..< nameEnd:
    prettycheck(
      sc.curr.int in {
        'a'.ord .. 'z'.ord,
        'A'.ord .. 'Z'.ord},
      "Invalid unicode name. " &
      "Expected chars in {'a'..'z', 'A'..'Z'}")
    name[i] = sc.next().int.char
  assert sc.peek == "}".toRune
  discard sc.next()
  prettycheck(
    name in [
      "Cn", "Lu", "Ll", "Lt", "Mn", "Mc", "Me", "Nd", "Nl",
      "No", "Zs", "Zl", "Zp", "Cc", "Cf", "Cs", "Co", "Cn",
      "Lm", "Lo", "Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po",
      "Sm", "Sc", "Sk", "So", "C", "L", "M", "N",
      "Z", "P", "S"],
    "Invalid unicode name. Found $#" %% name)
  result = Node(
    kind: reUCC,
    cp: "¿".toRune,
    cc: name.parseCC)

proc parseUnicodeName(sc: Scanner[Rune]): Node =
  let startPos = sc.pos-1
  case sc.peek
  of "{".toRune:
    result = parseUnicodeNameX(sc)
  else:
    prettycheck(
      sc.peek in [
        "C".toRune, "L".toRune, "M".toRune, "N".toRune,
        "Z".toRune, "P".toRune, "S".toRune],
      "Invalid unicode name. Found $#" %% sc.peek.toUTF8)
    result = Node(
      kind: reUCC,
      cp: "¿".toRune,
      cc: sc.next().toUTF8.parseCC)

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
  prettycheck(
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
    prettycheck(
      false,
      "Invalid ascii set. `$#` is not a valid name" %% name)

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
  result.cps.incl(cps.toSet)
  prettycheck(
    hasEnd,
    "Invalid set. Missing `]`")

proc parseRepRange(sc: Scanner[Rune]): Node =
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
    prettycheck(
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
  try:
    discard parseInt(first, firstNum)
    discard parseInt(last, lastNum)
  except OverflowError:
    prettycheck(
      false,
      "Invalid repetition range. Max value is $#" %% $int16.high)
  prettycheck(
    firstNum <= int16.high and
    lastNum <= int16.high,
    "Invalid repetition range. Max value is $#" %% $int16.high)
  # for perf reasons. This becomes a?a?a?...
  # too many parallel states
  prettycheck(
    not (lastNum - firstNum > 100),
    ("Invalid repetition range. " &
     "Expected 100 repetitions or less, " &
     "but found: $#") %% $(lastNum - firstNum))
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
    # todo: return err and show a better error msg
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
    # todo: return err and show a better error msg
    raise newException(RegexError,
      ("Invalid group flag, found -$# " &
       "but expected one of: -i, -m, -s, -U or -u") %% $r)

template checkEmptyGroup() {.dirty.} =
  prettycheck(
    peek(sc) != toRune(")"),
    "Invalid group. Empty group is not allowed")

proc parseGroupTag(sc: Scanner[Rune]): Node =
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
    prettycheck(
      sc.peek == "<".toRune,
      "Invalid group name. Missing `<`")
    discard sc.next()  # Consume "<"
    var name = newStringOfCap(75)
    for r in sc:
      if r == ">".toRune:
        break
      prettycheck(
        r.int in {
          'a'.ord .. 'z'.ord,
          'A'.ord .. 'Z'.ord,
          '0'.ord .. '9'.ord,
          '-'.ord, '_'.ord},
        ("Invalid group name. Expected char in " &
         "{'a'..'z', 'A'..'Z', '0'..'9', '-', '_'}, " &
         "but found `$#`") %% $r)
      name.add(r.int.char)
    prettycheck(
      name.len > 0,
      "Invalid group name. Name can't be empty")
    prettycheck(
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
      prettycheck(
        n.kind == reChar,
        "Invalid lookahead. A " &
        "character was expected, but " &
        "found a special symbol")
      result = Node(kind: reLookahead, cp: n.cp)
    else:
      prettycheck(
        not sc.finished,
        "Invalid lookahead. A character " &
        "was expected, but found nothing (end of string)")
      result = Node(kind: reLookahead, cp: sc.next())
    prettycheck(
      sc.peek == ")".toRune,
      "Invalid lookahead, expected closing symbol")
    discard sc.next()
  else:
    prettycheck(
      false,
      "Invalid group. Unknown group type")

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

proc skipWhiteSpace(sc: Scanner[Rune], vb: ElasticSeq[bool]): bool =
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
  let sc = expression.scan()
  for _ in sc:
    if sc.skipWhiteSpace(vb): continue
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
    names = initTable[string, int16]()
    count = 0'i16
  for i, n in expression.mpairs:
    case n.kind
    of reGroupStart:
      groups.add(i)
      if n.isCapturing:
        n.idx = count
        inc count
      if n.name.len > 0:
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

proc squash(flags: ElasticSeq[seq[Flag]]): array[Flag, bool] =
  ## Nested groups may contain flags,
  ## this will set/unset those flags
  ## in order. It should be done each time
  ## there is a group start/end
  for ff in flags:
    for f in ff:
      result[f.toggle()] = false
      result[f] = true

proc applyFlag(n: var Node, f: Flag) =
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
    #       shorthands of reSet/reNotSet (i.e: [:ascii:])
    if n.kind in {reSet, reNotSet}:
      var cps = initSet[Rune]()
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

proc applyFlags(expression: seq[Node]): seq[Node] =
  ## apply flags to each group
  result = newSeqOfCap[Node](expression.len)
  var flags = initElasticSeq[seq[Flag]](64)
  let sc = expression.scan()
  for nn in sc:
    var n = nn
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
      assert result[result.len-i].kind == reGroupStart
      result.add(result[result.len-i .. result.len-1].expandOneRepRange(n))
    of matchableKind:
      result.add(result[result.len-1 .. result.len-1].expandOneRepRange(n))
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
  result =
    (opsPA(b).associativity == asyRight and
      opsPA(b).precedence <= opsPA(a).precedence) or
    (opsPA(b).associativity == asyLeft and
      opsPA(b).precedence < opsPA(a).precedence)

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
  ends.fill(@[])  # todo: remove on nim >= 0.18.1
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
    groups: seq[Slice[int]]
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

proc clear(m: var RegexMatch) =
  if m.captures.len > 0:
    m.captures.setLen(0)
  if m.groups.len > 0:
    m.groups.setLen(0)
  m.namedGroups.clear()
  m.boundaries = 0 .. -1

iterator group*(m: RegexMatch, i: int): Slice[int] =
  ## return slices for a given group.
  ## Slices of start > end are empty
  ## matches (i.e.: ``re"(\d?)"``)
  ## and they are included same as in PCRE.
  ##
  ## .. code-block:: nim
  ##   let
  ##     expected = ["a", "b", "c"]
  ##     text = "abc"
  ##   var m: RegexMatch
  ##   doAssert text.match(re"(\w)+", m)
  ##   var i = 0
  ##   for bounds in m.group(0):
  ##     doAssert(expected[i] == text[bounds])
  ##     inc i
  ##
  for idx in m.groups[i]:
    yield m.captures[idx]

proc group*(m: RegexMatch, i: int): seq[Slice[int]] =
  ## return slices for a given group.
  ## Use the iterator version if you care about performance
  m.captures[m.groups[i]]

iterator group*(m: RegexMatch, s: string): Slice[int] =
  ## return slices for a given named group
  ##
  ## .. code-block:: nim
  ##   let
  ##     expected = ["a", "b", "c"]
  ##     text = "abc"
  ##   var m: RegexMatch
  ##   doAssert text.match(re"(?P<foo>\w)+", m)
  ##   var i = 0
  ##   for bounds in m.group("foo"):
  ##     doAssert(expected[i] == text[bounds])
  ##     inc i
  ##
  for bounds in m.group(m.namedGroups[s]):
    yield bounds

proc group*(m: RegexMatch, s: string): seq[Slice[int]] =
  ## return slices for a given named group.
  ## Use the iterator version if you care about performance
  m.group(m.namedGroups[s])

proc groupsCount*(m: RegexMatch): int =
  ## return the number of capturing groups
  ##
  ## .. code-block:: nim
  ##   block:
  ##     var m: RegexMatch
  ##     doAssert "ab".match(re"(a)(b)", m)
  ##     doAssert m.groupsCount == 2
  ##   block:
  ##     var m: RegexMatch
  ##     doAssert "ab".match(re"((ab))", m)
  ##     doAssert m.groupsCount == 2
  ##
  m.groups.len

proc stringify(pattern: Regex, nIdx: int16, visited: var set[int16]): string =
  ## NFA to string representation.
  ## For debugging purposes
  if nIdx in visited:
    result = "[...]"
    return
  visited.incl(nIdx)
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

proc `$`(pattern: Regex): string {.used.} =
  ## NFA to string representation.
  ## For debugging purposes
  var visited: set[int16] = {}
  result = pattern.stringify(pattern.states.high.int16, visited)

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
  result.groups.setLen(gc)
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
  result.captures.setLen(ci div 2)
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

iterator runesIt(s: string, start: int): (int, Rune) {.inline.} =
  ## yield current Rune and
  ## the ending index/start of next rune
  var
    i = start
    result: Rune
  while i < len(s):
    fastRuneAt(s, i, result, true)
    yield (i, result)

iterator peek(s: string, start = 0): (int, Rune, Rune) {.inline.} =
  ## iterates over unicode characters yielding
  ## next rune index, current rune and next rune
  var
    prev = invalidRune
    j = 0
  for i, r in runesIt(s, start):
    yield (j, prev, r)
    prev = r
    j = i
  yield (j, prev, invalidRune)

type
  DataSets = tuple
    visited: BitSet
    toVisit: ElasticSeq[State]
    captured: ElasticSeq[Capture]
    currStates: States
    nextStates: States

proc initDataSets(
    size: int, withCaptures = false): DataSets =
  var captured: ElasticSeq[Capture]
  result = (
    visited: initBitSet(size),
    toVisit: initElasticSeq[State](),
    captured: captured,
    currStates: initStates(size),
    nextStates: initStates(size))
  if withCaptures:
    result.captured = initElasticSeq[Capture]()
    result.captured.add(Capture())

proc clear(ds: var DataSets) =
  ## lear all data sets. This is
  ## pretty much a perf free op
  ds.visited.clear()
  ds.toVisit.clear()
  ds.currStates.clear()
  ds.nextStates.clear()
  if ds.captured.len > 0:
    ds.captured.clear()
    ds.captured.add(Capture())

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
    ds: var DataSets,
    cp: Rune,
    nxt: Rune) =
  assert ds.toVisit.len == 0
  ds.toVisit.add(state)
  while ds.toVisit.len > 0:
    let s = ds.toVisit.pop()
    if s.ni in ds.visited:
      continue
    ds.visited.incl(s.ni)
    let n = pattern.states[s.ni]
    case n.kind
    of matchableKind, reEOE:
      result.add(s)
    of assertionKind:
      if n.match(cp, nxt):
        toVisitStep(ds.toVisit, n, s.ci, s.si, s.ei)
    of reGroupStart:
      if not (n.isCapturing and
          ds.captured.len > 0):
        toVisitStep(ds.toVisit, n, s.ci, s.si, s.ei)
        continue
      ds.captured.add(Capture(
        kind: captStart,
        cpIdx: state.ei,
        prev: s.ci,
        idx: n.idx))
      toVisitStep(ds.toVisit, n, ds.captured.high, s.si, s.ei)
    of reGroupEnd:
      if not (n.isCapturing and
          ds.captured.len > 0):
        toVisitStep(ds.toVisit, n, s.ci, s.si, s.ei)
        continue
      ds.captured.add(Capture(
        kind: captEnd,
        cpIdx: state.ei - 1,
        prev: s.ci,
        idx: n.idx))
      toVisitStep(ds.toVisit, n, ds.captured.high, s.si, s.ei)
    else:
      toVisitStep(ds.toVisit, n, s.ci, s.si, s.ei)

proc stepFrom(
    result: var States,
    n: Node,
    pattern: Regex,
    ds: var DataSets,
    ci: int,
    si: int,
    ei: int,
    cp: Rune,
    nxt: Rune) {.inline.} =
  ## go to next states
  if n.outA != -1:
    let state = (ni: n.outA, ci: ci, si: si, ei: ei)
    result.step(pattern, state, ds, cp, nxt)
  if n.outB != -1:
    let state = (ni: n.outB, ci: ci, si: si, ei: ei)
    result.step(pattern, state, ds, cp, nxt)

proc setRegexMatch(
    m: var RegexMatch,
    pattern: Regex,
    ds: DataSets): bool {.inline.} =
  result = false
  for state in ds.currStates:
    if pattern.states[state.ni].kind == reEOE:
      m.clear()
      m.boundaries = state.si .. state.ei - 1
      if pattern.groupsCount > 0:
        m.populateCaptures(ds.captured, state.ci, pattern.groupsCount)
        m.namedGroups = pattern.namedGroups
      return true

proc matchImpl(
    ds: var DataSets,
    s: string,
    pattern: Regex,
    m: var RegexMatch,
    start=0): bool =
  ds.clear()
  let statesCount = pattern.states.high.int16
  for i, cp, nxt in s.peek(start):
    if cp == invalidRune:
      assert ds.currStates.len == 0
      assert i == 0
      let state = (ni: statesCount, ci: 0, si: 0, ei: 0)
      ds.currStates.step(pattern, state, ds, cp, nxt)
      continue
    if ds.currStates.len == 0:
      break
    for st in ds.currStates:
      let n = pattern.states[st.ni]
      if not n.match(cp):
        continue
      ds.visited.clear()
      ds.nextStates.stepFrom(
        n, pattern, ds, st.ci, st.si, i, cp, nxt)
    swap(ds.currStates, ds.nextStates)
    ds.nextStates.clear()
  result = setRegexMatch(m, pattern, ds)

proc match*(
    s: string,
    pattern: Regex,
    m: var RegexMatch,
    start=0): bool =
  ## return a match if the whole string
  ## matches the regular expression. This
  ## is similar to ``find(text, re"^regex$")``
  ## but has better performance
  ##
  ## .. code-block:: nim
  ##   var m: RegexMatch
  ##   doAssert "abcd".match(re"abcd", m)
  ##   doAssert(not "abcd".match(re"abc", m))
  ##
  var ds = initDataSets(
    pattern.states.len,
    pattern.groupsCount > 0)
  result = matchImpl(ds, s, pattern, m, start)

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
  var ds = initDataSets(
    pattern.states.len, false)
  let statesCount = pattern.states.high.int16
  for _, cp, nxt in s.peek:
    ds.visited.clear()
    let state = (ni: statesCount, ci: 0, si: 0, ei: 0)
    ds.currStates.step(pattern, state, ds, cp, nxt)
    if cp == invalidRune:
      continue
    for st in ds.currStates:
      let n = pattern.states[st.ni]
      result = n.kind == reEOE
      if result:
        return
      if not n.match(cp):
        continue
      ds.visited.clear()
      ds.nextStates.stepFrom(
        n, pattern, ds, 0, 0, 0, cp, nxt)
    swap(ds.currStates, ds.nextStates)
    ds.nextStates.clear()
  for state in ds.currStates:
    result = pattern.states[state.ni].kind == reEOE
    if result:
      return

proc findImpl(
    ds: var DataSets,
    s: string,
    pattern: Regex,
    m: var RegexMatch,
    start = 0): bool =
  ## search through the string looking for the first
  ## location where there is a match
  ds.clear()
  let statesCount = pattern.states.high.int16
  var si = start
  for i, cp, nxt in s.peek(start):
    # stop trying to match when there is a match
    if ds.currStates.len == 0 or
        pattern.states[ds.currStates[ds.currStates.len - 1].ni].kind != reEOE:
      ds.visited.clear()
      let state = (ni: statesCount, ci: 0, si: si, ei: si)
      ds.currStates.step(pattern, state, ds, cp, nxt)
      si = i
    # break on longest match
    if ds.currStates.len > 0 and
        pattern.states[ds.currStates[0].ni].kind == reEOE:
      break
    if cp == invalidRune:
      continue
    for st in ds.currStates:
      let n = pattern.states[st.ni]
      if n.kind == reEOE:
        # todo: put in var stateEOE?
        ds.nextStates.add(st)
        break
      if not n.match(cp):
        continue
      ds.visited.clear()
      ds.nextStates.stepFrom(
        n, pattern, ds, st.ci, st.si, i, cp, nxt)
    swap(ds.currStates, ds.nextStates)
    ds.nextStates.clear()
  result = setRegexMatch(m, pattern, ds)

proc find*(
    s: string,
    pattern: Regex,
    m: var RegexMatch,
    start = 0): bool =
  ## search through the string looking for the first
  ## location where there is a match
  ##
  ## .. code-block:: nim
  ##   var m: RegexMatch
  ##   doAssert "abcd".find(re"bc", m)
  ##   doAssert(not "abcd".find(re"de", m))
  ##   doAssert "2222".find(re"(22)*", m) and
  ##     m.group(0) == @[0 .. 1, 2 .. 3]
  ##
  var ds = initDataSets(
    pattern.states.len,
    pattern.groupsCount > 0)
  result = findImpl(ds, s, pattern, m, start)

template runeIncAt(s: string, n: var int) =
  ## increment ``n`` up to
  ## next rune's index
  if n == s.len:
    inc n
  else:
    inc(n, runeLenAt(s, n))

iterator findAllImpl(
    s: string,
    pattern: Regex,
    start = 0): RegexMatch {.inline.} =
  var
    m = RegexMatch()
    ds = initDataSets(
      pattern.states.len,
      pattern.groupsCount > 0)
    i = start
  while i < s.len:
    if not findImpl(ds, s, pattern, m, i): break
    let b = m.boundaries
    yield m
    if b.b == i or b.a > b.b: s.runeIncAt(i)
    else: i = b.b + 1

iterator findAll*(
    s: string,
    pattern: Regex,
    start = 0): RegexMatch {.inline.} =
  ## search through the string and
  ## return each match. Empty matches
  ## (start > end) are included
  ##
  ## .. code-block:: nim
  ##   var i = 0
  ##   let
  ##     expected = [1 .. 2, 4 .. 5]
  ##     text = "abcabc"
  ##   for m in findAll(text, re"bc"):
  ##     doAssert text[m.boundaries] == "bc"
  ##     doAssert m.boundaries == expected[i]
  ##     inc i
  ##
  for m in findAllImpl(s, pattern, start):
    yield m

proc findAll*(s: string, pattern: Regex, start = 0): seq[RegexMatch] =
  ## search through the string and
  ## return each match. Empty matches
  ## (start > end) are included
  result = newSeqOfCap[RegexMatch](s.len)
  for slc in findAll(s, pattern, start):
    result.add(slc)

proc findAndCaptureAll*(s: string, pattern: Regex): seq[string] =
  ## search through the string and
  ## return a seq with captures.
  ##
  ## .. code-block:: nim
  ##   let
  ##     expected = @["1", "2", "3", "4", "5"]
  ##     res = findAndCaptureAll("a1b2c3d4e5", re"\d")
  ##   doAssert(res == expected)
  ##
  result = @[]
  for m in s.findAll(pattern):
    result.add(s[m.boundaries])

proc matchEndImpl(
    ds: var DataSets,
    s: string,
    pattern: Regex,
    start = 0): int =
  ## return end index of the longest match.
  ## Return ``-1`` when there is no match.
  ## Pattern is anchored to the start of the string
  assert ds.captured.len == 0
  ds.clear()
  result = -1
  let statesCount = pattern.states.high.int16
  for i, cp, nxt in s.peek(start):
    if cp == invalidRune:
      assert ds.currStates.len == 0
      assert i == 0
      let state = (ni: statesCount, ci: 0, si: 0, ei: 0)
      ds.currStates.step(pattern, state, ds, cp, nxt)
      continue
    if ds.currStates.len == 0:
      break
    if pattern.states[ds.currStates[0].ni].kind == reEOE:
      break
    for st in ds.currStates:
      let n = pattern.states[st.ni]
      if n.kind == reEOE:
        ds.nextStates.add(st)
        break
      if not n.match(cp):
        continue
      ds.visited.clear()
      ds.nextStates.stepFrom(
        n, pattern, ds, st.ci, st.si, i, cp, nxt)
    swap(ds.currStates, ds.nextStates)
    ds.nextStates.clear()
  for state in ds.currStates:
    if pattern.states[state.ni].kind == reEOE:
      result = state.ei
      return

proc matchEnd(s: string, pattern: Regex, start = 0): int =
  ## return end index of the longest match.
  ## Return ``-1`` when there is no match.
  ## Pattern is anchored to the start of the string
  var ds = initDataSets(pattern.states.len, false)
  result = matchEndImpl(ds, s, pattern, start)

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
  var
    ds = initDataSets(sep.states.len, false)
    first = 0
    last = 0
    n = 0
  while last <= s.len:
    first = last
    while last <= s.len:
      n = matchEndImpl(ds, s, sep, last)
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

# This is the same as the split proc,
# except it's based on find instead of matchEnd,
# 'cause we need the captures an I'm not about to change
# matchEnd implementation. Also, this *might* be faster,
# but I need to profile it to prove it
proc splitIncl*(s: string, sep: Regex): seq[string] =
  ## return not matched substrings, including captured groups
  ##
  ## .. code-block:: nim
  ##   doAssert splitIncl("a,b", re"(,)") ==
  ##     @["a", ",", "b"]
  ##
  result = @[]
  var
    m = RegexMatch()
    ds = initDataSets(sep.states.len, true)
    first = 0
    last = 0
    n = 0
  while last <= s.len:
    first = last
    m.reset()
    if findImpl(ds, s, sep, m, last):
      if m.boundaries.a > m.boundaries.b:
        n = last
        s.runeIncAt(n)
        last = n
      else:
        n = m.boundaries.b+1
        last = m.boundaries.a
    else:
      n = s.len+1
      last = n
    result.add(substr(s, first, last-1))
    for g in 0 ..< m.groupsCount:
      for sl in m.group(g):
        result.add(substr(s, sl.a, sl.b))
    last = n

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
  result = false
  var
    m = RegexMatch()
    ds = initDataSets(pattern.states.len, false)
    i = 0
  while i < s.len:
    result = matchImpl(ds, s, pattern, m, i)
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
  for m in findAllImpl(s, pattern):
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
  for m in findAllImpl(s, pattern):
    result.addsubstr(s, i, m.boundaries.a - 1)
    result.add(by(m, s))
    i = m.boundaries.b + 1
    inc j
    if limit > 0 and j == limit: break
  result.addsubstr(s, i)

proc toPattern*(s: string): Regex {.raises: [RegexError].} =
  ## Parse and compile a regular expression.
  ## Deprecated: use directly `re` instead which works both at RT and CT.
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

when defined(forceRegexAtRuntime):
  proc re*(s: string): Regex {.inline.} =
    toPattern(s)
else:
  proc re*(s: static string): Regex {.inline.} =
    ## Parse and compile a regular expression at compile-time
    const pattern = toPattern(s)
    pattern

  proc re*(s: string): Regex {.inline.} =
    ## Parse and compile a regular expression at run-time
    toPattern(s)

when isMainModule:
  proc toAtoms(s: string): string =
    s.parse.greediness.applyFlags.expandRepRange.joinAtoms.`$`

  proc toNfaStr(s: string): string =
    let pattern = Regex(
      states: s.parse.greediness.applyFlags.expandRepRange.joinAtoms.rpn.nfa)
    result = $pattern

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

  # https://github.com/nitely/nim-regex/issues/24
  block:
    const a1 = re"foo"
    const a2 = re("foo")
    let pattern = "foo"
    let a3 = re(pattern)
