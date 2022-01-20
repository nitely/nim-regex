##[
A library for parsing, compiling, and executing
regular expressions. The match time is linear
in the length of the text and
the regular expression. So, it can handle
input from untrusted users. The syntax is similar to PCRE
but lacks a few features that can not be implemented
while keeping the space/time complexity guarantees,
ex: backreferences.

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

Lookaround Assertions
#####################

.. code-block::
  (?=regex)   A positive lookahead assertion
  (?!regex)   A negative lookahead assertion
  (?<=regex)  A positive lookbehind assertion
  (?<!regex)  A negative lookbehind assertion

Any regex expression is a valid lookaround; groups
are captured as well. Beware, lookarounds containing
repetitions (``*``, ``+``, and ``{n,}``) may run in
polynomial time.

Examples
********

Multiple captures
#################

Unlike most regex engines, this library supports capturing
all repetitions. Most other libraries return only the last
capture. The caveat is even non-repeated groups or
characters are returned as a list of captures instead of
a single capture.

.. code-block:: nim
    :test:
    let text = "nim c --styleCheck:hint --colors:off regex.nim"
    var m: RegexMatch
    if match(text, re"nim c (?:--(\w+:\w+) *)+ (\w+).nim", m):
      doAssert m.group(0, text) == @["styleCheck:hint", "colors:off"]
      doAssert m.group(1, text) == @["regex"]
    else:
      doAssert false, "no match"

Verbose Mode
############

Verbose mode `(?x)` makes regexes more readable by allowing
comments and multi-lines within the regular expression
itself. The caveat is spaces and pound signs must be
scaped to be matched.

.. code-block:: nim
    :test:
    const exp = re"""(?x)
    \#   # the hashtag
    \w+  # hashtag words
    """
    let text = "#NimLang"
    doAssert match(text, exp) 

Find All
########

The `findAll` function will find all boundaries
and captures that match the regular expression.

.. code-block:: nim
    :test:
    let text = """
    The Continental's email list:
    john_wick@continental.com
    winston@continental.com
    ms_perkins@continental.com
    """
    var matches = newSeq[string]()
    var captures = newSeq[string]()
    for m in findAll(text, re"(\w+)@\w+\.\w+"):
      matches.add text[m.boundaries]
      captures.add m.group(0, text)
    doAssert matches == @[
      "john_wick@continental.com",
      "winston@continental.com",
      "ms_perkins@continental.com"
    ]
    doAssert captures == @["john_wick", "winston", "ms_perkins"]

Match Macro
###########

The ``match`` macro is sometimes more convenient, and
faster than the function version. It will run a full
match on the whole string, similar to `^regex$`.

A ``matches: seq[string]`` variable is injected into
the scope, and it contains the submatches for every capture group.

.. code-block:: nim
    :test:
    var matched = false
    let text = "[my link](https://example.com)"
    match text, rex"\[([^\]]+)\]\((https?://[^)]+)\)":
      doAssert matches == @["my link", "https://example.com"]
      matched = true
    doAssert matched

]##

import std/tables
import std/sequtils
import std/unicode
from std/strutils import addf

import ./regex/types
import ./regex/common
import ./regex/compiler
import ./regex/nfatype
import ./regex/nfafindall
import ./regex/nfamatch
when not defined(noRegexOpt):
  import ./regex/litopt

const canUseMacro = (NimMajor, NimMinor) >= (1, 1)

when canUseMacro:
  import ./regex/nfamacro
  export RegexLit

export
  Regex,
  RegexMatch,
  RegexError

func re*(
  s: string
): Regex {.raises: [RegexError].} =
  ## Parse and compile a regular expression at run-time
  runnableExamples:
    let abcx = re"abc\w"
    let abcx2 = re(r"abc\w")
    let pat = r"abc\w"
    let abcx3 = re(pat)
  reImpl(s)

# Workaround Nim/issues/14515
# ideally only `re(string): Regex`
# would be needed (without static)
when not defined(forceRegexAtRuntime):
  func re*(
    s: static string
  ): static[Regex] {.inline.} =
    ## Parse and compile a regular expression at compile-time
    when canUseMacro:  # VM dies on Nim < 1.1
      reCt(s)
    else:
      reImpl(s)

func toPattern*(
  s: string
): Regex {.raises: [RegexError], deprecated: "Use `re` instead".} =
  re(s)

when canUseMacro:
  func rex*(s: string): RegexLit =
    ## Raw regex literal string
    RegexLit s

iterator group*(m: RegexMatch, i: int): Slice[int] {.inline, raises: [].} =
  ## return slices for a given group.
  ## Slices of start > end are empty
  ## matches (i.e.: ``re"(\d?)"``)
  ## and they are included same as in PCRE.
  runnableExamples:
    let text = "abc"
    var m: RegexMatch
    doAssert text.match(re"(\w)+", m)
    var captures = newSeq[string]()
    for bounds in m.group(0):
      captures.add text[bounds]
    doAssert captures == @["a", "b", "c"]

  for capt in m.captures[i]:
    yield capt

func group*(m: RegexMatch, i: int): seq[Slice[int]] {.inline, raises: [].} =
  ## return slices for a given group.
  ## Use the iterator version if you care about performance
  m.captures[i]

func group*(
  m: RegexMatch, i: int, text: string
): seq[string] {.inline, raises: [].} =
  ## return seq of captured text by group number `i`
  runnableExamples:
    let text = "hello beautiful world"
    var m: RegexMatch
    doAssert text.match(re"(hello) (?:([^\s]+)\s?)+", m)
    doAssert m.group(0, text) == @["hello"]
    doAssert m.group(1, text) == @["beautiful", "world"]

  result = newSeq[string]()
  for bounds in m.group i:
    result.add text[bounds]

func groupFirstCapture*(
  m: RegexMatch, i: int, text: string
): string {.inline, raises: [].} =
  ## return first capture for a given capturing group
  runnableExamples:
    let text = "hello beautiful world"
    var m: RegexMatch
    doAssert text.match(re"(hello) (?:([^\s]+)\s?)+", m)
    doAssert m.groupFirstCapture(0, text) == "hello"
    doAssert m.groupFirstCapture(1, text) == "beautiful"

  for bounds in m.group i:
    return text[bounds]

func groupLastCapture*(
  m: RegexMatch, i: int, text: string
): string {.inline, raises: [].} =
  ## return last capture for a given capturing group
  runnableExamples:
    let text = "hello beautiful world"
    var m: RegexMatch
    doAssert text.match(re"(hello) (?:([^\s]+)\s?)+", m)
    doAssert m.groupLastCapture(0, text) == "hello"
    doAssert m.groupLastCapture(1, text) == "world"

  var b = 0 .. -1
  for bounds in m.group i:
    b = bounds
  result = text[b]

iterator group*(
  m: RegexMatch, s: string
): Slice[int] {.inline, raises: [KeyError].} =
  ## return slices for a given named group
  runnableExamples:
    let text = "abc"
    var m: RegexMatch
    doAssert text.match(re"(?P<foo>\w)+", m)
    var captures = newSeq[string]()
    for bounds in m.group("foo"):
      captures.add text[bounds]
    doAssert captures == @["a", "b", "c"]

  for bounds in m.group(m.namedGroups[s]):
    yield bounds

func group*(
  m: RegexMatch, s: string
): seq[Slice[int]] {.inline, raises: [KeyError].} =
  ## return slices for a given named group.
  ## Use the iterator version if you care about performance
  m.group m.namedGroups[s]

func group*(
  m: RegexMatch,
  groupName: string,
  text: string
): seq[string] {.inline, raises: [KeyError].} =
  ## return seq of captured text by group `groupName`
  runnableExamples:
    let text = "hello beautiful world"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    doAssert m.group("greet", text) == @["hello"]
    doAssert m.group("who", text) == @["beautiful", "world"]

  result = newSeq[string]()
  for bounds in m.group(groupName):
    result.add text[bounds]

func groupFirstCapture*(
  m: RegexMatch,
  groupName: string,
  text: string
): string {.inline, raises: [KeyError].} =
  ## return first capture for a given capturing group
  runnableExamples:
    let text = "hello beautiful world"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    doAssert m.groupFirstCapture("greet", text) == "hello"
    doAssert m.groupFirstCapture("who", text) == "beautiful"

  let captures = m.group(groupName, text)
  if captures.len > 0:
    return captures[0]
  else:
    return "" 

func groupLastCapture*(
  m: RegexMatch,
  groupName: string,
  text: string
): string {.inline, raises: [KeyError].} =
  ## return last capture for a given capturing group
  runnableExamples:
    let text = "hello beautiful world"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?:(?P<who>[^\s]+)\s?)+", m)
    doAssert m.groupLastCapture("greet", text) == "hello"
    doAssert m.groupLastCapture("who", text) == "world"

  let captures = m.group(groupName, text)
  if captures.len > 0:
    return captures[captures.len-1]
  else:
    return ""

func groupsCount*(m: RegexMatch): int {.inline, raises: [].} =
  ## return the number of capturing groups
  runnableExamples:
    var m: RegexMatch
    doAssert "ab".match(re"(a)(b)", m)
    doAssert m.groupsCount == 2

  m.captures.len

func groupNames*(m: RegexMatch): seq[string] {.inline, raises: [].} =
  ## return the names of capturing groups.
  runnableExamples:
    let text = "hello world"
    var m: RegexMatch
    doAssert text.match(re"(?P<greet>hello) (?P<who>world)", m)
    doAssert m.groupNames() == @["greet", "who"]

  result = toSeq(m.namedGroups.keys)

when canUseMacro:
  macro match*(
    text: string,
    regex: RegexLit,
    body: untyped
  ): untyped =
    ## return a match if the whole string
    ## matches the regular expression. This is
    ## similar to the ``match`` function, but
    ## faster. Notice it requires a raw regex *literal*
    ## string as second parameter; the regex must be
    ## known at compile time, and cannot be a var/let/const
    ##
    ## A ``matches: seq[string]`` variable is injected into
    ## the scope, and it contains the submatches for every capture
    ## group. If a group is repeated (ex: `(\\w)+`), it will
    ## contain the last capture for that group.
    ##
    ## Note: Only available in Nim +1.1
    runnableExamples:
      match "abc", rex"(a(b)c)":
        doAssert matches == @["abc", "b"]

    matchImpl(text, regex, body)

func match*(
  s: string,
  pattern: Regex,
  m: var RegexMatch,
  start = 0
): bool {.inline, raises: [].} =
  ## return a match if the whole string
  ## matches the regular expression. This
  ## is similar to ``find(text, re"^regex$", m)``
  ## but has better performance
  runnableExamples:
    var m: RegexMatch
    doAssert "abcd".match(re"abcd", m)
    doAssert not "abcd".match(re"abc", m)

  result = matchImpl(s, pattern, m, start)

func match*(s: string, pattern: Regex): bool {.inline, raises: [].} =
  var m: RegexMatch
  result = matchImpl(s, pattern, m)

template runeIncAt(s: string, n: var int) =
  ## increment ``n`` up to
  ## next rune's index
  if n < s.len:
    inc(n, runeLenAt(s, n))
  else:
    n = s.len+1

when defined(noRegexOpt):
  template findSomeOptTpl(s, pattern, ms, i): untyped =
    findSomeImpl(s, pattern, ms, i)
else:
  template findSomeOptTpl(s, pattern, ms, i): untyped =
    if pattern.litOpt.canOpt:
      findSomeOptImpl(s, pattern, ms, i)
    else:
      findSomeImpl(s, pattern, ms, i)

iterator findAll*(
  s: string,
  pattern: Regex,
  start = 0
): RegexMatch {.inline, raises: [].} =
  ## search through the string and
  ## return each match. Empty matches
  ## (start > end) are included
  runnableExamples:
    let text = "abcabc"
    var bounds = newSeq[Slice[int]]()
    var found = newSeq[string]()
    for m in findAll(text, re"bc"):
      bounds.add m.boundaries
      found.add text[m.boundaries]
    doAssert bounds == @[1 .. 2, 4 .. 5]
    doAssert found == @["bc", "bc"]

  var i = start
  var i2 = start-1
  var m: RegexMatch
  var ms: RegexMatches
  while i <= len(s):
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, pattern, ms, i)
    #debugEcho i
    if i < 0: break
    for mi in ms:
      fillMatchImpl(m, mi, ms, pattern)
      yield m
    if i == len(s):
      break

func findAll*(
  s: string,
  pattern: Regex,
  start = 0
): seq[RegexMatch] {.inline, raises: [].} =
  for m in findAll(s, pattern, start):
    result.add m

iterator findAllBounds*(
  s: string,
  pattern: Regex,
  start = 0
): Slice[int] {.inline, raises: [].} =
  ## search through the string and
  ## return each match. Empty matches
  ## (start > end) are included
  runnableExamples:
    let text = "abcabc"
    var bounds = newSeq[Slice[int]]()
    for bd in findAllBounds(text, re"bc"):
      bounds.add bd
    doAssert bounds == @[1 .. 2, 4 .. 5]

  var i = start
  var i2 = start-1
  var ms: RegexMatches
  while i <= len(s):
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, pattern, ms, i)
    #debugEcho i
    if i < 0: break
    for ab in ms.bounds:
      yield ab
    if i == len(s):
      break

func findAllBounds*(
  s: string,
  pattern: Regex,
  start = 0
): seq[Slice[int]] {.inline, raises: [].} =
  for m in findAllBounds(s, pattern, start):
    result.add m

func findAndCaptureAll*(
  s: string, pattern: Regex
): seq[string] {.inline, raises: [].} =
  ## search through the string and
  ## return a seq with captures.
  runnableExamples:
    doAssert findAndCaptureAll("a1b2c3d4e5", re"\d") ==
      @["1", "2", "3", "4", "5"]

  for m in s.findAll(pattern):
    result.add s[m.boundaries]

# XXX find shortest match; disable captures
func contains*(s: string, pattern: Regex): bool {.inline, raises: [].} =
  ##  search for the pattern anywhere in the string
  runnableExamples:
    doAssert re"bc" in "abcd"
    doAssert re"(23)+" in "23232"
    doAssert re"^(23)+$" notin "23232"

  for _ in findAllBounds(s, pattern):
    return true
  return false

func find*(
  s: string,
  pattern: Regex,
  m: var RegexMatch,
  start = 0
): bool {.inline, raises: [].} =
  ## search through the string looking for the first
  ## location where there is a match
  runnableExamples:
    var m: RegexMatch
    doAssert "abcd".find(re"bc", m) and
      m.boundaries == 1 .. 2
    doAssert not "abcd".find(re"de", m)
    doAssert "2222".find(re"(22)*", m) and
      m.group(0) == @[0 .. 1, 2 .. 3]

  m.clear()
  for m2 in findAll(s, pattern, start):
    m.captures.add m2.captures
    m.namedGroups = m2.namedGroups
    m.boundaries = m2.boundaries
    return true
  return false

iterator split*(s: string, sep: Regex): string {.inline, raises: [].} =
  ## return not matched substrings
  runnableExamples:
    var found = newSeq[string]()
    for s in split("11a22Ϊ33Ⓐ44弢55", re"\d+"):
      found.add s
    doAssert found == @["", "a", "Ϊ", "Ⓐ", "弢", ""]

  var
    first, last, i = 0
    i2 = -1
    done = false
    ms: RegexMatches
  while not done:
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, sep, ms, i)
    done = i < 0 or i >= len(s)
    if done: ms.dummyMatch(s.len)
    for ab in ms.bounds:
      last = ab.a
      if ab.a > 0 or ab.a <= ab.b:  # skip first empty match
        yield substr(s, first, last-1)
      first = ab.b+1

func split*(s: string, sep: Regex): seq[string] {.inline, raises: [].} =
  ## return not matched substrings
  runnableExamples:
    doAssert split("11a22Ϊ33Ⓐ44弢55", re"\d+") ==
      @["", "a", "Ϊ", "Ⓐ", "弢", ""]

  for w in split(s, sep):
    result.add w

func splitIncl*(s: string, sep: Regex): seq[string] {.inline, raises: [].} =
  ## return not matched substrings, including captured groups
  runnableExamples:
    let
      parts = splitIncl("a,b", re"(,)")
      expected = @["a", ",", "b"]
    doAssert parts == expected

  template ab: untyped = m.boundaries
  var
    first, last, i = 0
    i2 = -1
    done = false
    m: RegexMatch
    ms: RegexMatches
  while not done:
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, sep, ms, i)
    done = i < 0 or i >= len(s)
    if done: ms.dummyMatch(s.len)
    for mi in ms:
      fillMatchImpl(m, mi, ms, sep)
      last = ab.a
      if ab.a > 0 or ab.a <= ab.b:  # skip first empty match
        result.add substr(s, first, last-1)
        for g in 0 ..< m.groupsCount:
          for sl in m.group(g):
            result.add substr(s, sl.a, sl.b)
      first = ab.b+1

func startsWith*(
  s: string, pattern: Regex, start = 0
): bool {.inline, raises: [].} =
  ## return whether the string
  ## starts with the pattern or not
  runnableExamples:
    doAssert "abc".startsWith(re"\w")
    doAssert not "abc".startsWith(re"\d")

  startsWithImpl(s, pattern, start)

# XXX use findAll and check last match bounds
func endsWith*(s: string, pattern: Regex): bool {.inline, raises: [].} =
  ## return whether the string
  ## ends with the pattern or not
  runnableExamples:
    doAssert "abc".endsWith(re"\w")
    doAssert not "abc".endsWith(re"\d")

  result = false
  var
    m: RegexMatch
    i = 0
  while i < s.len:
    result = match(s, pattern, m, i)
    if result: return
    s.runeIncAt(i)

func flatCaptures(
  result: var seq[string],
  m: RegexMatch,
  s: string
) {.inline, raises: [].} =
  ## Concat capture repetitions
  var i, n = 0
  for g in 0 ..< m.groupsCount:
    n = 0
    for sl in m.group(g):
      if sl.a <= sl.b:
        n += sl.b - sl.a + 1
    i = 0
    result[g].setLen(n)
    for sl in m.group(g):
      for c in sl:
        result[g][i] = s[c]
        inc i
    assert i == n

func addsubstr(
  result: var string, s: string, first, last: int
) {.inline, raises: [].} =
  let
    first = max(first, 0)
    last = min(last, s.high)
  if first > last: return
  let n = result.len
  result.setLen(result.len + last-first+1)
  # XXX copyMem
  var j = 0
  for i in first .. last:
    result[n + j] = s[i]
    inc j

func addsubstr(
  result: var string, s: string, first: int
) {.inline, raises: [].} =
  addsubstr(result, s, first, s.high)

func replace*(
  s: string,
  pattern: Regex,
  by: string,
  limit = 0
): string {.inline, raises: [ValueError].} =
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
  runnableExamples:
    doAssert "aaa".replace(re"a", "b", 1) == "baa"
    doAssert "abc".replace(re"(a(b)c)", "m($1) m($2)") ==
      "m(abc) m(b)"
    doAssert "Nim is awesome!".replace(re"(\w\B)", "$1_") ==
      "N_i_m i_s a_w_e_s_o_m_e!"

  result = ""
  var
    i, j = 0
    capts = newSeq[string](pattern.groupsCount)
  for m in findAll(s, pattern):
    result.addsubstr(s, i, m.boundaries.a-1)
    flatCaptures(capts, m, s)
    if capts.len > 0:
      result.addf(by, capts)
    else:
      result.add(by)
    i = m.boundaries.b+1
    inc j
    if limit > 0 and j == limit: break
  result.addsubstr(s, i)

when not defined(nimHasEffectsOf):
  {.pragma: effectsOf.}

func replace*(
  s: string,
  pattern: Regex,
  by: proc (m: RegexMatch, s: string): string,
  limit = 0
): string {.inline, raises: [], effectsOf: by.} =
  ## Replace matched substrings.
  ##
  ## If ``limit`` is given, at most ``limit``
  ## replacements are done. ``limit`` of 0
  ## means there is no limit
  runnableExamples:
    proc removeEvenWords(m: RegexMatch, s: string): string =
      result = ""
      if m.group(1).len mod 2 != 0:
        result = s[m.group(0)[0]]
    
    let text = "Es macht Spaß, alle geraden Wörter zu entfernen!"
    doAssert text.replace(re"((\w)+\s*)", removeEvenWords) ==
      "macht , geraden entfernen!"

  result = ""
  var i, j = 0
  for m in findAll(s, pattern):
    result.addsubstr(s, i, m.boundaries.a-1)
    result.add by(m, s)
    i = m.boundaries.b+1
    inc j
    if limit > 0 and j == limit: break
  result.addsubstr(s, i)

func isInitialized*(re: Regex): bool {.inline, raises: [].} =
  ## Check whether the regex has been initialized
  runnableExamples:
    var re: Regex
    doAssert not re.isInitialized
    re = re"foo"
    doAssert re.isInitialized

  re.nfa.s.len > 0

func escapeRe*(s: string): string {.raises: [].} =
  ## Escape special regex characters in ``s``
  ## so that it can be matched verbatim
  # The special char list is the same as re.escape
  # in Python 3.7
  #
  # utf-8 ascii code-points cannot be part of multi-byte
  # code-points, so we can read/match byte by byte
  result = ""
  for c in s:
    case c
    of ' ', '#', '$', '&', '(',
        ')', '*', '+', '-', '.',
        '?', '[', '\\', ']', '^',
        '{', '|', '}', '~', char(9),
        char(10), char(11), char(12), char(13):
      result.add '\\'
      result.add c
    else:
      result.add c

proc toString(
  pattern: Regex,
  nIdx: int16,
  visited: var set[int16]
): string {.used.} =
  ## NFA to string representation.
  ## For debugging purposes
  # XXX zero-match transitions are missing
  if nIdx in visited:
    result = "[...]"
    return
  visited.incl(nIdx)
  let n = pattern.nfa.s[nIdx]
  result = "["
  result.add($n)
  for nn in n.next:
    result.add(", ")
    result.add(pattern.toString(nn, visited))
  result.add("]")

proc toString(pattern: Regex): string {.used.} =
  ## NFA to string representation.
  ## For debugging purposes
  var visited: set[int16]
  result = pattern.toString(0, visited)

when isMainModule:
  import ./regex/parser
  import ./regex/exptransformation
  import ./regex/dotgraph

  func toAtoms(s: string): string =
    var groups: GroupsCapture
    let atoms = s
      .parse
      .toAtoms(groups)
    result = atoms.s.toString

  func toNfaStr(s: string): string =
    result = re(s).toString

  doAssert toAtoms(r"a(b|c)*d") == r"a~(b|c)*~d"
  doAssert toAtoms(r"abc") == r"a~b~c"
  doAssert toAtoms(r"(abc|def)") == r"(a~b~c|d~e~f)"
  doAssert toAtoms(r"(abc|def)*xyz") == r"(a~b~c|d~e~f)*~x~y~z"
  doAssert toAtoms(r"a*b") == r"a*~b"
  doAssert toAtoms(r"(a)b") == r"(a)~b"
  doAssert toAtoms(r"(a)(b)") == r"(a)~(b)"
  doAssert toAtoms(r"\y") == r"y"
  doAssert toAtoms(r"a\*b") == r"a~*~b"
  doAssert toAtoms(r"\(a\)") == r"(~a~)"
  doAssert toAtoms(r"\w") == r"\w"
  doAssert toAtoms(r"\d") == r"\d"
  doAssert toAtoms(r"[a-z]") == r"[a-z]"
  doAssert toAtoms(r"[aa-zz]") == r"[aza-z]"
  doAssert toAtoms(r"[aa\-zz]") == r"[-az]"
  doAssert toAtoms(r"[^a]") == r"[^a]"
  doAssert toAtoms(r"(a*)*") != toAtoms(r"a*")
  doAssert toAtoms(r"(a*|b*)*") != toAtoms(r"(a|b)*")
  doAssert toAtoms(r"(a*b*)*") != toAtoms(r"(a|b)*")
  doAssert toAtoms(r"(a*|b*)") != toAtoms(r"(a|b)*")
  doAssert toAtoms(r"(a(b)){2}") == r"(a~(b))~(a~(b))"

  # trepetition_range_expand
  doAssert r"a{0}".toNfaStr == r"a".toNfaStr
  doAssert r"a{0}b".toNfaStr == r"ab".toNfaStr
  doAssert r"a{1}".toNfaStr == r"a".toNfaStr
  doAssert r"a{10}".toNfaStr == r"aaaaaaaaaa".toNfaStr
  doAssert r"a{1,}".toNfaStr == r"aa*".toNfaStr
  doAssert r"a{10,}".toNfaStr == r"aaaaaaaaaaa*".toNfaStr
  doAssert r"a{10,10}".toNfaStr == r"aaaaaaaaaa".toNfaStr
  doAssert r"a{0,0}".toNfaStr == r"a".toNfaStr
  doAssert r"a{1,2}".toNfaStr == r"aa?".toNfaStr
  doAssert r"a{2,4}".toNfaStr == r"aaa?a?".toNfaStr
  doAssert r"a{0,10}".toNfaStr == r"a?a?a?a?a?a?a?a?a?a?".toNfaStr
  doAssert r"(a(b)){2}".toNfaStr == r"(a(b))(a(b))".toNfaStr

  # tascii_set
  doAssert r"[[:alnum:]]".toAtoms == "[[0-9a-zA-Z]]"
  doAssert r"[[:^alnum:]]".toAtoms == "[[^0-9a-zA-Z]]"
  doAssert r"[[:alpha:]]".toAtoms == "[[a-zA-Z]]"
  doAssert r"[[:ascii:]]".toAtoms == "[[\x00-\x7F]]"
  doAssert r"[[:blank:]]".toAtoms == "[[\t ]]"
  doAssert r"[[:cntrl:]]".toAtoms == "[[\x7F\x00-\x1F]]"
  doAssert r"[[:digit:]]".toAtoms == "[[0-9]]"
  doAssert r"[[:graph:]]".toAtoms == "[[!-~]]"
  doAssert r"[[:lower:]]".toAtoms == "[[a-z]]"
  doAssert r"[[:print:]]".toAtoms == "[[ -~]]"
  doAssert r"[[:punct:]]".toAtoms == "[[!-/:-@[-`{-~]]"
  doAssert r"[[:space:]]".toAtoms == "[[\t\n\v\f\r ]]"
  doAssert r"[[:upper:]]".toAtoms == "[[A-Z]]"
  doAssert r"[[:word:]]".toAtoms == "[[_0-9a-zA-Z]]"
  doAssert r"[[:xdigit:]]".toAtoms == "[[0-9a-fA-F]]"
  doAssert r"[[:alpha:][:digit:]]".toAtoms == "[[a-zA-Z][0-9]]"

  var m: RegexMatch
  #doAssert match("abc", re(r"abc", {reAscii}), m)
  doAssert match("abc", re"abc", m)
  doAssert match("ab", re"a(b|c)", m)
  doAssert match("ac", re"a(b|c)", m)
  doAssert(not match("ad", re"a(b|c)", m))
  doAssert match("ab", re"(ab)*", m)
  doAssert match("abab", re"(ab)*", m)
  doAssert(not match("ababc", re"(ab)*", m))
  doAssert(not match("a", re"(ab)*", m))
  doAssert match("ab", re"(ab)+", m)
  doAssert match("abab", re"(ab)+", m)
  doAssert(not match("ababc", re"(ab)+", m))
  doAssert(not match("a", re"(ab)+", m))
  doAssert match("aa", re"\b\b\baa\b\b\b", m)
  doAssert(not match("cac", re"c\ba\bc", m))
  doAssert match("abc", re"[abc]+", m)
  doAssert match("abc", re"[\w]+", m)
  doAssert match("弢弢弢", re"[\w]+", m)
  doAssert(not match("abc", re"[\d]+", m))
  doAssert match("123", re"[\d]+", m)
  doAssert match("abc$%&", re".+", m)
  doAssert(not match("abc$%&\L", re"(.+)", m))
  doAssert(not match("abc$%&\L", re".+", m))
  doAssert(not match("弢", re"\W", m))
  doAssert match("$%&", re"\W+", m)
  doAssert match("abc123", re"[^\W]+", m)

  doAssert match("aabcd", re"(aa)bcd", m) and
    m.captures == @[@[0 .. 1]]
  doAssert match("aabc", re"(aa)(bc)", m) and
    m.captures == @[@[0 .. 1], @[2 .. 3]]
  doAssert match("ab", re"a(b|c)", m) and
    m.captures == @[@[1 .. 1]]
  doAssert match("ab", re"(ab)*", m) and
    m.captures == @[@[0 .. 1]]
  doAssert match("abab", re"(ab)*", m) and
    m.captures == @[@[0 .. 1, 2 .. 3]]
  doAssert match("ab", re"((a))b", m) and
    m.captures == @[@[0 .. 0], @[0 .. 0]]
  doAssert match("c", re"((ab)*)c", m) and
    m.captures == @[@[0 .. -1], @[]]
  doAssert match("aab", re"((a)*b)", m) and
    m.captures == @[@[0 .. 2], @[0 .. 0, 1 .. 1]]
  doAssert match("abbbbcccc", re"a(b|c)*", m) and
    m.captures == @[@[1 .. 1, 2 .. 2, 3 .. 3, 4 .. 4, 5 .. 5, 6 .. 6, 7 .. 7, 8 .. 8]]
  doAssert match("ab", re"(a*)(b*)", m) and
    m.captures == @[@[0 .. 0], @[1 .. 1]]
  doAssert match("ab", re"(a)*(b)*", m) and
    m.captures == @[@[0 .. 0], @[1 .. 1]]
  doAssert match("ab", re"(a)*b*", m) and
    m.captures == @[@[0 .. 0]]
  doAssert match("abbb", re"((a(b)*)*(b)*)", m) and
    m.captures == @[@[0 .. 3], @[0 .. 3], @[1 .. 1, 2 .. 2, 3 .. 3], @[]]
  doAssert match("aa", re"(a)+", m) and
    m.captures == @[@[0 .. 0, 1 .. 1]]
  doAssert match("abab", re"(ab)+", m) and
    m.captures == @[@[0 .. 1, 2 .. 3]]
  doAssert match("a", re"(a)?", m) and
    m.captures == @[@[0 .. 0]]
  doAssert match("ab", re"(ab)?", m) and
    m.captures == @[@[0 .. 1]]
  doAssert match("aaabbbaaa", re"(a*|b*)*", m) and
    m.captures == @[@[0 .. 2, 3 .. 5, 6 .. 8, 9 .. 8]]
  doAssert match("abab", re"(a(b))*", m) and
    m.captures == @[@[0 .. 1, 2 .. 3], @[1 .. 1, 3 .. 3]]
  doAssert match("aaanasdnasd", re"((a)*n?(asd)*)*", m) and
    m.captures == @[@[0 .. 6, 7 .. 10, 11 .. 10], @[0 .. 0, 1 .. 1, 2 .. 2], @[4 .. 6, 8 .. 10]]
  doAssert match("aaanasdnasd", re"((a)*n?(asd))*", m) and
    m.captures == @[@[0 .. 6, 7 .. 10], @[0 .. 0, 1 .. 1, 2 .. 2], @[4 .. 6, 8 .. 10]]
  doAssert match("abd", re"((ab)c)|((ab)d)", m) and
    m.captures == @[@[], @[], @[0 .. 2], @[0 .. 1]]
  doAssert match("aaa", re"(a*)", m) and
    m.captures == @[@[0 .. 2]]
  doAssert match("aaaa", re"(a*)(a*)", m) and
    m.captures == @[@[0 .. 3], @[4 .. 3]]
  doAssert match("aaaa", re"(a*?)(a*?)", m) and
    m.captures == @[@[0 .. -1], @[0 .. 3]]
  doAssert match("aaaa", re"(a)*(a)", m) and
    m.captures == @[@[0 .. 0, 1 .. 1, 2 .. 2], @[3 .. 3]]
  
  doAssert match("abc", re"abc")
  doAssert(not match("abc", re"abd"))
  doAssert(not match("abc", re"ab"))
  doAssert(not match("abc", re"b"))
  doAssert(not match("abc", re"c"))

  doAssert re"bc" in "abcd"
  doAssert re"(23)+" in "23232"
  doAssert re"^(23)+$" notin "23232"
  doAssert re"\w" in "弢"
  #doAssert re(r"\w", {reAscii}) notin "弢"
  #doAssert re(r"\w", {reAscii}) in "a"

  doAssert "abcd".find(re"bc", m)
  doAssert(not "abcd".find(re"de", m))
  #doAssert "%ab%".find(re(r"\w{2}", {reAscii}), m)
  doAssert "%弢弢%".find(re"\w{2}", m)
  #doAssert(not "%弢弢%".find(re(r"\w{2}", {reAscii}), m)
  doAssert(
    "2222".find(re"(22)*", m) and
    m.group(0) == @[0 .. 1, 2 .. 3])
  doAssert(
    "11222211".find(re"(22)+", m) and
    m.group(0) == @[2 .. 3, 4 .. 5])
  
  doAssert match("650-253-0001", re"[0-9]+-[0-9]+-[0-9]+", m)
  doAssert(not match("abc-253-0001", re"[0-9]+-[0-9]+-[0-9]+", m))
  doAssert(not match("650-253", re"[0-9]+-[0-9]+-[0-9]+", m))
  doAssert(not match("650-253-0001-abc", re"[0-9]+-[0-9]+-[0-9]+", m))
  doAssert match("650-253-0001", re"[0-9]+..*", m)
  doAssert(not match("abc-253-0001", re"[0-9]+..*", m))
  doAssert(not match("6", re"[0-9]+..*", m))

  doAssert match("abcabcabc", re"(?:(?:abc)){3}")
  doAssert match("abcabcabc", re"((abc)){3}")

  doAssert match("", re"|")
  doAssert match("a", re"a|")
  doAssert match("", re"a|")
  doAssert(not match("b", re"a|"))
  doAssert match("b", re"|b")
  doAssert match("", re"|b")
  doAssert(not match("a", re"|b"))
  doAssert match("", re"(|)")
  doAssert match("a", re"(a|)")
  doAssert match("", re"(a|)")
  doAssert(not match("b", re"(a|)"))
  doAssert match("b", re"(|b)")
  doAssert match("", re"(|b)")
  doAssert(not match("a", re"(|b)"))
  doAssert match("", re"||")

  doAssert match(" ", re"(?x)     (?-x) ")
  doAssert match("aa", re"((?x)   a    )a")
  doAssert match(" ", re"((?x)     ) ")
  doAssert match(" ", re"(?x:(?x)     ) ")
  doAssert match(" ", re"((?x:)) ")
  doAssert match("A", re"(?xi)     a")
  doAssert(not match("A", re"((?xi))     a"))
  doAssert(not match("A", re"(?xi:(?xi)     )a"))

  doAssert graph(re"^a+$") == """digraph graphname {
    0 [label="q0";color=blue];
    1 [label="q1";color=black];
    2 [label="q2";color=blue];
    0 -> 1 [label="a, {^}, i=0"];
    1 -> 1 [label="a, i=0"];1 -> 2 [label="{eoe}, {$}, i=1"];
}
"""

  # subset of tests.nim
  proc raisesMsg(pattern: string): string =
    try:
      discard re(pattern)
    except RegexError:
      result = getCurrentExceptionMsg()

  template test(body: untyped): untyped =
    static:
      (proc() = body)()
    (proc() = body)()

  test:
    var m: RegexMatch
    doAssert match("ac", re"a(b|c)", m)
    doAssert(not match("ad", re"a(b|c)", m))
    doAssert match("ab", re"(ab)*", m)
    doAssert match("abab", re"(ab)*", m)
    doAssert(not match("ababc", re"(ab)*", m))
    doAssert(not match("a", re"(ab)*", m))
    doAssert match("abab", re"(ab)*", m) and
      m.captures == @[@[0 .. 1, 2 .. 3]]
    doAssert match("bbaa aa", re"([\w ]*?)(\baa\b)", m) and
      m.captures == @[@[0 .. 4], @[5 .. 6]]
    doAssert re"bc" in "abcd"
    doAssert re"(23)+" in "23232"
    doAssert re"^(23)+$" notin "23232"
    doAssert re"\w" in "弢"
    doAssert "2222".find(re"(22)*", m) and
      m.group(0) == @[0 .. 1, 2 .. 3]
    doAssert raisesMsg(r"[a-\w]") ==
      "Invalid set range. Range can't contain " &
      "a character-class or assertion\n" &
      "[a-\\w]\n" &
      "   ^"
    doAssert "a,b".splitIncl(re"(,)") == @["a", ",", "b"]
    doAssert "abcabc".replace(re"(abc)", "m($1)") ==
      "m(abc)m(abc)"
    const ip = re"""(?x)
    \b
    ((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}
    (25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
    \b
    """
    doAssert match("127.0.0.1", ip)
    doAssert(not match("127.0.0.999", ip))
    doAssert "abcd".find(re"bc", m) and
      m.boundaries == 1 .. 2
    doAssert "bcd".find(re"bc", m) and
      m.boundaries == 0 .. 1
    doAssert "bc".find(re"bc", m) and
      m.boundaries == 0 .. 1
    doAssert "#foo://#".find(re"[\w]+://", m) and
      m.boundaries == 1 .. 6
    doAssert findAllBounds("abcd", re"bc") == @[1 .. 2]
    doAssert findAllBounds("bcd", re"bc") == @[0 .. 1]
    doAssert findAllBounds("bc", re"bc") == @[0 .. 1]
    doAssert findAllBounds("#foo://#", re"[\w]+://") == @[1 .. 6]
    doAssert findAllBounds("abc\nabc\na", re"(?m)^a") ==
      @[0 .. 0, 4 .. 4, 8 .. 8]
    doAssert match("ab", re"a(?=b)\w")
    doAssert(not match("ab", re"a(?=x)\w"))
    doAssert match("ab", re"\w(?<=a)b")
    doAssert(not match("ab", re"\w(?<=x)b"))
    doAssert match("aaab", re".*(?<=^(\w*?)(\w*?)(\w??)$)", m) and
      m.captures == @[@[0 .. 3], @[4 .. 3], @[4 .. 3]]
    doAssert match("aaab", re".*(?<=^(\w*?)(\w*)(\w??)$)", m) and
      m.captures == @[@[0 .. -1], @[0 .. 3], @[4 .. 3]]
    doAssert match("aaab", re"(\w*)(\w??)", m) and
      m.captures == @[@[0 .. 3], @[4 .. 3]]
    doAssert match("aaab", re".*(?<=^(\w*)(\w??)$)", m) and
      m.captures == @[@[0 .. 3], @[4 .. 3]]
    doAssert match("aaab", re".*(?<=^(\w*)(\w?)$)", m) and
      m.captures == @[@[0 .. 2], @[3 .. 3]]
    doAssert match("aaab", re".*(?<=^(\d)\w+|(\w)\w{3}|(\w)\w{3}|(\w)\w+$)", m) and
      m.captures == @[@[], @[0 .. 0], @[], @[]]
    doAssert match("aaab", re".*(?<=^(\d)\w+|(\d)\w{3}|(\w)\w{3}|(\w)\w+$)", m) and
      m.captures == @[@[], @[], @[0 .. 0], @[]]
    doAssert match("aaab", re".*(?<=^(\d)\w+|(\d)\w{3}|(\d)\w{3}|(\w)\w+$)", m) and
      m.captures == @[@[], @[], @[], @[0 .. 0]]
    doAssert(not match("ab", re"\w(?<=a(?=b(?<=a)))b"))
    doAssert(not match("ab", re"\w(?<=a(?<=a(?=b(?<=a))))b"))
    doAssert match("ab", re"\w(?<=a(?=b(?<=b)))b")
    doAssert match("ab", re"\w(?<=a(?<=a(?=b(?<=b))))b")
    doAssert findAllBounds(r"1abab", re"(?<=\d\w*)ab") ==
      @[1 .. 2, 3 .. 4]
    doAssert findAllBounds(r"abab", re"(?<=\d\w*)ab").len == 0
    doAssert findAllBounds(r"abab1", re"ab(?=\w*\d)") ==
      @[0 .. 1, 2 .. 3]
    doAssert findAllBounds(r"abab", re"ab(?=\w*\d)").len == 0
    doAssert match("aΪ", re"a(?=Ϊ)\w")
    doAssert match("Ϊb", re"Ϊ(?=b)\w")
    doAssert match("弢Ⓐ", re"弢(?=Ⓐ)\w")
    doAssert match("aΪ", re"\w(?<=a)Ϊ")
    doAssert match("Ϊb", re"\w(?<=Ϊ)b")
    doAssert match("弢Ⓐ", re"\w(?<=弢)Ⓐ")
    block:  # Follows Nim re's behaviour
      doAssert match("abc", re"(?<=a)bc", m, start = 1)
      doAssert(not match("abc", re"(?<=x)bc", m, start = 1))
      doAssert(not match("abc", re"^bc", m, start = 1))
    doAssert startsWith("abc", re"b", start = 1)
    doAssert startsWith("abc", re"(?<=a)b", start = 1)
    doAssert startsWith("abc", re"b", start = 1)
    doAssert(not startsWith("abc", re"(?<=x)b", start = 1))
    doAssert(not startsWith("abc", re"^b", start = 1))
    doAssert(not match("ab", re"ab(?=x)"))
    doAssert(not match("ab", re"(?<=x)ab"))
    doAssert match("ab", re"(?<=^)ab")
    doAssert match("ab", re"ab(?=$)")
    doAssert match("abcdefg", re"\w+(?<=(ab)(?=(cd)))\w+", m) and
      m.captures == @[@[0 .. 1], @[2 .. 3]]
    doAssert match("abcdefg", re"\w+(?<=(ab)(?=(cd)(?<=(cd))))\w+", m) and
      m.captures == @[@[0 .. 1], @[2 .. 3], @[2 .. 3]]
    when canUseMacro:
      block:
        var m = false
        var matches: seq[string]
        match "abc", rex"(\w+)":
          doAssert matches == @["abc"]
          m = true
        doAssert m
        doAssert matches.len == 0
      block:
        var m = false
        match "abc", rex"(\w)+":
          doAssert matches == @["c"]
          m = true
        doAssert m
      block:
        var m = false
        match "abc", rex"(a(b)c)":
          doAssert matches == @["abc", "b"]
          m = true
        doAssert m
      block:
        var m = false
        match "x", rex"y":
          m = true
        doAssert not m
        match "y", rex"y":
          m = true
        doAssert m
      block:
        template myRegex: untyped =
          rex"""(?x)
            abc  # verbose mode
          """
        var m = false
        match "abc", myRegex:
          m = true
        doAssert m
      block:
        var m = false
        var txt = "abc"
        match txt, rex"(\w)+":
          m = true
        doAssert m
      block:
        var matched = false
        let text = "[my link](https://example.com)"
        match text, rex"\[([a-z ]*)\]\((https?://[^)]+)\)":
          doAssert matches == @["my link", "https://example.com"]
          matched = true
        doAssert matched
      block:
        var matched = false
        match "abcdefg", rex"\w+(?<=(ab)(?=(cd)))\w+":
          doAssert matches == @["ab", "cd"]
          matched = true
        doAssert matched

  echo "ok regex.nim"
