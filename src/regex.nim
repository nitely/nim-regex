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
  x  ignore whitespace and allow line comments (starting with #)

All flags are disabled by default unless stated otherwise

The regex accepts passing a set of flags:

.. code-block::
  regexCaseless        same as (?i)
  regexMultiline       same as (?m)
  regexDotAll          same as (?s)
  regexUngreedy        same as (?U)
  regexAscii           same as (?-u)
  regexExtended        same as (?x)
  regexArbitraryBytes  treat both the regex and the input text
                       as arbitrary byte sequences

.. note::
  Read the `Match arbitrary bytes <#examples-match-arbitrary-bytes>`_ section
  to learn more about the arbitrary bytes mode and ascii mode

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

Match
#####

The ``match`` function match a text from start to end, similar to ``^regex$``.
This means the whole text needs to match the regex for this function to return ``true``.

.. code-block:: nim
    :test:
    let text = "nim c --styleCheck:hint --colors:off regex.nim"
    var m: RegexMatch2
    if match(text, re2"nim c (?:--(\w+:\w+) *)+ (\w+).nim", m):
      doAssert text[m.group(0)] == "colors:off"
      doAssert text[m.group(1)] == "regex"
    else:
      doAssert false, "no match"

Captures
########

Like most other regex engines, this library only
captures the last repetition in a repeated group
(``*``, ``+``, ``{n}``). Note how in the previous example
both ``styleCheck:hint`` and ``colors:off`` are matched in
the same group but only the last captured match (``colors:off``)
is returned.

To check if a capture group did match you can use ``reNonCapture``.
For example ``doAssert m.group(0) != reNonCapture``. This is useful
to disambiguate empty captures and non-matched captures. Since both return
an empty string when slicing the text.

The space complexity for captures is ``O(regex_len * groups_count)``,
and so it can be used to match untrusted text.

Find
####

The ``find`` function will find the first piece of text that
match a given regex.

.. code-block:: nim
    :test:
    let text = """
    The Continental's email list:
    john_wick@continental.com
    winston@continental.com
    ms_perkins@continental.com
    """
    var match = ""
    var capture = ""
    var m: RegexMatch2
    if find(text, re2"(\w+)@\w+\.\w+", m):
      match = text[m.boundaries]
      capture = text[m.group(0)]
    doAssert match == "john_wick@continental.com"
    doAssert capture == "john_wick"

Find All
########

The `findAll` function will find all pieces of text
that match a given regex, returning their boundaries
and captures/submatches.

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
    for m in findAll(text, re2"(\w+)@\w+\.\w+"):
      matches.add text[m.boundaries]
      captures.add text[m.group(0)]
    doAssert matches == @[
      "john_wick@continental.com",
      "winston@continental.com",
      "ms_perkins@continental.com"
    ]
    doAssert captures == @["john_wick", "winston", "ms_perkins"]

Verbose Mode
############

Verbose mode `(?x)` makes regexes more readable by allowing
comments and multi-lines within the regular expression
itself. The caveat is spaces and pound signs must be
scaped to be matched.

.. code-block:: nim
    :test:
    const exp = re2"""(?x)
    \#   # the hashtag
    \w+  # hashtag words
    """
    let text = "#NimLang"
    doAssert match(text, exp) 

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

Invalid UTF-8 input text
########################

UTF-8 validation on the input text is only done in debug mode for perf reasons.
The behaviour on invalid UTF-8 input (i.e: malformed, corrupted, truncated, etc)
when compiling in release/danger mode is currently undefined,
and it will likely result in an internal AssertionDefect or some other error.

What can be done about this is validating the input text to avoid
passing invalid input to the match function.

.. code-block:: nim
    :test:
    import unicode
    # good input text
    doAssert validateUtf8("abc") == -1
    # bad input text
    doAssert validateUtf8("\xf8\xa1\xa1\xa1\xa1") != -1

Note at the time of writting this, Nim's ``validateUtf8``
`is not strict enough <https://github.com/nim-lang/Nim/issues/19333>`_
and so you are better off using `nim-unicodeplus's <https://github.com/nitely/nim-unicodeplus>`_
``verifyUtf8`` function.

Match arbitrary bytes
#####################

Setting the ``regexArbitraryBytes`` flag will
treat both the regex and the input text as byte sequences.
This flag makes ascii mode the default.

.. note::
    Do not confuse this with ascii mode. Setting the regex to
    ascii mode ``(?-u)`` alone is not enough to match arbitrary bytes,
    since both the input and regex will be treated as UTF8.

.. code-block:: nim
    :test:
    const flags = {regexArbitraryBytes}
    doAssert match("\xff", re2(r"\xff", flags))
    doAssert match("\xf8\xa1\xa1\xa1\xa1", re2(r".+", flags))

Beware of (un)expected behaviour when mixin UTF-8 characters.

.. code-block:: nim
    :test:
    const flags = {regexArbitraryBytes}
    doAssert match("Ⓐ", re2(r"Ⓐ", flags))
    doAssert match("ⒶⒶ", re2(r"(Ⓐ)+", flags))
    doAssert not match("ⒶⒶ", re2(r"Ⓐ+", flags))  # ???

The last line in the above example won't match because the
regex is parsed as a byte sequence. The ``Ⓐ`` character
is composed of multiple bytes (``\xe2\x92\xb6``),
and only the last byte is affected by the ``+`` operator.

Compile the regex at compile time
#################################

Passing a regex literal or assigning it to a ``const``
will compile the regex at compile time. Errors in the
expression will be catched at compile time this way.

Do not confuse the regex compilation with the matching operation.
The following examples do the matching at runtime. But matching
at compile-time is supported as well.

.. code-block:: nim
    :test:
    let text = "abc"
    block:
      const rexp = re2".+"
      doAssert match(text, rexp)
    block:
      doAssert match(text, re2".+")
    block:
      func myFn(s: string, exp: static string) =
        const rexp = re2(exp)
        doAssert match(s, rexp)
      myFn(text, r".+")

Using a ``const`` can avoid confusion when passing flags:

.. code-block:: nim
    :test:
    let text = "abc"
    block:
      const rexp = re2(r".+", {regexDotAll})
      doAssert match(text, rexp)
    block:
      doAssert match(text, re2(r".+", {regexDotAll}))
    block:
      # this will compile the expression at runtime
      # because flags is a var, avoid it!
      let flags = {regexDotAll}
      doAssert match(text, re2(r".+", flags))

Compile the regex at runtime
############################

.. note::
    Consider `compiling the regex at compile-time <#examples-compile-the-regex-at-compile-time>`_
    whenever possible.

Most of the time compiling the regex at runtime can be avoided,
and it should be avoided. Nim has really good compile-time
capabilities like reading files, constructing strings,
and so on. However, it cannot be helped in cases where
the regex is passed to the program at runtime (from terminal input,
network, or text files).

To compile the regex at runtime pass the regex expression as a ``var/let``.

.. code-block:: nim
    :test:
    let text = "abc"
    block:
      var rexp = r".+"
      doAssert match(text, re2(rexp))
    block:
      let rexp = r".+"
      doAssert match(text, re2(rexp))
    block:
      func myFn(s: string, exp: string) =
        doAssert match(s, re2(exp))
      myFn(text, r".+")

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
import ./regex/nfafindall2
import ./regex/nfamatch
import ./regex/nfamatch2
when not defined(noRegexOpt):
  import ./regex/litopt

import ./regex/nfamacro
export RegexLit

export
  Regex,
  Regex2,
  RegexMatch,
  RegexMatch2,
  RegexFlag,
  RegexFlags,
  RegexError

#
# NEW APIs
#

const reNonCapture* = nonCapture

template debugCheckUtf8(s: untyped): untyped =
  ## This is for input strings. Regex are already checked.
  ## On release/danger the behaviour on invalid utf-8 input
  ## is undefined
  when not defined(release):
    assert(verifyUtf8(s) == -1, "Invalid utf-8 input")

template debugCheckUtf8(s: string, pat: Regex2): untyped =
  when not defined(release):
    assert(
      regexArbitraryBytes in pat.toRegex.flags or
      verifyUtf8(s) == -1,
      "Invalid utf-8 input"
    )

func rex*(s: string): RegexLit =
  ## Raw regex literal string
  RegexLit s

func re2*(s: string, flags: RegexFlags = {}): Regex2 {.raises: [RegexError].} =
  ## Parse and compile a regular expression at run-time
  runnableExamples:
    let abcx = re2"abc\w"
    let abcx2 = re2(r"abc\w")
    let pat = r"abc\w"
    let abcx3 = re2(pat)

  toRegex2 reImpl(s, flags)

# Workaround Nim/issues/14515
# ideally only `re2(string): Regex`
# would be needed (without static)
when not defined(forceRegexAtRuntime):
  func re2*(
    s: static string,
    flags: static RegexFlags = {}
  ): static[Regex2] {.inline.} =
    ## Parse and compile a regular expression at compile-time
    toRegex2 reCt(s, flags)

func group*(m: RegexMatch2, i: int): Slice[int] {.inline, raises: [].} =
  ## return slice for a given group.
  ## Slice of start > end are empty
  ## matches (i.e.: ``re2"(\d?)"``)
  ## and they are included same as in PCRE.
  runnableExamples:
    let text = "abc"
    var m: RegexMatch2
    doAssert text.match(re2"(\w)+", m)
    doAssert text[m.group(0)] == "c"

  m.captures[i]

func group*(
  m: RegexMatch2, s: string
): Slice[int] {.inline, raises: [KeyError].} =
  ## return slices for a given named group
  runnableExamples:
    let text = "abc"
    var m: RegexMatch2
    doAssert text.match(re2"(?P<foo>\w)+", m)
    doAssert text[m.group("foo")] == "c"

  m.group m.namedGroups[s]

func groupsCount*(m: RegexMatch2): int {.inline, raises: [].} =
  ## return the number of capturing groups
  runnableExamples:
    var m: RegexMatch2
    doAssert "ab".match(re2"(a)(b)", m)
    doAssert m.groupsCount == 2

  m.captures.len

func groupNames*(m: RegexMatch2): seq[string] {.inline, raises: [].} =
  ## return the names of capturing groups.
  runnableExamples:
    let text = "hello world"
    var m: RegexMatch2
    doAssert text.match(re2"(?P<greet>hello) (?P<who>world)", m)
    doAssert m.groupNames == @["greet", "who"]

  result = toSeq(m.namedGroups.keys)

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
  pattern: Regex2,
  m: var RegexMatch2,
  start = 0
): bool {.inline, raises: [].} =
  ## return a match if the whole string
  ## matches the regular expression. This
  ## is similar to ``find(text, re"^regex$", m)``
  ## but has better performance
  runnableExamples:
    var m: RegexMatch2
    doAssert "abcd".match(re2"abcd", m)
    doAssert not "abcd".match(re2"abc", m)

  debugCheckUtf8(s, pattern)
  result = matchImpl(s, pattern.toRegex, m, start)

func match*(s: string, pattern: Regex2): bool {.inline, raises: [].} =
  debugCheckUtf8(s, pattern)
  var m: RegexMatch2
  result = matchImpl(s, pattern.toRegex, m)

when defined(noRegexOpt):
  template findSomeOptTpl(s, pattern, ms, i): untyped =
    findSomeImpl(s, pattern, ms, i)
  template findSomeOptTpl(s, pattern, ms, i, flags): untyped =
    findSomeImpl(s, pattern, ms, i, flags)
else:
  template findSomeOptTpl(s, pattern, ms, i): untyped =
    if pattern.litOpt.canOpt:
      findSomeOptImpl(s, pattern, ms, i)
    else:
      findSomeImpl(s, pattern, ms, i)
  template findSomeOptTpl(s, pattern, ms, i, flags): untyped =
    if pattern.litOpt.canOpt:
      findSomeOptImpl(s, pattern, ms, i, flags)
    else:
      findSomeImpl(s, pattern, ms, i, flags)

iterator findAll*(
  s: string,
  pattern: Regex2,
  start = 0
): RegexMatch2 {.inline, raises: [].} =
  ## search through the string and
  ## return each match. Empty matches
  ## (start > end) are included
  runnableExamples:
    let text = "abcabc"
    var bounds = newSeq[Slice[int]]()
    var found = newSeq[string]()
    for m in findAll(text, re2"bc"):
      bounds.add m.boundaries
      found.add text[m.boundaries]
    doAssert bounds == @[1 .. 2, 4 .. 5]
    doAssert found == @["bc", "bc"]

  debugCheckUtf8(s, pattern)
  var i = start
  var i2 = start-1
  var m: RegexMatch2
  var ms: RegexMatches2
  while i <= len(s):
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, pattern.toRegex, ms, i)
    #debugEcho i
    if i < 0: break
    for mi in ms:
      fillMatchImpl(m, mi, ms, pattern.toRegex)
      yield m
    if i == len(s):
      break

func findAll*(
  s: string,
  pattern: Regex2,
  start = 0
): seq[RegexMatch2] {.inline, raises: [].} =
  for m in findAll(s, pattern, start):
    result.add m

iterator findAllBounds*(
  s: string,
  pattern: Regex2,
  start = 0
): Slice[int] {.inline, raises: [].} =
  ## search through the string and
  ## return each match. Empty matches
  ## (start > end) are included
  runnableExamples:
    let text = "abcabc"
    var bounds = newSeq[Slice[int]]()
    for bd in findAllBounds(text, re2"bc"):
      bounds.add bd
    doAssert bounds == @[1 .. 2, 4 .. 5]

  debugCheckUtf8(s, pattern)
  var i = start
  var i2 = start-1
  var ms: RegexMatches2
  let flags = {mfNoCaptures}
  while i <= len(s):
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, pattern.toRegex, ms, i, flags)
    #debugEcho i
    if i < 0: break
    for ab in ms.bounds:
      yield ab
    if i == len(s):
      break

func findAllBounds*(
  s: string,
  pattern: Regex2,
  start = 0
): seq[Slice[int]] {.inline, raises: [].} =
  for m in findAllBounds(s, pattern, start):
    result.add m

func find*(
  s: string,
  pattern: Regex2,
  m: var RegexMatch2,
  start = 0
): bool {.inline, raises: [].} =
  ## search through the string looking for the first
  ## location where there is a match
  runnableExamples:
    var m: RegexMatch2
    doAssert "abcd".find(re2"bc", m) and
      m.boundaries == 1 .. 2
    doAssert not "abcd".find(re2"de", m)
    doAssert "2222".find(re2"(22)*", m) and
      m.group(0) == 2 .. 3

  m.clear()
  for m2 in findAll(s, pattern, start):
    m.captures = m2.captures
    m.namedGroups = m2.namedGroups
    m.boundaries = m2.boundaries
    return true
  return false

# XXX find shortest match; disable captures
func contains*(s: string, pattern: Regex2): bool {.inline, raises: [].} =
  runnableExamples:
    doAssert re2"bc" in "abcd"
    doAssert re2"(23)+" in "23232"
    doAssert re2"^(23)+$" notin "23232"

  for _ in findAllBounds(s, pattern):
    return true
  return false

iterator split*(s: string, sep: Regex2): string {.inline, raises: [].} =
  ## return not matched substrings
  runnableExamples:
    var found = newSeq[string]()
    for s in split("11a22Ϊ33Ⓐ44弢55", re2"\d+"):
      found.add s
    doAssert found == @["", "a", "Ϊ", "Ⓐ", "弢", ""]

  debugCheckUtf8(s, sep)
  var
    first, last, i = 0
    i2 = -1
    done = false
    ms: RegexMatches2
    flags = {mfNoCaptures}
  while not done:
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, sep.toRegex, ms, i, flags)
    done = i < 0 or i >= len(s)
    if done: ms.dummyMatch(s.len)
    for ab in ms.bounds:
      last = ab.a
      if ab.a > 0 or ab.a <= ab.b:  # skip first empty match
        yield substr(s, first, last-1)
      first = ab.b+1

func split*(s: string, sep: Regex2): seq[string] {.inline, raises: [].} =
  ## return not matched substrings
  runnableExamples:
    doAssert split("11a22Ϊ33Ⓐ44弢55", re2"\d+") ==
      @["", "a", "Ϊ", "Ⓐ", "弢", ""]

  for w in split(s, sep):
    result.add w

func splitIncl*(s: string, sep: Regex2): seq[string] {.inline, raises: [].} =
  ## return not matched substrings, including captured groups
  runnableExamples:
    let
      parts = splitIncl("a,b", re2"(,)")
      expected = @["a", ",", "b"]
    doAssert parts == expected

  template ab: untyped = m.boundaries
  debugCheckUtf8(s, sep)
  var
    first, last, i = 0
    i2 = -1
    done = false
    m: RegexMatch2
    ms: RegexMatches2
  while not done:
    doAssert(i > i2); i2 = i
    i = findSomeOptTpl(s, sep.toRegex, ms, i)
    done = i < 0 or i >= len(s)
    if done: ms.dummyMatch(s.len)
    for mi in ms:
      fillMatchImpl(m, mi, ms, sep.toRegex)
      last = ab.a
      if ab.a > 0 or ab.a <= ab.b:  # skip first empty match
        result.add substr(s, first, last-1)
        for g in 0 ..< m.groupsCount:
          if m.group(g) != nonCapture:
            result.add substr(s, m.group(g).a, m.group(g).b)
      first = ab.b+1

func startsWith*(
  s: string,
  pattern: Regex2,
  start = 0
): bool {.inline, raises: [].} =
  ## return whether the string
  ## starts with the pattern or not
  runnableExamples:
    doAssert "abc".startsWith(re2"\w")
    doAssert not "abc".startsWith(re2"\d")

  debugCheckUtf8(s, pattern)
  startsWithImpl2(s, pattern.toRegex, start)

func endsWith*(s: string, pattern: Regex2): bool {.inline, raises: [].} =
  ## return whether the string
  ## ends with the pattern or not
  runnableExamples:
    doAssert "abc".endsWith(re2"\w")
    doAssert not "abc".endsWith(re2"\d")

  var lastBounds = -2 .. -2
  for bounds in findAllBounds(s, pattern):
    lastBounds = bounds
  return lastBounds.b == s.len-1 

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
  pattern: Regex2,
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
    doAssert "aaa".replace(re2"a", "b", 1) == "baa"
    doAssert "abc".replace(re2"(a(b)c)", "m($1) m($2)") ==
      "m(abc) m(b)"
    doAssert "Nim is awesome!".replace(re2"(\w\B)", "$1_") ==
      "N_i_m i_s a_w_e_s_o_m_e!"

  debugCheckUtf8(s, pattern)
  result = newStringOfCap(s.len)
  var
    i, j = 0
    capts = newSeqOfCap[string](pattern.toRegex.groupsCount)
  for m in findAll(s, pattern):
    result.addsubstr(s, i, m.boundaries.a-1)
    capts.setLen 0
    for c in m.captures:
      capts.add s[c]  # XXX openArray
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
  pattern: Regex2,
  by: proc (m: RegexMatch2, s: string): string,
  limit = 0
): string {.inline, raises: [], effectsOf: by.} =
  ## Replace matched substrings.
  ##
  ## If ``limit`` is given, at most ``limit``
  ## replacements are done. ``limit`` of 0
  ## means there is no limit
  runnableExamples:
    proc removeStars(m: RegexMatch2, s: string): string =
      result = s[m.group(0)]
      if result == "*":
        result = ""
    let text = "**this is a test**"
    doAssert text.replace(re2"(\*)", removeStars) == "this is a test"

  debugCheckUtf8(s, pattern)
  result = newStringOfCap(s.len)
  var i, j = 0
  for m in findAll(s, pattern):
    result.addsubstr(s, i, m.boundaries.a-1)
    result.add by(m, s)
    i = m.boundaries.b+1
    inc j
    if limit > 0 and j == limit: break
  result.addsubstr(s, i)

func isInitialized*(re: Regex2): bool {.inline, raises: [].} =
  ## Check whether the regex has been initialized
  runnableExamples:
    var re: Regex2
    doAssert not re.isInitialized
    re = re2"foo"
    doAssert re.isInitialized

  toRegex(re).nfa.s.len > 0

func escapeRe*(s: string): string {.raises: [].} =
  ## Escape special regex characters in ``s``
  ## so that it can be matched verbatim
  # The special char list is the same as re.escape
  # in Python 3.7
  #
  # utf-8 ascii code-points cannot be part of multi-byte
  # code-points, so we can read/match byte by byte
  debugCheckUtf8 s
  result = newStringOfCap(s.len)
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
  pattern: Regex2,
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
  let n = toRegex(pattern).nfa.s[nIdx]
  result = "["
  result.add($n)
  for nn in n.next:
    result.add(", ")
    result.add(pattern.toString(nn, visited))
  result.add("]")

proc toString(pattern: Regex2): string {.used.} =
  ## NFA to string representation.
  ## For debugging purposes
  var visited: set[int16]
  result = pattern.toString(0, visited)

#
#
# OLD DEPRECATED APIs
#
#

# below deprecated funcs call each other,
# so turn warnings
{.push warning[Deprecated]: off.}

func re*(
  s: string
): Regex {.raises: [RegexError], deprecated: "use re2(string) instead".} =
  reImpl(s)

when not defined(forceRegexAtRuntime):
  func re*(
    s: static string
  ): static[Regex] {.inline, deprecated: "use re2(static string) instead".} =
    reCt(s)

func toPattern*(
  s: string
): Regex {.raises: [RegexError], deprecated: "Use `re2(string)` instead".} =
  re(s)

iterator group*(m: RegexMatch, i: int): Slice[int] {.inline, raises: [], deprecated.} =
  for capt in m.captures[i]:
    yield capt

func group*(m: RegexMatch, i: int): seq[Slice[int]] {.inline, raises: [], deprecated: "use group(RegexMatch2, int)".} =
  m.captures[i]

func group*(
  m: RegexMatch, i: int, text: string
): seq[string] {.inline, raises: [], deprecated.} =
  result = newSeq[string]()
  for bounds in m.group i:
    result.add text[bounds]

func groupFirstCapture*(
  m: RegexMatch, i: int, text: string
): string {.inline, raises: [], deprecated.} =
  for bounds in m.group i:
    return text[bounds]

func groupLastCapture*(
  m: RegexMatch, i: int, text: string
): string {.inline, raises: [], deprecated: "use group(RegexMatch2, int) instead".} =
  var b = 0 .. -1
  for bounds in m.group i:
    b = bounds
  result = text[b]

iterator group*(
  m: RegexMatch, s: string
): Slice[int] {.inline, raises: [KeyError], deprecated.} =
  for bounds in m.group(m.namedGroups[s]):
    yield bounds

func group*(
  m: RegexMatch, s: string
): seq[Slice[int]] {.inline, raises: [KeyError], deprecated: "use group(RegexMatch2, string)".} =
  m.group m.namedGroups[s]

func group*(
  m: RegexMatch,
  groupName: string,
  text: string
): seq[string] {.inline, raises: [KeyError], deprecated.} =
  result = newSeq[string]()
  for bounds in m.group(groupName):
    result.add text[bounds]

func groupFirstCapture*(
  m: RegexMatch,
  groupName: string,
  text: string
): string {.inline, raises: [KeyError], deprecated.} =
  let captures = m.group(groupName, text)
  if captures.len > 0:
    return captures[0]
  else:
    return "" 

func groupLastCapture*(
  m: RegexMatch,
  groupName: string,
  text: string
): string {.inline, raises: [KeyError], deprecated: "use group(RegexMatch2, string) instead".} =
  let captures = m.group(groupName, text)
  if captures.len > 0:
    return captures[captures.len-1]
  else:
    return ""

func groupsCount*(m: RegexMatch): int {.inline, raises: [], deprecated: "use groupsCount(RegexMatch2)".} =
  m.captures.len

func groupNames*(m: RegexMatch): seq[string] {.inline, raises: [], deprecated: "use groupNames(RegexMatch2)".} =
  result = toSeq(m.namedGroups.keys)

func match*(
  s: string,
  pattern: Regex,
  m: var RegexMatch,
  start = 0
): bool {.inline, raises: [], deprecated: "use match(string, Regex2, var RegexMatch2) instead".} =
  debugCheckUtf8 s
  result = matchImpl(s, pattern, m, start)

func match*(s: string, pattern: Regex): bool {.inline, raises: [], deprecated: "use match(string, Regex2) instead".} =
  debugCheckUtf8 s
  var m: RegexMatch
  result = matchImpl(s, pattern, m)

iterator findAll*(
  s: string,
  pattern: Regex,
  start = 0
): RegexMatch {.inline, raises: [], deprecated: "use findAll(string, Regex2) instead".} =
  debugCheckUtf8 s
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
): seq[RegexMatch] {.inline, raises: [], deprecated: "use findAll(string, Regex2) instead".} =
  for m in findAll(s, pattern, start):
    result.add m

iterator findAllBounds*(
  s: string,
  pattern: Regex,
  start = 0
): Slice[int] {.inline, raises: [], deprecated: "use findAllBounds(string, Regex2) instead".} =
  debugCheckUtf8 s
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
): seq[Slice[int]] {.inline, raises: [], deprecated: "use findAllBounds(string, Regex2) instead".} =
  for m in findAllBounds(s, pattern, start):
    result.add m

func findAndCaptureAll*(
  s: string, pattern: Regex
): seq[string] {.inline, raises: [], deprecated: "use findAll(string, Regex2) instead".} =
  for m in s.findAll(pattern):
    result.add s[m.boundaries]

func contains*(s: string, pattern: Regex): bool {.inline, raises: [], deprecated: "use contains(string, Regex2) instead".} =
  for _ in findAllBounds(s, pattern):
    return true
  return false

func find*(
  s: string,
  pattern: Regex,
  m: var RegexMatch,
  start = 0
): bool {.inline, raises: [], deprecated: "use find(string, Regex2, var RegexMatch2) instead".} =
  m.clear()
  for m2 in findAll(s, pattern, start):
    m.captures.add m2.captures
    m.namedGroups = m2.namedGroups
    m.boundaries = m2.boundaries
    return true
  return false

iterator split*(s: string, sep: Regex): string {.inline, raises: [], deprecated: "use split(string, Regex2) instead".} =
  debugCheckUtf8 s
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

func split*(s: string, sep: Regex): seq[string] {.inline, raises: [], deprecated: "use split(string, Regex2) instead".} =
  for w in split(s, sep):
    result.add w

func splitIncl*(s: string, sep: Regex): seq[string] {.inline, raises: [], deprecated: "use splitIncl(string, Regex2) instead".} =
  template ab: untyped = m.boundaries
  debugCheckUtf8 s
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
): bool {.inline, raises: [], deprecated: "use startsWith(string, Regex2) instead".} =
  debugCheckUtf8 s
  startsWithImpl(s, pattern, start)

template runeIncAt(s: string, n: var int) =
  ## increment ``n`` up to
  ## next rune's index
  if n < s.len:
    inc(n, runeLenAt(s, n))
  else:
    n = s.len+1

func endsWith*(s: string, pattern: Regex): bool {.inline, raises: [], deprecated: "use endsWith(string, Regex2) instead".} =
  debugCheckUtf8 s
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

func replace*(
  s: string,
  pattern: Regex,
  by: string,
  limit = 0
): string {.inline, raises: [ValueError], deprecated: "use replace(string, Regex2, string) instead".} =
  debugCheckUtf8 s
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
): string {.inline, raises: [], effectsOf: by, deprecated: "use replace(string, Regex2, proc(RegexMatch2, string): string) instead".} =
  debugCheckUtf8 s
  result = ""
  var i, j = 0
  for m in findAll(s, pattern):
    result.addsubstr(s, i, m.boundaries.a-1)
    result.add by(m, s)
    i = m.boundaries.b+1
    inc j
    if limit > 0 and j == limit: break
  result.addsubstr(s, i)

func isInitialized*(re: Regex): bool {.inline, raises: [], deprecated: "use isInitialized(Regex2) instead".} =
  re.nfa.s.len > 0

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
    if isEpsilonTransition(pattern.nfa.s[nn]):
      continue
    result.add(", ")
    result.add(pattern.toString(nn, visited))
  result.add("]")

proc toString(pattern: Regex): string {.used.} =
  ## NFA to string representation.
  ## For debugging purposes
  var visited: set[int16]
  result = pattern.toString(0, visited)

{.pop.}  # {.push warning[Deprecated]: off.}

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
    result = re2(s).toString

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

  var m: RegexMatch2
  #doAssert match("abc", re2(r"abc", {reAscii}), m)
  doAssert match("abc", re2"abc", m)
  doAssert match("ab", re2"a(b|c)", m)
  doAssert match("ac", re2"a(b|c)", m)
  doAssert(not match("ad", re2"a(b|c)", m))
  doAssert match("ab", re2"(ab)*", m)
  doAssert match("abab", re2"(ab)*", m)
  doAssert(not match("ababc", re2"(ab)*", m))
  doAssert(not match("a", re2"(ab)*", m))
  doAssert match("ab", re2"(ab)+", m)
  doAssert match("abab", re2"(ab)+", m)
  doAssert(not match("ababc", re2"(ab)+", m))
  doAssert(not match("a", re2"(ab)+", m))
  doAssert match("aa", re2"\b\b\baa\b\b\b", m)
  doAssert(not match("cac", re2"c\ba\bc", m))
  doAssert match("abc", re2"[abc]+", m)
  doAssert match("abc", re2"[\w]+", m)
  doAssert match("弢弢弢", re2"[\w]+", m)
  doAssert(not match("abc", re2"[\d]+", m))
  doAssert match("123", re2"[\d]+", m)
  doAssert match("abc$%&", re2".+", m)
  doAssert(not match("abc$%&\L", re2"(.+)", m))
  doAssert(not match("abc$%&\L", re2".+", m))
  doAssert(not match("弢", re2"\W", m))
  doAssert match("$%&", re2"\W+", m)
  doAssert match("abc123", re2"[^\W]+", m)

  doAssert match("aabcd", re2"(aa)bcd", m) and
    m.captures == @[0 .. 1]
  doAssert match("aabc", re2"(aa)(bc)", m) and
    m.captures == @[0 .. 1, 2 .. 3]
  doAssert match("ab", re2"a(b|c)", m) and
    m.captures == @[1 .. 1]
  doAssert match("ab", re2"(ab)*", m) and
    m.captures == @[0 .. 1]
  doAssert match("abab", re2"(ab)*", m) and
    m.captures == @[2 .. 3]
  doAssert match("ab", re2"((a))b", m) and
    m.captures == @[0 .. 0, 0 .. 0]
  doAssert match("c", re2"((ab)*)c", m) and
    m.captures == @[0 .. -1, nonCapture]
  doAssert match("aab", re2"((a)*b)", m) and
    m.captures == @[0 .. 2, 1 .. 1]
  doAssert match("abbbbcccc", re2"a(b|c)*", m) and
    m.captures == @[8 .. 8]
  doAssert match("ab", re2"(a*)(b*)", m) and
    m.captures == @[0 .. 0, 1 .. 1]
  doAssert match("ab", re2"(a)*(b)*", m) and
    m.captures == @[0 .. 0, 1 .. 1]
  doAssert match("ab", re2"(a)*b*", m) and
    m.captures == @[0 .. 0]
  doAssert match("abbb", re2"((a(b)*)*(b)*)", m) and
    m.captures == @[0 .. 3, 0 .. 3, 3 .. 3, nonCapture]
  doAssert match("aa", re2"(a)+", m) and
    m.captures == @[1 .. 1]
  doAssert match("abab", re2"(ab)+", m) and
    m.captures == @[2 .. 3]
  doAssert match("a", re2"(a)?", m) and
    m.captures == @[0 .. 0]
  doAssert match("ab", re2"(ab)?", m) and
    m.captures == @[0 .. 1]
  doAssert match("aaabbbaaa", re2"(a*|b*)*", m) and
    m.captures == @[9 .. 8]
  doAssert match("abab", re2"(a(b))*", m) and
    m.captures == @[2 .. 3, 3 .. 3]
  doAssert match("aaanasdnasd", re2"((a)*n?(asd)*)*", m) and
    m.captures == @[11 .. 10, 2 .. 2, 8 .. 10]
  doAssert match("aaanasdnasd", re2"((a)*n?(asd))*", m) and
    m.captures == @[7 .. 10, 2 .. 2, 8 .. 10]
  doAssert match("abd", re2"((ab)c)|((ab)d)", m) and
    m.captures == @[nonCapture, nonCapture, 0 .. 2, 0 .. 1]
  doAssert match("aaa", re2"(a*)", m) and
    m.captures == @[0 .. 2]
  doAssert match("aaaa", re2"(a*)(a*)", m) and
    m.captures == @[0 .. 3, 4 .. 3]
  doAssert match("aaaa", re2"(a*?)(a*?)", m) and
    m.captures == @[0 .. -1, 0 .. 3]
  doAssert match("aaaa", re2"(a)*(a)", m) and
    m.captures == @[2 .. 2, 3 .. 3]
  
  doAssert match("abc", re2"abc")
  doAssert(not match("abc", re2"abd"))
  doAssert(not match("abc", re2"ab"))
  doAssert(not match("abc", re2"b"))
  doAssert(not match("abc", re2"c"))

  doAssert re2"bc" in "abcd"
  doAssert re2"(23)+" in "23232"
  doAssert re2"^(23)+$" notin "23232"
  doAssert re2"\w" in "弢"
  #doAssert re2(r"\w", {reAscii}) notin "弢"
  #doAssert re2(r"\w", {reAscii}) in "a"

  doAssert "abcd".find(re2"bc", m)
  doAssert(not "abcd".find(re2"de", m))
  #doAssert "%ab%".find(re2(r"\w{2}", {reAscii}), m)
  doAssert "%弢弢%".find(re2"\w{2}", m)
  #doAssert(not "%弢弢%".find(re2(r"\w{2}", {reAscii}), m)
  doAssert(
    "2222".find(re2"(22)*", m) and
    m.group(0) == 2 .. 3)
  doAssert(
    "11222211".find(re2"(22)+", m) and
    m.group(0) == 4 .. 5)
  
  doAssert match("650-253-0001", re2"[0-9]+-[0-9]+-[0-9]+", m)
  doAssert(not match("abc-253-0001", re2"[0-9]+-[0-9]+-[0-9]+", m))
  doAssert(not match("650-253", re2"[0-9]+-[0-9]+-[0-9]+", m))
  doAssert(not match("650-253-0001-abc", re2"[0-9]+-[0-9]+-[0-9]+", m))
  doAssert match("650-253-0001", re2"[0-9]+..*", m)
  doAssert(not match("abc-253-0001", re2"[0-9]+..*", m))
  doAssert(not match("6", re2"[0-9]+..*", m))

  doAssert match("abcabcabc", re2"(?:(?:abc)){3}")
  doAssert match("abcabcabc", re2"((abc)){3}")

  doAssert match("", re2"|")
  doAssert match("a", re2"a|")
  doAssert match("", re2"a|")
  doAssert(not match("b", re2"a|"))
  doAssert match("b", re2"|b")
  doAssert match("", re2"|b")
  doAssert(not match("a", re2"|b"))
  doAssert match("", re2"(|)")
  doAssert match("a", re2"(a|)")
  doAssert match("", re2"(a|)")
  doAssert(not match("b", re2"(a|)"))
  doAssert match("b", re2"(|b)")
  doAssert match("", re2"(|b)")
  doAssert(not match("a", re2"(|b)"))
  doAssert match("", re2"||")

  doAssert match(" ", re2"(?x)     (?-x) ")
  doAssert match("aa", re2"((?x)   a    )a")
  doAssert match(" ", re2"((?x)     ) ")
  doAssert match(" ", re2"(?x:(?x)     ) ")
  doAssert match(" ", re2"((?x:)) ")
  doAssert match("A", re2"(?xi)     a")
  doAssert(not match("A", re2"((?xi))     a"))
  doAssert(not match("A", re2"(?xi:(?xi)     )a"))

  # bug: raises invalid utf8 regex in Nim 1.0 + js target
  when not defined(js) or NimMajor >= 2:
    block:
      let flags = {regexArbitraryBytes}
      doAssert match("\xff", re2(r"\xff", flags))
      doAssert match("\xff", re2("\xff", flags))
      doAssert replace("\xff", re2(r"\xff", flags), "abc") == "abc"
      doAssert match("\xff\xff", re2(r"\xff\xff", flags))
      doAssert replace("\xff\xff", re2(r"\xff\xff", flags), "abc") == "abc"
      doAssert match("\xff\xff", re2(r"\xff+", flags))
      doAssert replace("\xff\xff", re2(r"\xff", flags), "abc") == "abcabc"
      doAssert(not match("\xf0", re2(r"\xff", flags)))
      doAssert replace("\xf0", re2(r"\xff", flags), "abc") == "\xf0"
      doAssert match("\x02\xF8\x95", re2(r"(?u).+(?<=\x{2F895})", flags))

  doAssert graph(toRegex(re2"^a+$")) == """digraph graphname {
    0 [label="q0";color=blue];
    2 [label="q1";color=black];
    4 [label="q2";color=blue];
    0 -> 2 [label="a, {^}, i=0"];
    2 -> 2 [label="a, i=0"];2 -> 4 [label="{eoe}, {$}, i=1"];
}
"""

  # subset of tests.nim
  proc raisesMsg(pattern: string): string =
    try:
      discard re2(pattern)
    except RegexError:
      result = getCurrentExceptionMsg()

  template test(body: untyped): untyped =
    static:
      (proc() = body)()
    (proc() = body)()

  test:
    var m: RegexMatch2
    doAssert match("ac", re2"a(b|c)", m)
    doAssert(not match("ad", re2"a(b|c)", m))
    doAssert match("ab", re2"(ab)*", m)
    doAssert match("abab", re2"(ab)*", m)
    doAssert(not match("ababc", re2"(ab)*", m))
    doAssert(not match("a", re2"(ab)*", m))
    doAssert match("abab", re2"(ab)*", m) and
      m.captures == @[2 .. 3]
    doAssert match("bbaa aa", re2"([\w ]*?)(\baa\b)", m) and
      m.captures == @[0 .. 4, 5 .. 6]
    doAssert re2"bc" in "abcd"
    doAssert re2"(23)+" in "23232"
    doAssert re2"^(23)+$" notin "23232"
    doAssert re2"\w" in "弢"
    doAssert "2222".find(re2"(22)*", m) and
      m.group(0) == 2 .. 3
    doAssert raisesMsg("\xff") == "Invalid utf-8 regex"
    doAssert raisesMsg(r"[a-\w]") ==
      "Invalid set range. Range can't contain " &
      "a character-class or assertion\n" &
      "[a-\\w]\n" &
      "   ^"
    doAssert "a,b".splitIncl(re2"(,)") == @["a", ",", "b"]
    doAssert "abcabc".replace(re2"(abc)", "m($1)") ==
      "m(abc)m(abc)"
    const ip = re2"""(?x)
    \b
    ((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}
    (25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
    \b
    """
    doAssert match("127.0.0.1", ip)
    doAssert(not match("127.0.0.999", ip))
    doAssert "abcd".find(re2"bc", m) and
      m.boundaries == 1 .. 2
    doAssert "bcd".find(re2"bc", m) and
      m.boundaries == 0 .. 1
    doAssert "bc".find(re2"bc", m) and
      m.boundaries == 0 .. 1
    doAssert "#foo://#".find(re2"[\w]+://", m) and
      m.boundaries == 1 .. 6
    doAssert findAllBounds("abcd", re2"bc") == @[1 .. 2]
    doAssert findAllBounds("bcd", re2"bc") == @[0 .. 1]
    doAssert findAllBounds("bc", re2"bc") == @[0 .. 1]
    doAssert findAllBounds("#foo://#", re2"[\w]+://") == @[1 .. 6]
    doAssert findAllBounds("abc\nabc\na", re2"(?m)^a") ==
      @[0 .. 0, 4 .. 4, 8 .. 8]
    doAssert match("ab", re2"a(?=b)\w")
    doAssert(not match("ab", re2"a(?=x)\w"))
    doAssert match("ab", re2"\w(?<=a)b")
    doAssert(not match("ab", re2"\w(?<=x)b"))
    doAssert match("aaab", re2".*(?<=^(\w*?)(\w*?)(\w??)$)", m) and
      m.captures == @[0 .. 3, 4 .. 3, 4 .. 3]
    doAssert match("aaab", re2".*(?<=^(\w*?)(\w*)(\w??)$)", m) and
      m.captures == @[0 .. -1, 0 .. 3, 4 .. 3]
    doAssert match("aaab", re2"(\w*)(\w??)", m) and
      m.captures == @[0 .. 3, 4 .. 3]
    doAssert match("aaab", re2".*(?<=^(\w*)(\w??)$)", m) and
      m.captures == @[0 .. 3, 4 .. 3]
    doAssert match("aaab", re2".*(?<=^(\w*)(\w?)$)", m) and
      m.captures == @[0 .. 2, 3 .. 3]
    doAssert match("aaab", re2".*(?<=^(\d)\w+|(\w)\w{3}|(\w)\w{3}|(\w)\w+$)", m) and
      m.captures == @[nonCapture, 0 .. 0, nonCapture, nonCapture]
    doAssert match("aaab", re2".*(?<=^(\d)\w+|(\d)\w{3}|(\w)\w{3}|(\w)\w+$)", m) and
      m.captures == @[nonCapture, nonCapture, 0 .. 0, nonCapture]
    doAssert match("aaab", re2".*(?<=^(\d)\w+|(\d)\w{3}|(\d)\w{3}|(\w)\w+$)", m) and
      m.captures == @[nonCapture, nonCapture, nonCapture, 0 .. 0]
    doAssert(not match("ab", re2"\w(?<=a(?=b(?<=a)))b"))
    doAssert(not match("ab", re2"\w(?<=a(?<=a(?=b(?<=a))))b"))
    doAssert match("ab", re2"\w(?<=a(?=b(?<=b)))b")
    doAssert match("ab", re2"\w(?<=a(?<=a(?=b(?<=b))))b")
    doAssert findAllBounds(r"1abab", re2"(?<=\d\w*)ab") ==
      @[1 .. 2, 3 .. 4]
    doAssert findAllBounds(r"abab", re2"(?<=\d\w*)ab").len == 0
    doAssert findAllBounds(r"abab1", re2"ab(?=\w*\d)") ==
      @[0 .. 1, 2 .. 3]
    doAssert findAllBounds(r"abab", re2"ab(?=\w*\d)").len == 0
    doAssert match("aΪ", re2"a(?=Ϊ)\w")
    doAssert match("Ϊb", re2"Ϊ(?=b)\w")
    doAssert match("弢Ⓐ", re2"弢(?=Ⓐ)\w")
    doAssert match("aΪ", re2"\w(?<=a)Ϊ")
    doAssert match("Ϊb", re2"\w(?<=Ϊ)b")
    doAssert match("弢Ⓐ", re2"\w(?<=弢)Ⓐ")
    doAssert match("弢", re2"(?-u).+")
    block:  # Follows Nim re's behaviour
      doAssert match("abc", re2"(?<=a)bc", m, start = 1)
      doAssert(not match("abc", re2"(?<=x)bc", m, start = 1))
      doAssert(not match("abc", re2"^bc", m, start = 1))
    doAssert startsWith("abc", re2"b", start = 1)
    doAssert startsWith("abc", re2"(?<=a)b", start = 1)
    doAssert startsWith("abc", re2"b", start = 1)
    doAssert(not startsWith("abc", re2"(?<=x)b", start = 1))
    doAssert(not startsWith("abc", re2"^b", start = 1))
    doAssert(not match("ab", re2"ab(?=x)"))
    doAssert(not match("ab", re2"(?<=x)ab"))
    doAssert match("ab", re2"(?<=^)ab")
    doAssert match("ab", re2"ab(?=$)")
    doAssert match("abcdefg", re2"\w+(?<=(ab)(?=(cd)))\w+", m) and
      m.captures == @[0 .. 1, 2 .. 3]
    doAssert match("abcdefg", re2"\w+(?<=(ab)(?=(cd)(?<=(cd))))\w+", m) and
      m.captures == @[0 .. 1, 2 .. 3, 2 .. 3]
    doAssert match("aaab", re2"(\w+)|\w+(?<=^\w+)b", m) and
      m.captures == @[0 .. 3]
    doAssert match("aaab", re2"(\w+)|\w+(?<=^(\w+))b", m) and
      m.captures == @[0 .. 3, reNonCapture]
    doAssert match("aaab", re2"(\w+)|\w+(?<=^(\w)(\w+))b", m) and
      m.captures == @[0 .. 3, reNonCapture, reNonCapture]
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
