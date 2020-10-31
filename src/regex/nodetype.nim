# XXX hack because of nfamatch compile warning
when NimMajor >= 1:
  {.used.}

import std/unicode
import std/sets
from std/algorithm import sorted

import pkg/unicodedb/properties

import ./common

type
  Flag* = enum
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
  NodeKind* = enum
    reChar,
    reCharCi,
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
    reWord,  # \w
    reDigit,  # \d
    reWhiteSpace,  # \s
    reUCC,  # \pN or \p{Nn}
    reNotAlphaNum,  # \W
    reNotDigit,  # \D
    reNotWhiteSpace,  # \S
    reNotUCC,  # \PN or \P{Nn}
    reAny,  # .
    reAnyNl,  # . new-line
    reWordBoundaryAscii,  # \b ascii only
    reNotWordBoundaryAscii,  # \B ascii only
    reWordAscii,  # \w ascii only
    reDigitAscii,  # \d ascii only
    reWhiteSpaceAscii,  # \s ascii only
    reNotAlphaNumAscii,  # \W ascii only
    reNotDigitAscii,  # \D ascii only
    reNotWhiteSpaceAscii,  # \S ascii only
    reAnyAscii,  # . ascii only
    reAnyNlAscii,  # . new-line ascii only
    reInSet,  # [abc]
    reNotSet,  # [^abc]
    reLookahead,  # (?=...)
    reLookbehind,  # (?<=...)
    reNotLookahead,  # (?!...)
    reNotLookbehind,  # (?<!...)
    reSkip,  # dummy
    reEoe  # End of expression
  NodeUid* = int16
  Node* = object
    kind*: NodeKind
    cp*: Rune
    next*: seq[int16]
    isGreedy*: bool
    uid*: NodeUid
    # reGroupStart, reGroupEnd
    idx*: int16  # todo: rename?
    isCapturing*: bool
    name*: string
    flags*: seq[Flag]
    # reRepRange
    min*, max*: int16
    # reInSet, reNotSet
    cps*: HashSet[Rune]
    ranges*: seq[Slice[Rune]]  # todo: interval tree
    shorthands*: seq[Node]
    # reUCC, reNotUCC
    cc*: UnicodeCategorySet

func toCharNode*(r: Rune): Node =
  ## return a ``Node`` that is meant to be matched
  ## against text characters
  Node(kind: reChar, cp: r)

func initJoinerNode*(): Node =
  ## return a ``Node`` of ``reJoiner`` kind.
  ## Joiners are temporary nodes,
  ## they serve to generate the NFA
  ## but they are never part of it
  Node(kind: reJoiner, cp: "~".toRune)

func initEoeNode*(): Node =
  ## return the end-of-expression ``Node``.
  ## This is a dummy node that marks a match as successful
  Node(kind: reEoe, cp: "#".toRune)

template initSetNodeImpl(result: var Node, k: NodeKind) =
  ## base node
  assert k in {reInSet, reNotSet}
  result = Node(
    kind: k,
    cp: "#".toRune,
    cps: initHashSet[Rune](2),
    ranges: @[],
    shorthands: @[])

func initSetNode*(): Node =
  ## return a set ``Node``,
  ## parsed from an expression such as ``[a-z]``
  initSetNodeImpl(result, reInSet)

func initNotSetNode*(): Node =
  ## return a negated set ``Node``,
  ## parsed from an expression such as ``[^a-z]``
  initSetNodeImpl(result, reNotSet)

func initGroupStart*(
  name: string = "",
  flags: seq[Flag] = @[],
  isCapturing = true
): Node =
  ## return a ``reGroupStart`` node
  Node(
    kind: reGroupStart,
    cp: "(".toRune,
    name: name,
    flags: flags,
    isCapturing: isCapturing)

func initSkipNode*(next: openArray[int16]): Node =
  ## Return a dummy node that should be skipped
  ## while traversing the NFA
  result = Node(
    kind: reSkip,
    cp: "#".toRune)
  when false:
    # pending https://github.com/nim-lang/Nim/issues/15511#issuecomment-719952709
    result.next.add next
  else:
    for ai in next:
      result.next.add ai

func isEmpty*(n: Node): bool =
  ## check if a set ``Node`` is empty
  assert n.kind in {reInSet, reNotSet}
  result = (
    n.cps.len == 0 and
    n.ranges.len == 0 and
    n.shorthands.len == 0)

const
  opKind* = {
    reJoiner,
    reOr,
    reZeroOrMore,
    reOneOrMore,
    reZeroOrOne,
    reRepRange}
  assertionKind* = {
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
  shorthandKind* = {
    reWord,
    reDigit,
    reWhiteSpace,
    reUCC,
    reNotAlphaNum,
    reNotDigit,
    reNotWhiteSpace,
    reNotUCC,
    reWordAscii,
    reDigitAscii,
    reWhiteSpaceAscii,
    reNotAlphaNumAscii,
    reNotDigitAscii,
    reNotWhiteSpaceAscii}
  matchableKind* = {
    reChar,
    reCharCI,
    reWord,
    reDigit,
    reWhiteSpace,
    reUCC,
    reNotAlphaNum,
    reNotDigit,
    reNotWhiteSpace,
    reNotUCC,
    reAny,
    reAnyNL,
    reInSet,
    reNotSet,
    reWordAscii,
    reDigitAscii,
    reWhiteSpaceAscii,
    reNotAlphaNumAscii,
    reNotDigitAscii,
    reNotWhiteSpaceAscii,
    reAnyAscii,
    reAnyNLAscii}
  repetitionKind* = {
    reZeroOrMore,
    reOneOrMore,
    reRepRange}
  groupKind* = {
    reGroupStart,
    reGroupEnd}

func toString*(n: Node): string =
  ## return the string representation
  ## of a `Node`. The string is always
  ## equivalent to the original
  ## expression but not necessarily equal
  # Note this is not complete. Just
  # what's needed for debugging so far
  case n.kind
  of reZeroOrMore,
      reOneOrMore,
      reZeroOrOne:
    if not n.isGreedy:
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
      str.add(nn.toString)
    str.add(']')
    str
  of reSkip:
    "SKIP"
  of reEOE:
    "EOE"
  else:
    n.cp.toUTF8

func toString*(n: seq[Node]): string =
  result = newStringOfCap(n.len)
  for nn in n:
    result.add(nn.toString)
