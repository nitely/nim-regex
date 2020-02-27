import unicode
import sets

import unicodedb/properties

import common

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
  Node* = object
    kind*: NodeKind
    cp*: Rune
    next*: seq[int16]
    isGreedy*: bool
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

func initEOENode*(): Node =
  ## return the end-of-expression ``Node``.
  ## This is a dummy node that marks a match as successful
  Node(kind: reEOE, cp: "#".toRune)

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
  matchTransitionKind* = {
    reWhiteSpace,
    reUCC,
    reNotAlphaNum,
    reNotDigit,
    reNotWhiteSpace,
    reNotUCC,
    reInSet,
    reNotSet}
