import std/unicode except `==`
import std/sets

import pkg/unicodedb/properties
import pkg/unicodedb/types as utypes

import ./types
import ./common

func `==`(a, b: Rune): bool {.inline.} =
  a.int32 == b.int32

func isWord(r: Rune): bool {.inline.} =
  utmWord in unicodeTypes(r)

func isDecimal(r: Rune): bool {.inline.} =
  utmDecimal in unicodeTypes(r)

func isWordAscii(r: Rune): bool {.inline.} =
  ## return ``true`` if the given
  ## rune is in ``[A-Za-z0-9]`` range
  case r.int32
  of 'A'.ord .. 'Z'.ord,
      'a'.ord .. 'z'.ord,
      '0'.ord .. '9'.ord,
      '_'.ord:
    true
  else:
    false

template isWordBoundaryImpl(r, nxt, isWordProc): bool =
  (r.int32 > -1 and isWordProc(r)) xor
    (nxt.int32 > -1 and isWordProc(nxt))

func isWordBoundary(r: Rune, nxt: Rune): bool {.inline.} =
  ## check if current match
  ## is a boundary (i.e the end of a word)
  isWordBoundaryImpl(r, nxt, isWord)

func isWordBoundaryAscii(r: Rune, nxt: Rune): bool {.inline.} =
  ## check if current match
  ## is a boundary. Match ascii only
  isWordBoundaryImpl(r, nxt, isWordAscii)

func match*(n: Node, r: Rune, nxt: Rune): bool {.inline.} =
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

func contains(sr: seq[Slice[Rune]], r: Rune): bool {.inline.} =
  result = false
  for sl in sr:
    result = r in sl
    if result:
      break

func isWhiteSpace(r: Rune): bool {.inline.} =
  utmWhiteSpace in unicodeTypes(r)

func isWhiteSpaceAscii(r: Rune): bool {.inline.} =
  case r.int32
  of ' '.ord,
      '\t'.ord,
      '\L'.ord,
      '\r'.ord,
      '\f'.ord,
      '\v'.ord:
    true
  else:
    false

func isDigitAscii(r: Rune): bool {.inline.} =
  case r.int32
  of '0'.ord .. '9'.ord:
    true
  else:
    false

# todo: can not use unicodeplus due to
# https://github.com/nim-lang/Nim/issues/7059
func swapCase*(r: Rune): Rune =
  result = r.toLower()
  if result != r:
    return
  result = r.toUpper()

func matchShorthand(n: Node, r: Rune): bool =
  case n.kind
  of reWord: r.isWord()
  of reNotAlphaNum: not r.isWord()
  of reDigit: r.isDecimal()
  of reNotDigit: not r.isDecimal()
  of reWhiteSpace: r.isWhiteSpace()
  of reNotWhiteSpace: not r.isWhiteSpace()
  of reWordAscii: r.isWordAscii()
  of reDigitAscii: r.isDigitAscii()
  of reWhiteSpaceAscii: r.isWhiteSpaceAscii()
  of reUCC: r.unicodeCategory() in n.cc
  of reNotAlphaNumAscii: not r.isWordAscii()
  of reNotDigitAscii: not r.isDigitAscii()
  of reNotWhiteSpaceAscii: not r.isWhiteSpaceAscii()
  of reNotUCC: r.unicodeCategory() notin n.cc
  else:
    doAssert false
    false

func matchSet(n: Node, r: Rune): bool =
  result = (
    r in n.cps or
    r in n.ranges
  )
  if not result:
    for nn in n.shorthands:
      result = matchShorthand(nn, r)
      if result:
        break
  result = (
    (result and n.kind == reInSet) or
    (not result and n.kind == reNotSet)
  )

func match*(n: Node, r: Rune): bool {.inline.} =
  ## match for ``Node`` of matchable kind.
  ## Return whether the node matches
  ## the current character or not
  if r.int32 < 0:
    return n.kind == reEOE
  if n.kind == reChar:
    return n.cp == r
  case n.kind
  of reEOE: r == invalidRune
  of reWord: r.isWord()
  of reNotAlphaNum: not r.isWord()
  of reDigit: r.isDecimal()
  of reNotDigit: not r.isDecimal()
  of reWhiteSpace: r.isWhiteSpace()
  of reNotWhiteSpace: not r.isWhiteSpace()
  of reAny: r != lineBreakRune
  of reAnyNL: true
  of reCharCI: r == n.cp or r == n.cp.swapCase()
  of reWordAscii: r.isWordAscii()
  of reDigitAscii: r.isDigitAscii()
  of reWhiteSpaceAscii: r.isWhiteSpaceAscii()
  of reUCC: r.unicodeCategory() in n.cc
  of reNotAlphaNumAscii: not r.isWordAscii()
  of reNotDigitAscii: not r.isDigitAscii()
  of reNotWhiteSpaceAscii: not r.isWhiteSpaceAscii()
  of reNotUCC: r.unicodeCategory() notin n.cc
  of reInSet, reNotSet: matchSet(n, r)
  else:
    assert n.kind == reChar
    n.cp == r
