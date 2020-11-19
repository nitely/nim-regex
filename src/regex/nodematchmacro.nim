import std/macros
import std/unicode
import std/tables
import std/sets

import pkg/unicodedb/properties
import pkg/unicodedb/types

import ./nodetype

# todo: can not use unicodeplus due to
# https://github.com/nim-lang/Nim/issues/7059
func swapCase(r: Rune): Rune =
  result = r.toLower()
  if result != r:
    return
  result = r.toUpper()

func genWordAsciiMatch(c: NimNode): NimNode =
  result = newStmtList()
  result.add quote do:
    ('a'.ord <= `c` and `c` <= 'z'.ord) or
    ('A'.ord <= `c` and `c` <= 'Z'.ord) or
    ('0'.ord <= `c` and `c` <= '9'.ord) or
    (`c` == '_'.ord)

func genWordMatch(c: NimNode): NimNode =
  result = newStmtList()
  result.add quote do:
    ('a'.ord <= `c` and `c` <= 'z'.ord) or
    ('A'.ord <= `c` and `c` <= 'Z'.ord) or
    ('0'.ord <= `c` and `c` <= '9'.ord) or
    (`c` == '_'.ord) or
    (`c` > 128'i32 and contains(
      unicodeTypes(Rune(`c`)), utmWord))

func genDigitAsciiMatch(c: NimNode): NimNode =
  result = newStmtList()
  result.add quote do:
    '0'.ord <= `c` and `c` <= '9'.ord

func genDigitMatch(c: NimNode): NimNode =
  result = newStmtList()
  result.add quote do:
    ('0'.ord <= `c` and `c` <= '9'.ord) or
    (`c` > 128'i32 and contains(
      unicodeTypes(Rune(`c`)), utmDecimal))

func genWhiteSpaceAsciiMatch(c: NimNode): NimNode =
  result = newStmtList()
  result.add quote do:
    case `c`
    of ' '.ord, '\t'.ord, '\L'.ord,
        '\r'.ord, '\f'.ord, '\v'.ord:
      true
    else:
      false

func genWhiteSpaceMatch(c: NimNode): NimNode =
  result = newStmtList()
  result.add quote do:
    case `c`
    of ' '.ord, '\t'.ord, '\L'.ord,
        '\r'.ord, '\f'.ord, '\v'.ord:
      true
    else:
      `c` > 128'i32 and contains(
        unicodeTypes(Rune(`c`)), utmWhiteSpace)

func genMatch*(c: NimNode, n: Node): NimNode =
  let cpLit = newLit n.cp.int32
  result = case n.kind
    of reChar:
      quote do: `c` == `cpLit`
    of reWord:
      genWordMatch(c)
    of reNotAlphaNum:
      let wordMatch = genWordMatch(c)
      quote do: not `wordMatch`
    of reDigit:
      genDigitMatch(c)
    of reNotDigit:
      let digitMatch = genDigitMatch(c)
      quote do: not `digitMatch`
    of reWhiteSpace:
      genWhiteSpaceMatch(c)
    of reNotWhiteSpace:
      let whiteSpaceMatch = genWhiteSpaceMatch(c)
      quote do: not `whiteSpaceMatch`
    of reAny:
      quote do: `c` != '\L'.ord
    of reAnyAscii:
      quote do: `c` <= 128 and `c` != '\L'.ord
    of reAnyNL:
      quote do: true
    of reAnyNlAscii:
      quote do: `c` <= 128
    of reCharCI:
      let cp2Lit = newLit n.cp.swapCase().int32
      quote do: `c` == `cpLit` or `c` == `cp2Lit`
    of reWordAscii:
      genWordAsciiMatch(c)
    of reNotAlphaNumAscii:
      let wordMatch = genWordAsciiMatch(c)
      quote do: not `wordMatch`
    of reDigitAscii:
      genDigitAsciiMatch(c)
    of reNotDigitAscii:
      let digitMatch = genDigitAsciiMatch(c)
      quote do: not `digitMatch`
    of reWhiteSpaceAscii:
      genWhiteSpaceAsciiMatch(c)
    of reNotWhiteSpaceAscii:
      let whiteSpaceMatch = genWhiteSpaceAsciiMatch(c)
      quote do: not `whiteSpaceMatch`
    of reUCC:
      let cc = newLit n.cc.int32
      quote do: contains(
        UnicodeCategorySet(`cc`),
        unicodeCategory(Rune(`c`)))
    of reNotUCC:
      let cc = newLit n.cc.int32
      quote do: not contains(
        UnicodeCategorySet(`cc`),
        unicodeCategory(Rune(`c`)))
    else:
      doAssert false, $n.kind
      quote do: false

func genSetMatch*(c: NimNode, n: Node): NimNode =
  assert n.kind in {reInSet, reNotSet}
  var terms: seq[NimNode]
  if n.ranges.len > 0:
    for bound in n.ranges:
      let a = newLit bound.a.int32
      let b = newLit bound.b.int32
      terms.add quote do:
        `a` <= `c` and `c` <= `b`
  if n.cps.len > 0:
    var caseStmt: seq[NimNode]
    caseStmt.add c
    for cp in n.cps:
      caseStmt.add newTree(nnkOfBranch,
        newLit cp.int32,
        quote do: true)
    caseStmt.add newTree(nnkElse,
      quote do: false)
    let caseStmtTerm = newTree(nnkCaseStmt, caseStmt)
    terms.add quote do:
      `caseStmtTerm`
  if n.shorthands.len > 0:
    for nn in n.shorthands:
      terms.add case nn.kind:
      of reInSet:
        genSetMatch(c, nn)
      of reNotSet:
        let setMatch = genSetMatch(c, nn)
        quote do: not `setMatch`
      else:
        genMatch(c, nn)
  assert terms.len > 0
  let term = terms[0]
  result = quote do:
    `term`
  for i in 1 .. terms.len-1:
    let term = terms[i]
    result = quote do:
      `result` or `term`

func genWordBoundary(cA, cB: NimNode): NimNode =
  let wordMatchA = genWordMatch(cA)
  let wordMatchB = genWordMatch(cB)
  result = quote do:
    (`cA` != -1'i32 and `wordMatchA`) xor
      (`cB` != -1'i32 and `wordMatchB`)

func genWordBoundaryAscii(cA, cB: NimNode): NimNode =
  let wordMatchA = genWordAsciiMatch(cA)
  let wordMatchB = genWordAsciiMatch(cB)
  result = quote do:
    (`cA` != -1'i32 and `wordMatchA`) xor
      (`cB` != -1'i32 and `wordMatchB`)

func genMatch*(n: Node, cA, cB: NimNode): NimNode =
  let cpLit = newLit n.cp.int32
  case n.kind
  of reStart, reStartSym:
    quote do: `cA` == -1'i32
  of reEnd, reEndSym:
    quote do: `cB` == -1'i32
  of reStartSymML:
    quote do: `cA` == -1'i32 or `cA` == '\L'.ord
  of reEndSymML:
    quote do: `cB` == -1'i32 or `cB` == '\L'.ord
  of reWordBoundary:
    genWordBoundary(cA, cB)
  of reNotWordBoundary:
    let wordBoundary = genWordBoundary(cA, cB)
    quote do: not `wordBoundary`
  of reWordBoundaryAscii:
    genWordBoundaryAscii(cA, cB)
  of reNotWordBoundaryAscii:
    let wordBoundary = genWordBoundaryAscii(cA, cB)
    quote do: not `wordBoundary`
  of reLookahead:
    quote do: `cpLit` == `cB`
  of reNotLookahead:
    quote do: `cpLit` != `cB`
  of reLookbehind:
    quote do: `cpLit` == `cA`
  of reNotLookbehind:
    quote do: `cpLit` != `cA`
  else:
    doAssert false
    quote do: false
