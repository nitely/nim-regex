## NFA matcher for static regexes

import macros
import unicode
import tables
import sets

import unicodedb/properties
import unicodedb/types

import nodetype
import nfatype

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

func genMatch(c: NimNode, n: Node): NimNode =
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

func genSetMatch(c: NimNode, n: Node): NimNode =
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

func genMatch(n: Node, cA, cB: NimNode): NimNode =
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

func genMatchedBody(
  smB, ntLit, capt, start, matched, captx,
  capts, charIdx, cPrev, c: NimNode,
  i, nti: int,
  regex: Regex
): NimNode =
  if regex.transitions.allZ[i][nti] == -1'i16:
    return quote do:
      add(`smB`, (`ntLit`, `capt`, `start`))
  var matchedBody: seq[NimNode]
  matchedBody.add quote do:
    `matched` = true
    `captx` = `capt`
  for z in regex.transitions.z[regex.transitions.allZ[i][nti]]:
    case z.kind
    of groupKind:
      let zIdx = newLit z.idx
      matchedBody.add quote do:
        add(`capts`, CaptNode(
          parent: `captx`,
          bound: `charIdx`,
          idx: `zIdx`))
        `captx` = (len(`capts`) - 1).int32
    of assertionKind:
      # https://github.com/nim-lang/Nim/issues/13266
      #let zLit = newLit z
      let matchCond = genMatch(`z`, `cPrev`, `c`)
      matchedBody.add quote do:
        `matched` = `matched` and `matchCond`
    else:
      doAssert false
  matchedBody.add quote do:
    if `matched`:
      add(`smB`, (`ntLit`, `captx`, `start`))
  return newStmtList matchedBody

macro genSubmatch(
  n, capt, start, smB, c, matched, captx,
  capts, charIdx, cPrev: typed,
  regex: static Regex
): untyped =
  #[
    case n
    of 0:
      if not smB.hasState(1):
        if c == 'a':
          smB.add((1, capt))
      if not smB.hasState(4):
        if c == 'b':
          smB.add((4, capt))
    of 1:
      ...
    else:
      error
  ]#
  result = newStmtList()
  var caseStmtN: seq[NimNode]
  caseStmtN.add n
  for i in 0 .. regex.nfa.len-1:
    if regex.nfa[i].next.len == 0:  # end state
      continue
    var branchBodyN: seq[NimNode]
    for nti, nt in regex.nfa[i].next.pairs:
      let matchCond = case regex.nfa[nt].kind
        of reEoe:
          quote do: `c` == -1'i32
        of reInSet:
          let m = genSetMatch(c, regex.nfa[nt])
          quote do: `c` >= 0'i32 and `m`
        of reNotSet:
          let m = genSetMatch(c, regex.nfa[nt])
          quote do: `c` >= 0'i32 and not `m`
        else:
          let m = genMatch(c, regex.nfa[nt])
          quote do: `c` >= 0'i32 and `m`
      let ntLit = newLit nt
      let matchedBodyStmt = genMatchedBody(
        smB, ntLit, capt, start, matched, captx,
        capts, charIdx, cPrev, c,
        i, nti, regex)
      branchBodyN.add quote do:
        if not hasState(`smB`, `ntLit`) and (`matchCond`):
          `matchedBodyStmt`
    doAssert branchBodyN.len > 0
    caseStmtN.add newTree(nnkOfBranch,
      newLit i.int16,
      newStmtList(
        branchBodyN))
  doAssert caseStmtN.len > 1
  caseStmtN.add newTree(nnkElse,
    quote do:
      doAssert false
      discard)
  result.add newTree(nnkCaseStmt, caseStmtN)
  when defined(reDumpMacro):
    echo "==== genSubmatch ===="
    echo repr(result)

template submatch(
  smA, smB, regex, c,
  capts, charIdx, cPrev: untyped
): untyped =
  smB.clear()
  var captx: int32
  var matched = true
  for n, capt, start in smA.items:
    genSubmatch(
      n, capt, start, smB, c, matched, captx,
      capts, charIdx, cPrev, regex)
  swap smA, smB

macro genSubmatchEoe(
  n, capt, start, smB, c, c2, matched, captx,
  capts, charIdx, cPrev: typed,
  regex: static Regex
): untyped =
  # This is the same as genSubmatch
  # but just for EOE states
  #[
    case n
    of 0:
      if not smB.hasState(1):
        if c == -1:
          smB.add((1, capt))
    of 1:
      ...
    else:
      discard
  ]#
  result = newStmtList()
  var caseStmtN: seq[NimNode]
  caseStmtN.add n
  for i in 0 .. regex.nfa.len-1:
    if regex.nfa[i].next.len == 0:  # end state
      continue
    var branchBodyN: seq[NimNode]
    for nti, nt in regex.nfa[i].next.pairs:
      if regex.nfa[nt].kind == reEoe:
        let ntLit = newLit nt
        let matchedBodyStmt = genMatchedBody(
          smB, ntLit, capt, start, matched, captx,
          capts, charIdx, cPrev, c,
          i, nti, regex)
        branchBodyN.add quote do:
          if `c2` == -1'i32 and not hasState(`smB`, `ntLit`):
            `matchedBodyStmt`
    if branchBodyN.len > 0:
      caseStmtN.add newTree(nnkOfBranch,
        newLit i.int16,
        newStmtList(
          branchBodyN))
  doAssert caseStmtN.len > 1
  caseStmtN.add newTree(nnkElse,
    quote do: discard)
  result.add newTree(nnkCaseStmt, caseStmtN)
  when defined(reDumpMacro):
    echo "==== genSubmatchEoe ===="
    echo repr(result)

template submatchEoe(
  smA, smB, regex, c, c2,
  capts, charIdx, cPrev: untyped
): untyped =
  smB.clear()
  var captx: int32
  var matched = true
  for n, capt, start in smA.items:
    genSubmatchEoe(
      n, capt, start, smB, c, c2, matched, captx,
      capts, charIdx, cPrev, regex)
  swap smA, smB

template shortestMatch: untyped {.dirty.} =
  submatchEoe(smA, smB, regex, c.int32, -1'i32, capts, iPrev, cPrev)
  if smA.len > 0:
    return true
  swap smA, smB

template longestMatchInit: untyped {.dirty.} =
  var
    matchedLong = false
    captLong = -1'i32
    startLong = start
    iPrevLong = start

template longestMatchEnter: untyped {.dirty.} =
  submatchEoe(smA, smB, regex, c.int32, -1'i32, capts, iPrev, cPrev)
  if smA.len > 0:
    matchedLong = true
    captLong = smA[0][1]
    startLong = smA[0][2]
    iPrevLong = iPrev
  swap smA, smB

template longestMatchExit: untyped {.dirty.} =
  if not matchedLong:
    return false
  assert smA.len == 0
  when groupsCount > 0:
    constructSubmatches(m.captures, capts, captLong, groupsCount)
  when namedGroups.len > 0:
    m.namedGroups = namedGroups
  m.boundaries = startLong .. iPrevLong-1
  return true

template findMatch: untyped {.dirty.} =
  when mfLongestMatch in flags:
    if matchedLong and
        (smA.len == 0 or startLong < smA[0][2]):
      smA.clear()
      longestMatchExit()
  elif mfShortestMatch in flags:
    discard
  else:
    doAssert false
  smA.add((0'i16, -1'i32, i))

func matchImpl*(
  text: string,
  regex: static Regex,
  m: var RegexMatch,
  flags: static MatchFlags,
  start = 0
): bool {.inline.} =
  # workaround Nim/issues/13252
  const groupsCount = regex.groupsCount
  const namedGroups = regex.namedGroups
  m.clear()
  var
    smA, smB: Submatches
    capts: Capts
    c = Rune(-1)
    cPrev = -1'i32
    i = start
    iPrev = start
  when mfLongestMatch in flags:
    longestMatchInit()
  smA = newSubmatches(regex.nfa.len)
  smB = newSubmatches(regex.nfa.len)
  smA.add((0'i16, -1'i32, start))
  while i < len(text):
    fastRuneAt(text, i, c, true)
    #c = text[i].Rune
    #i += 1
    when mfShortestMatch in flags:
      shortestMatch()
    when mfLongestMatch in flags:
      longestMatchEnter()
    submatch(smA, smB, regex, c.int32, capts, iPrev, cPrev)
    when mfFindMatch in flags:
      findMatch()
    if smA.len == 0:
      when mfLongestMatch in flags:
        longestMatchExit()
      return false
    iPrev = i
    cPrev = c.int32
  submatchEoe(smA, smB, regex, -1'i32, -1'i32, capts, iPrev, cPrev)
  if smA.len == 0:
    when mfLongestMatch in flags:
      longestMatchExit()
    return false
  when groupsCount > 0:
    constructSubmatches(m.captures, capts, smA[0][1], groupsCount)
  when namedGroups.len > 0:
    m.namedGroups = namedGroups
  m.boundaries = smA[0][2] .. iPrev-1
  return true
