## NFA matcher for static regexes

import macros
import unicode
import tables
import sets
import algorithm

import unicodedb/properties
import unicodedb/types

import nodetype
from nfamatch import 
  RegexFlag,
  Regex,
  MatchFlag,
  MatchFlags,
  RegexMatch

type
  CaptNode = object
    parent: int
    bound: int
    idx: int16
  Capts = seq[CaptNode]
  Captures* = seq[seq[Slice[int]]]

type
  NodeIdx = int16
  CaptIdx = int32
  Submatches = ref object
    ## Parallel states would be a better name.
    ## This is a sparse set
    sx: seq[(NodeIdx, CaptIdx)]
    ss: seq[int16]
    si: int16

func newSubmatches(size: int): Submatches {.inline.} =
  result = new Submatches
  result.sx = newSeq[(NodeIdx, CaptIdx)](8)
  result.ss = newSeq[int16](size)
  result.si = 0

when defined(release):
  {.push checks: off.}

func `[]`(sm: Submatches, i: int): (NodeIdx, CaptIdx) {.inline.} =
  assert i < sm.si
  sm.sx[i]

func hasState(sm: Submatches, n: int16): bool {.inline.} =
  sm.ss[n] < sm.si and sm.sx[sm.ss[n]][0] == n

func add(sm: var Submatches, item: (NodeIdx, CaptIdx)) {.inline.} =
  assert not sm.hasState(item[0])
  assert sm.si <= sm.sx.len
  if (sm.si == sm.sx.len).unlikely:
    sm.sx.setLen(sm.sx.len * 2)
  sm.sx[sm.si] = item
  sm.ss[item[0]] = sm.si
  sm.si += 1'i16

func len(sm: Submatches): int {.inline.} =
  sm.si

func clear(sm: var Submatches) {.inline.} =
  sm.si = 0

iterator items(sm: Submatches): (NodeIdx, CaptIdx) {.inline.} =
  for i in 0 .. sm.len-1:
    yield sm.sx[i]

when defined(release):
  {.pop.}

func constructSubmatches(
  captures: var Captures,
  capts: Capts,
  capt, size: int
) {.inline.} =
  template currGroup: untyped = captures[capts[capt].idx]
  captures.setLen(size)
  for i in 0 .. captures.len-1:
    captures[i].setLen(0)
  if capts.len == 0:
    return
  var capt = capt
  while capt != -1:
    if currGroup.len == 0:
      currGroup.add(-2 .. -2)
    if currGroup[^1].a != -2:
      currGroup.add(-2 .. -2)
    if currGroup[^1].b == -2:
      currGroup[^1].b = capts[capt].bound-1
    else:
      currGroup[^1].a = capts[capt].bound
    capt = capts[capt].parent
  for c in captures.mitems:
    c.reverse()

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
      unicodeTypes(`c`.Rune), utmWord))

func genDigitAsciiMatch(c: NimNode): NimNode =
  result = newStmtList()
  result.add quote do:
    '0'.ord <= `c` and `c` <= '9'.ord

func genDigitMatch(c: NimNode): NimNode =
  result = newStmtList()
  result.add quote do:
    ('0'.ord <= `c` and `c` <= '9'.ord) or
    (`c` > 128'i32 and contains(
      unicodeTypes(`c`.Rune), utmDecimal))

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
        unicodeTypes(`c`.Rune), utmWhiteSpace)

func genMatch(c: NimNode, n: Node): NimNode =
  let cpLit = newLit n.cp.int32
  result = case n.kind
    of reChar:
      quote do: `c` == `cpLit`
    of reWord:
      genWordMatch(c)
    of reNotAlphaNum:
      let wordMatch = genWordMatch(c)
      quote do: not (`wordMatch`)
    of reDigit:
      genDigitMatch(c)
    of reNotDigit:
      let digitMatch = genDigitMatch(c)
      quote do: not (`digitMatch`)
    of reWhiteSpace:
      genWhiteSpaceMatch(c)
    of reNotWhiteSpace:
      let whiteSpaceMatch = genWhiteSpaceMatch(c)
      quote do: not (`whiteSpaceMatch`)
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
      quote do: not (`wordMatch`)
    of reDigitAscii:
      genDigitAsciiMatch(c)
    of reNotDigitAscii:
      let digitMatch = genDigitAsciiMatch(c)
      quote do: not (`digitMatch`)
    of reWhiteSpaceAscii:
      genWhiteSpaceAsciiMatch(c)
    of reNotWhiteSpaceAscii:
      let whiteSpaceMatch = genWhiteSpaceAsciiMatch(c)
      quote do: not (`whiteSpaceMatch`)
    of reUCC:
      let cc = n.cc  # XXX newLit
      quote do: contains(
        cc, unicodeCategory(`c`.Rune))
    of reNotUCC:
      let cc = n.cc  # XXX newLit
      quote do: not contains(
        cc, unicodeCategory(`c`.Rune))
    else:
      doAssert false
      quote do: false

func genSetMatch(c: NimNode, n: Node): NimNode =
  assert n.kind == reInSet
  result = newStmtList()
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
      terms.add genMatch(c, nn)
  assert terms.len > 0
  let term = terms[0]
  var matchStmt = quote do:
    (`term`)
  for i in 1 .. terms.len-1:
    let term = terms[i]
    matchStmt = quote do:
      `matchStmt` or (`term`)
  result.add matchStmt

func genMatchedBody(
  smB, ntLit, capt, matched, captx,
  capts, charIdx, cPrev, c: NimNode,
  i, nti: int,
  regex: Regex
): NimNode =
  if regex.transitions.allZ[i][nti] == -1'i16:
    return quote do:
      add(`smB`, (`ntLit`, `capt`))
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
      matchedBody.add quote do:
        `matched` = `matched` and match(
          `z`, Rune(`cPrev`), Rune(`c`))
    else:
      doAssert false
  matchedBody.add quote do:
    if `matched`:
      add(`smB`, (`ntLit`, `captx`))
  return newStmtList matchedBody

macro genSubmatch(
  n, capt, smB, c, matched, captx,
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
          quote do: `c` >= 0'i32 and (`m`)
        of reNotSet:
          let m = genSetMatch(c, regex.nfa[nt])
          quote do: `c` >= 0'i32 and not (`m`)
        else:
          let m = genMatch(c, regex.nfa[nt])
          quote do: `c` >= 0'i32 and (`m`)
      let ntLit = newLit nt
      let matchedBodyStmt = genMatchedBody(
        smB, ntLit, capt, matched, captx,
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
  for n, capt in smA.items:
    genSubmatch(
      n, capt, smB, c, matched, captx,
      capts, charIdx, cPrev, regex)
  swap smA, smB

macro genSubmatchEoe(
  n, capt, smB, c, matched, captx,
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
          smB, ntLit, capt, matched, captx,
          capts, charIdx, cPrev, c,
          i, nti, regex)
        branchBodyN.add quote do:
          if `c` == -1'i32 and not hasState(`smB`, `ntLit`):
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
  smA, smB, regex, c,
  capts, charIdx, cPrev: untyped
): untyped =
  smB.clear()
  var captx: int32
  var matched = true
  for n, capt in smA.items:
    genSubmatchEoe(
      n, capt, smB, c, matched, captx,
      capts, charIdx, cPrev, regex)
  swap smA, smB

func clear(m: var RegexMatch) {.inline.} =
  if m.captures.len > 0:
    m.captures.setLen(0)
  if m.namedGroups.len > 0:
    m.namedGroups.clear()
  m.boundaries = 0 .. -1

func matchImpl*(
  text: string,
  regex: static Regex,
  m: var RegexMatch,
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
  smA = newSubmatches(regex.nfa.len)
  smB = newSubmatches(regex.nfa.len)
  smA.add((0'i16, -1'i32))
  while i < len(text):
    #fastRuneAt(text, i, c, true)
    c = text[i].Rune
    i += 1
    submatch(smA, smB, regex, c.int32, capts, iPrev, cPrev)
    if smA.len == 0:
      return false
    iPrev = i
    cPrev = c.int32
  submatchEoe(smA, smB, regex, -1'i32, capts, iPrev, cPrev)
  if smA.len == 0:
    return false
  when groupsCount > 0:
    constructSubmatches(m.captures, capts, smA[0][1], groupsCount)
  when namedGroups.len > 0:
    m.namedGroups = namedGroups
  m.boundaries = start .. iPrev-1
  return true
