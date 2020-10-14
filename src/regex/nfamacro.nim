## NFA matcher for static regexes

import std/macros
import std/unicode
import std/tables
import std/sets

import pkg/unicodedb/properties
import pkg/unicodedb/types

import ./nodetype
import ./nfatype
import ./compiler

macro defVars(idns: varargs[untyped]): untyped =
  var lets = newNimNode nnkLetSection
  for idn in idns:
    lets.add newIdentDefs(
      idn, newEmptyNode(), newCall("genSym", newLit nskVar, newLit $idn))
  return newStmtList lets

macro defForVars(idns: varargs[untyped]): untyped =
  var lets = newNimNode nnkLetSection
  for idn in idns:
    lets.add newIdentDefs(
      idn, newEmptyNode(), newCall("genSym", newLit nskForVar, newLit $idn))
  return newStmtList lets

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
  smB, ntLit, capt, bounds, matched, captx,
  capts, charIdx, cPrev, c: NimNode,
  i, nti: int,
  regex: Regex
): NimNode =
  if regex.transitions.allZ[i][nti] == -1'i16:
    return quote do:
      add(`smB`, (`ntLit`, `capt`, `bounds`.a .. `charIdx`-1))
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
      add(`smB`, (`ntLit`, `captx`, `bounds`.a .. `charIdx`-1))
  return newStmtList matchedBody

func genSubmatch(
  n, capt, bounds, smB, c, matched, captx,
  capts, charIdx, cPrev: NimNode,
  regex: Regex
): NimNode =
  #[
    case n
    of 0:
      if not smB.hasState(1):
        if c == 'a':
          smB.add((1, capt, bounds))
      if not smB.hasState(4):
        if c == 'b':
          smB.add((4, capt, bounds))
    of 1:
      ...
    else:
      error
  ]#
  result = newStmtList()
  var caseStmtN: seq[NimNode]
  caseStmtN.add n
  for i in 0 .. regex.nfa.len-1:
    if regex.nfa[i].kind == reEoe:
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
        smB, ntLit, capt, bounds, matched, captx,
        capts, charIdx, cPrev, c,
        i, nti, regex)
      branchBodyN.add quote do:
        if not hasState(`smB`, `ntLit`) and `matchCond`:
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

func submatch(
  smA, smB, c,
  capts, charIdx, cPrev,
  captx, matched: NimNode,
  regex: Regex
): NimNode =
  defForVars n, capt, bounds
  let genSubmatchCall = genSubmatch(
    n, capt, bounds, smB, c, matched, captx,
    capts, charIdx, cPrev, regex)
  result = quote do:
    `smB`.clear()
    for `n`, `capt`, `bounds` in `smA`.items:
      `genSubmatchCall`
    swap `smA`, `smB`

func genSubmatchEoe(
  n, capt, bounds, smB, matched, captx,
  capts, charIdx, cPrev: NimNode,
  regex: Regex
): NimNode =
  # This is the same as genSubmatch
  # but just for EOE states
  #[
    case n
    of 0:
      if not smB.hasState(1):
        if c == -1:
          smB.add((1, capt, bounds))
    of 1:
      ...
    else:
      discard
  ]#
  result = newStmtList()
  var caseStmtN: seq[NimNode]
  caseStmtN.add n
  for i in 0 .. regex.nfa.len-1:
    if regex.nfa[i].kind == reEoe:
      continue
    var branchBodyN: seq[NimNode]
    for nti, nt in regex.nfa[i].next.pairs:
      if regex.nfa[nt].kind == reEoe:
        let ntLit = newLit nt
        let cLit = newLit -1'i32
        let matchedBodyStmt = genMatchedBody(
          smB, ntLit, capt, bounds, matched, captx,
          capts, charIdx, cPrev, cLit,
          i, nti, regex)
        branchBodyN.add quote do:
          if not hasState(`smB`, `ntLit`):
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

func submatchEoe(
  smA, smB,
  capts, charIdx, cPrev,
  captx, matched: NimNode,
  regex: Regex
): NimNode =
  defForVars n, capt, bounds
  let genSubmatchEoeCall = genSubmatchEoe(
    n, capt, bounds, smB, matched, captx,
    capts, charIdx, cPrev, regex)
  result = quote do:
    `smB`.clear()
    for `n`, `capt`, `bounds` in `smA`.items:
      `genSubmatchEoeCall`
    swap `smA`, `smB`

template constructSubmatches2(
  captures, txt, capts, capt, size: untyped
): untyped =
  var bounds: array[size, Slice[int]]
  for i in 0 .. bounds.len-1:
    bounds[i] = -2 .. -3
  var captx = capt
  while captx != -1:
    if bounds[capts[captx].idx].b == -3:
      bounds[capts[captx].idx].b = capts[captx].bound-1
    elif bounds[capts[captx].idx].a == -2:
      bounds[capts[captx].idx].a = capts[captx].bound
    captx = capts[captx].parent
  captures.setLen size
  for i in 0 .. bounds.len-1:
    captures[i] = txt[bounds[i]]

proc matchImpl*(text, expLit, body: NimNode): NimNode =
  if not (expLit.kind == nnkCallStrLit and $expLit[0] == "rex"):
    error "not a regex literal; only rex\"regex\" is allowed", expLit
  let exp = expLit[1]
  defVars smA, smB, c, capts, iPrev, cPrev, captx, matched
  let c2 = quote do: int32(`c`)
  let regex = reCt(exp.strVal)
  let submatchCall = submatch(
    smA, smB, c2, capts, iPrev, cPrev, captx, matched, regex)
  let submatchEoeCall = submatchEoe(
    smA, smB, capts, iPrev, cPrev, captx, matched, regex)
  let nfaLenLit = newLit regex.nfa.len
  let nfaGroupsLen = regex.groupsCount
  result = quote do:
    block:
      var
        `smA`, `smB`: Submatches
        `c` = Rune(-1)
        `cPrev` = -1'i32
        `iPrev` = 0
        `capts` {.used.}: Capts
        `captx` {.used.}: int32
        `matched` {.used.}: bool
        i = 0
      `smA` = newSubmatches `nfaLenLit`
      `smB` = newSubmatches `nfaLenLit`
      add(`smA`, (0'i16, -1'i32, 0 .. -1))
      while i < len(`text`):
        fastRuneAt(`text`, i, `c`, true)
        `submatchCall`
        if `smA`.len == 0:
          break
        `iPrev` = i
        `cPrev` = `c2`
      `submatchEoeCall`
      if `smA`.len > 0:
        var matches {.used, inject.}: seq[string]
        when `nfaGroupsLen` > 0:
          constructSubmatches2(
            matches, `text`, `capts`, `smA`[0].ci, `nfaGroupsLen`)
        `body`
