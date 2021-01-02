## NFA matcher for static regexes

import std/macros
import std/unicode
import std/tables
import std/sets

import pkg/unicodedb/properties
import pkg/unicodedb/types as utypes

import ./types
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

type
  AheadSig = proc (
    smA, smB, capts, captIdx, matched, text, start: NimNode,
    nfa: Nfa,
    look: Lookaround,
    flags: set[MatchFlag]
  ): NimNode {.noSideEffect, raises: [].}
  BehindSig = proc (
    smA, smB, capts, captIdx, text, start: NimNode,
    nfa: Nfa,
    look: Lookaround,
    flags: set[MatchFlag]
  ): NimNode {.noSideEffect, raises: [].}
  Lookaround = object
    ahead: AheadSig
    behind: BehindSig

func bwRuneAt(s: string, n: int): Rune =
  ## Take rune ending at ``n``
  doAssert n >= 0
  doAssert n <= s.len-1
  var n = n
  while n > 0 and s[n].ord shr 6 == 0b10:
    dec n
  fastRuneAt(s, n, result, false)

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
  nfa: Nfa
): NimNode =
  template t: untyped = nfa.t
  if t.allZ[i][nti] == -1'i16:
    return quote do:
      add(`smB`, (`ntLit`, `capt`, `bounds`.a .. `charIdx`-1))
  var matchedBody: seq[NimNode]
  matchedBody.add quote do:
    `matched` = true
    `captx` = `capt`
  for z in t.z[t.allZ[i][nti]]:
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

func genNextState(
  n, capt, bounds, smB, c, matched, captx,
  capts, charIdx, cPrev: NimNode,
  nfa: Nfa,
  flags: set[MatchFlag],
  eoeOnly: bool
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
  template s: untyped = nfa.s
  result = newStmtList()
  var caseStmtN: seq[NimNode]
  caseStmtN.add n
  for i in 0 .. s.len-1:
    if s[i].kind == reEoe:
      continue
    var branchBodyN: seq[NimNode]
    for nti, nt in s[i].next.pairs:
      if eoeOnly and s[nt].kind != reEoe:
        continue
      let matchCond = case s[nt].kind
        of reEoe:
          quote do: `c` == -1'i32
        of reInSet:
          let m = genSetMatch(c, s[nt])
          quote do: `c` >= 0'i32 and `m`
        of reNotSet:
          let m = genSetMatch(c, s[nt])
          quote do: `c` >= 0'i32 and not `m`
        else:
          let m = genMatch(c, s[nt])
          quote do: `c` >= 0'i32 and `m`
      let ntLit = newLit nt
      let matchedBodyStmt = genMatchedBody(
        smB, ntLit, capt, bounds, matched, captx,
        capts, charIdx, cPrev, c,
        i, nti, nfa)
      if mfAnchored in flags and s[nt].kind == reEoe:
        branchBodyN.add quote do:
          if not hasState(`smB`, `ntLit`):
            `matchedBodyStmt`
      else:
        branchBodyN.add quote do:
          if not hasState(`smB`, `ntLit`) and `matchCond`:
            `matchedBodyStmt`
    doAssert eoeOnly or branchBodyN.len > 0
    if branchBodyN.len > 0:
      caseStmtN.add newTree(nnkOfBranch,
        newLit i.int16,
        newStmtList(
          branchBodyN))
  doAssert caseStmtN.len > 1
  let elseAssertion = if eoeOnly:
    newEmptyNode()
  else:
    quote do: doAssert false
  caseStmtN.add newTree(nnkElse,
    quote do:
      `elseAssertion`
      discard)
  result.add newTree(nnkCaseStmt, caseStmtN)
  when defined(reDumpMacro):
    echo "==== genNextState ===="
    echo repr(result)

func nextState(
  smA, smB, c,
  capts, charIdx, cPrev,
  captx, matched, eoe: NimNode,
  nfa: Nfa,
  flags: set[MatchFlag],
  eoeOnly = false
): NimNode =
  defForVars n, capt, bounds
  var eoeBailOut = newEmptyNode()
  if mfAnchored in flags:
    eoeBailOut = quote do:
      if `n` == `eoe` and not hasState(`smB`, `n`):
        add(`smB`, (`n`, `capt`, `bounds`))
      break
  let nextStateStmt = genNextState(
    n, capt, bounds, smB, c, matched, captx,
    capts, charIdx, cPrev, nfa, flags, eoeOnly)
  result = quote do:
    `smB`.clear()
    for `n`, `capt`, `bounds` in `smA`.items:
      `eoeBailOut`
      `nextStateStmt`
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

func eoeIdx(nfa: Nfa): int16 =
  for i in 0 .. nfa.s.len-1:
    if nfa.s[i].kind == reEoe:
      return i.int16
  doAssert false

func matchImpl(
  smA, smB, capts, captIdx, matched, text, start: NimNode,
  nfa: Nfa,
  look: Lookaround,
  flags: set[MatchFlag]
): NimNode =
  defVars c, iPrev, cPrev, captx
  let eoe = newLit nfa.eoeIdx
  let c2 = quote do: int32(`c`)
  let nextStateStmt = nextState(
    smA, smB, c2, capts, iPrev, cPrev, captx,
    matched, eoe, nfa, flags)
  let cEoe = newLit -1'i32
  let nextStateEoeStmt = nextState(
    smA, smB, cEoe, capts, iPrev, cPrev, captx,
    matched, eoe, nfa, flags, eoeOnly = true)
  result = quote do:
    var
      `c` = Rune(-1)
      `cPrev` = -1'i32
      `iPrev` = `start`
      `captx` {.used.} = -1'i32
      i = `start`
    if `start`-1 in 0 .. `text`.len-1:  # XXX anchored only?
      `cPrev` = bwRuneAt(`text`, `start`-1).int32
    clear(`smA`)
    add(`smA`, (0'i16, `captIdx`, i .. i-1))
    while i < `text`.len:
      fastRuneAt(`text`, i, `c`, true)
      `nextStateStmt`
      if `smA`.len == 0:
        break
      if `smA`[0].ni == `eoe`:
        break
      `iPrev` = i
      `cPrev` = `c2`
    `nextStateEoeStmt`
    if `smA`.len > 0:
      `captIdx` = `smA`[0].ci
    `matched` = `smA`.len > 0

when false:
  func reversedMatchImpl(
    smA, smB, capts, captIdx, text, start, limit: NimNode,
    nfa: Nfa,
    look: Lookaround
  ): NimNode =
    defVars c, iPrev, cPrev, captx
    let c2 = quote do: int32(`c`)
    let nextStateBwStmt = nextStateBw(
      smA, smB, c2, capts, iPrev, cPrev, captx, matched, nfa)
    let nextStateEoeStmt = nextStateEoe(
      smA, smB, capts, iPrev, cPrev, captx, matched, nfa)
    let nfaLenLit = newLit nfa.s.len
    let eoe = newLit nfa.eoeIdx
    result = quote do:
      doAssert start >= limit
      var
        `c` = Rune(-1)
        `cPrev` = -1'i32
        `iPrev` = `start`
        `captx` {.used.} = -1'i32
        i = `start`
      if `start` in 0 .. `text`.len-1:
        `cPrev` = runeAt(`text`, `start`).int32
      clear(`smA`)
      add(`smA`, (0'i16, `captIdx`, i .. i-1))
      while i > `limit`:
        bwFastRuneAt(`text`, i, `c`)
        `nextStateBwStmt`
        if `smA`.len == 0:
          break
        if `smA`[0].ni == `eoe`:
          break
        `iPrev` = i
        `cPrev` = `c2`
      `nextStateEoeStmt`
      if `smA`.len > 0:
        `captIdx` = `smA`[0].ci
      `matched` = `smA`.len > 0

template look: untyped =
  Lookaround(
    ahead: matchImpl)#,
    #behind: reversedMatchImpl)

proc matchImpl*(text, expLit, body: NimNode): NimNode =
  if not (expLit.kind == nnkCallStrLit and $expLit[0] == "rex"):
    error "not a regex literal; only rex\"regex\" is allowed", expLit
  let exp = expLit[1]
  defVars smA, smB, capts, capt, matched
  let regex = reCt(exp.strVal)
  let startLit = newLit 0
  let flags: set[MatchFlag] = {}
  let matchImplStmt = matchImpl(
    smA, smB, capts, capt, matched,
    text, startLit, regex.nfa, look, flags)
  let nfaLenLit = newLit regex.nfa.s.len
  let nfaGroupsLen = regex.groupsCount
  result = quote do:
    block:
      var
        `smA` = newSubmatches `nfaLenLit`
        `smB` = newSubmatches `nfaLenLit`
        `capts`: Capts
        `capt` = -1'i32
        `matched` = false
      `matchImplStmt`
      if `matched`:
        var matches {.used, inject.}: seq[string]
        when `nfaGroupsLen` > 0:
          constructSubmatches2(
            matches, `text`, `capts`, `capt`, `nfaGroupsLen`)
        `body`
