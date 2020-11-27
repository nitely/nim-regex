import std/macros
import std/unicode
import std/tables
#from std/strutils import find

import ./nodetype
import ./nfatype
import ./compiler
import ./nfafindalltype
import ./nodematchmacro

macro defVars(idns: varargs[untyped]): untyped =
  var lets = newNimNode nnkLetSection
  for idn in idns:
    lets.add newIdentDefs(
      idn, newEmptyNode(), newCall("genSym", newLit nskVar, newLit $idn))
  return newStmtList lets

func genMatchedBody(
  smA, smB, m, ntLit, capt, bounds,
  matched, captx, eoeFound, smi,
  capts, charIdx, cPrev, c: NimNode,
  i, nti, nt: int,
  regex: Regex
): NimNode =
  template nfa: untyped = regex.nfa
  template tns: untyped = regex.transitions
  let eoeStmt = case nfa[nt].kind:
    of reEoe:
      quote do:
        `m`.add (`captx`, `bounds`.a .. `i`-1)
        `smA`.clear()
        if not `eoeFound`:
          `eoeFound` = true
          `smA`.add (0'i16, -1'i32, `i` .. `i`-1)
        `smi` = 0
        continue
    else: newEmptyNode()
  if tns.allZ[i][nti] == -1'i16:
    if nfa[nt].kind == reEoe:
      return eoeStmt
    else:
      return quote do:
        add(`smB`, (`ntLit`, `capt`, `bounds`.a .. `charIdx`-1))
  var matchedBody: seq[NimNode]
  matchedBody.add quote do:
    `matched` = true
    `captx` = `capt`
  for z in tns.z[tns.allZ[i][nti]]:
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
      let matchCond = genMatch(z, cPrev, c)
      matchedBody.add quote do:
        `matched` = `matched` and `matchCond`
    else:
      doAssert false
  matchedBody.add quote do:
    if `matched`:
      `eoeStmt`
      add(`smB`, (`ntLit`, `captx`, `bounds`.a .. `charIdx`-1))
  return newStmtList matchedBody

func genSubmatch(
  n, capt, bounds, smA, smB, m, c,
  matched, captx, eoeFound, smi,
  capts, charIdx, cPrev: NimNode,
  regex: Regex
): NimNode =
  template nfa: untyped = regex.nfa
  result = newStmtList()
  var caseStmtN: seq[NimNode]
  caseStmtN.add n
  for i in 0 .. nfa.len-1:
    if nfa[i].kind == reEoe:
      continue
    var branchBodyN: seq[NimNode]
    for nti, nt in nfa[i].next.pairs:
      let matchCond = case nfa[nt].kind
        of reEoe:
          quote do: true
        of reInSet:
          let m = genSetMatch(c, nfa[nt])
          quote do: `c` >= 0'i32 and `m`
        of reNotSet:
          let m = genSetMatch(c, nfa[nt])
          quote do: `c` >= 0'i32 and not `m`
        else:
          let m = genMatch(c, nfa[nt])
          quote do: `c` >= 0'i32 and `m`
      let ntLit = newLit nt
      let matchedBodyStmt = genMatchedBody(
        smA, smB, m, ntLit, capt, bounds,
        matched, captx, eoeFound, smi,
        capts, charIdx, cPrev, c,
        i, nti, nt, regex)
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

func submatch(
  regex: Regex,
  ms, charIdx, cPrev, c: NimNode
): NimNode =
  defVars captx, matched, eoeFound, smi
  let smA = quote do: `ms`.a
  let smB = quote do: `ms`.b
  let capts = quote do: `ms`.c
  let m = quote do: `ms`.m
  let n = quote do: `ms`.a[`smi`].ni
  let capt = quote do: `ms`.a[`smi`].ci
  let bounds = quote do: `ms`.a[`smi`].bounds
  let submatchStmt = genSubmatch(
    n, capt, bounds, smA, smB, m, c,
    matched, captx, eoeFound, smi,
    capts, charIdx, cPrev, regex)
  result = quote do:
    `smB`.clear()
    var `captx`: int32
    var `matched` = true
    var `eoeFound` = false
    var `smi` = 0
    while `smi` < `smA`.len:
      `submatchStmt`
      `smi` += 1
    swap `smA`, `smB`

func genSubmatchEoe(
  n, capt, bounds, smA, smB, m,
  matched, captx, eoeFound, smi,
  capts, charIdx, cPrev: NimNode,
  regex: Regex
): NimNode =
  template nfa: untyped = regex.nfa
  result = newStmtList()
  var caseStmtN: seq[NimNode]
  caseStmtN.add n
  for i in 0 .. nfa.len-1:
    if nfa[i].kind == reEoe:
      continue
    var branchBodyN: seq[NimNode]
    for nti, nt in nfa[i].next.pairs:
      if nfa[nt].kind == reEoe:
        let ntLit = newLit nt
        let cLit = newLit -1'i32
        let matchedBodyStmt = genMatchedBody(
          smA, smB, m, ntLit, capt, bounds,
          matched, captx, eoeFound, smi,
          capts, charIdx, cPrev, cLit,
          i, nti, nt, regex)
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

func submatchEoe(
  regex: Regex,
  ms, charIdx, cPrev: NimNode
): NimNode =
  defVars captx, matched, eoeFound, smi
  let smA = quote do: `ms`.a
  let smB = quote do: `ms`.b
  let capts = quote do: `ms`.c
  let m = quote do: `ms`.m
  let n = quote do: `ms`.a[`smi`].ni
  let capt = quote do: `ms`.a[`smi`].ci
  let bounds = quote do: `ms`.a[`smi`].bounds
  let submatchStmt = genSubmatchEoe(
    n, capt, bounds, smA, smB, m,
    matched, captx, eoeFound, smi,
    capts, charIdx, cPrev, regex)
  result = quote do:
    `smB`.clear()
    var `captx`: int32
    var `matched` = true
    var `eoeFound` = false
    var `smi` = 0
    while `smi` < `smA`.len:
      `submatchStmt`
      `smi` += 1
    swap `smA`, `smB`

proc findSomeImpl*(
  text, expLit, ms, i, isOpt: NimNode
): NimNode =
  defVars c, cPrev
  let exp = expLit[1]
  let regex = reCt(exp.strVal)
  let nfaLenLit = newLit regex.nfa.len
  let smA = quote do: `ms`.a
  let c2 = quote do: int32(`c`)
  let submatchStmt = submatch(regex, ms, i, cPrev, c2)
  let submatchEoeStmt = submatchEoe(regex, ms, i, cPrev)
  return quote do:
    #block:
    initMaybeImpl(`ms`, `nfaLenLit`)
    `ms`.clear()
    var
      `c` = Rune(-1)
      `cPrev` = -1'i32
      iPrev = 0
    `smA`.add (0'i16, -1'i32, `i` .. `i`-1)
    if 0 <= `i`-1 and `i`-1 <= len(`text`)-1:
      `cPrev` = bwRuneAt(`text`, `i`-1).int32
    while `i` < len(`text`):
      fastRuneAt(`text`, `i`, `c`, true)
      `submatchStmt`
      if `smA`.len == 0:
        if `i` < len(`text`):
          if hasMatches(`ms`) or `isOpt`:
            break
      `smA`.add (0'i16, -1'i32, `i` .. `i`-1)
      iPrev = `i`
      `cPrev` = `c`.int32
    if `i` >= len(`text`):
      `submatchEoeStmt`
      doAssert `smA`.len == 0
    if not hasMatches(`ms`):
      `i` = -1

proc findAllItImpl*(fr: NimNode): NimNode =
  expectKind fr, nnkForStmt
  doAssert fr[^2].len == 3, "Expected two parameters"
  #result = newStmtList()
  #result.add newVarStmt(fr[0], countStart)
  var body = fr[^1]
  if body.kind != nnkStmtList:
    body = newTree(nnkStmtList, body)
  defVars ms, i
  let txt = fr[^2][1]
  let exp = fr[^2][2]
  let isOpt = quote do: false
  let findSomeStmt = findSomeImpl(txt, exp, ms, i, isOpt)
  echo repr(findSomeStmt)
  result = quote do:
    block:
      var `i` = 0
      var i2 = -1
      var `ms`: RegexMatches
      while `i` <= len(`txt`):
        doAssert(`i` > i2); i2 = `i`
        `findSomeStmt`
        if `i` < 0: break
        for mi in items(`ms`):
          #fillMatchImpl(m, mi, `ms`, pattern)
          `body`
        if `i` == len(`txt`): break
