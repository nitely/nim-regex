import std/macros
import std/unicode
import std/tables
from std/strutils import find

import ./nodemacro
import ./nodetype
import ./nfatype
import ./compiler
import ./nfafindalltype

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
      # https://github.com/nim-lang/Nim/issues/13266
      #let zLit = newLit z
      let matchCond = genMatch(`z`, `cPrev`, `c`)
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
    capts, charIdx, cPrev: NimNode,
    regex: Regex)
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
      if regex.nfa[nt].kind == reEoe:
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
    capts, charIdx, cPrev: NimNode,
    regex: Regex)
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
  text, expLit, ms, isOpt: NimNode
): NimNode =
  defVars c, i, cPrev
  let exp = expLit[1]
  let regex = reCt(exp.strVal)
  let nfaLenLit = newLit regex.nfa.len
  let smA = quote do: `ms`.a
  let c2 = quote do: int32(`c`)
  let submatchStmt = submatch(regex, ms, i, cPrev, c2)
  let submatchEoeStmt = submatchEoe(regex, ms, i, cPrev)
  return quote do:
    initMaybeImpl(`ms`, `nfaLenLit`)
    `ms`.clear()
    var
      `c` = Rune(-1)
      `cPrev` = -1'i32
      `i` = 0
      iPrev = 0
    `smA`.add (0'i16, -1'i32, `i` .. `i`-1)
    while `i` < len(`text`):
      fastRuneAt(`text`, `i`, `c`, true)
      `submatchStmt`
      if `smA`.len == 0:
        if `i` < len(`text`):
          if `ms`.hasMatches():
            return `i`
          if `isOpt`:
            return `i`
      `smA`.add (0'i16, -1'i32, `i` .. `i`-1)
      iPrev = `i`
      `cPrev` = `c`.int32
    `submatchEoeStmt`
    doAssert `smA`.len == 0
    if `ms`.hasMatches():
      return `i`
    return -1
