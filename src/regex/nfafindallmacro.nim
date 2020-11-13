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

func genMatchedBody2(
  smA, smB, ntLit, capt, bounds, matched, captx,
  capts, charIdx, cPrev, c: NimNode,
  i, nti: int,
  regex: Regex
): NimNode =
  template tns: untyped = regex.transitions



func genSubmatch2(
  n, capt, bounds, smA, smB, c, matched, captx,
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
        smA, smB, ntLit, capt, bounds, matched, captx,
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

func submatch(
  regex: Regex,
  ms, charIdx, cPrev, c: NimNode
): NimNode =
  defVars captx, matched, eoeFound, smi
  let smA = quote do: `ms`.a
  let smB = quote do: `ms`.b
  let capts = quote do: `ms`.c
  let n = quote do: `ms`.a[`smi`].ni
  let capt = quote do: `ms`.a[`smi`].ci
  let bounds = quote do: `ms`.a[`smi`].bounds
  let genSubmatch2Body = genSubmatch2(
    n, capt, bounds, smA, smB, c, matched, captx,
    capts, charIdx, cPrev: NimNode,
    regex: Regex)
  result = quote do:
    `smB`.clear()
    var `captx`: int32
    var `matched` = true
    var `eoeFound` = false
    var `smi` = 0
    while `smi` < `smA`.len:
      `genSubmatch2Body`
      `smi` += 1
    swap `smA`, `smB`
