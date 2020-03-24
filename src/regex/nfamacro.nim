## NFA matcher for static regexes

import macros
import unicode
import tables
import sets

import unicodedb/properties
import unicodedb/types

import nodematch
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
    idx: int
  Capts = seq[CaptNode]
  Captures* = seq[seq[Slice[int]]]

type
  NodeIdx = int16
  CaptIdx = int32
  # ref makes this marginally faster,
  # the swap improvement makes up for
  # the indirection
  Submatches = ref object
    ## Parallel states would be a better name
    sx: seq[(NodeIdx, CaptIdx)]
    si: int16
    # This used to be a set[int16] but it was slower
    ss: seq[int16]

func newSubmatches(size: int): Submatches {.inline.} =
  result = new Submatches
  result.sx = newSeq[(NodeIdx, CaptIdx)](8)
  result.ss = newSeq[int16](size)
  result.si = 0

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

func genWordMatch(c: NimNode): NimNode =
  result = newStmtList()
  result.add quote do:
    case `c`
    of -1.int32:
      false
    of 'a'.ord .. 'z'.ord,
        'A'.ord .. 'Z'.ord,
        '0'.ord .. '9'.ord,
        '_'.ord:
      true
    else:
      contains(unicodeTypes(`c`.Rune), utmWord)

func genMatch(c: NimNode, n: Node): NimNode =
  let cpLit = newLit n.cp.int32
  let eoeLit = newLit -1.int32
  result = case n.kind
    of reChar:
      quote do: `c` == `cpLit`
    of reWord:
      genWordMatch(c)
    of reAny:
      quote do: `c` != `eoeLit`
    of reEoe:
      quote do: `c` == `eoeLit`
    # XXX add the remaining matchers
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
        (`a` <= `c` and `c` <= `b`)
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
  var matchStmt = terms[0]
  for i in 1 .. terms.len-1:
    let term = terms[i]
    matchStmt = quote do:
      `matchStmt` or `term`
  result.add matchStmt

macro genSubmatch(
  n, capt, smB, c: typed,
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
      let ntLit = newLit nt
      let matchCond = case regex.nfa[nt].kind
        of reInSet:
          genSetMatch(c, regex.nfa[nt])
        else:
          genMatch(c, regex.nfa[nt])
      branchBodyN.add quote do:
        if not hasState(`smB`, `ntLit`):
          if `matchCond`:
            add(`smB`, (`ntLit`, `capt`))
    doAssert branchBodyN.len > 0
    caseStmtN.add newTree(nnkOfBranch,
      newLit i.int16,
      newStmtList(
        branchBodyN))
  caseStmtN.add newTree(nnkElse,
    quote do:
      doAssert false
      discard)
  result.add newTree(nnkCaseStmt, caseStmtN)
  when defined(reDumpMacro):
    echo "==== genSubmatch ===="
    echo repr(result)

template submatch(
  smA, smB, regex, c: untyped
): untyped =
  smB.clear()
  for n, capt in smA.items:
    genSubmatch(n, capt, smB, c, regex)
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
    submatch(smA, smB, regex, c.int32)
    if smA.len == 0:
      return false
    iPrev = i
    cPrev = c.int32
  # XXX gen for EOE only (smaller code)
  submatch(smA, smB, regex, -1'i32)
  if smA.len == 0:
    return false
  m.boundaries = start .. iPrev-1
  return true
