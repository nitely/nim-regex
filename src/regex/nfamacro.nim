## NFA matcher for static regexes

import macros
import unicode
import tables
import algorithm

import nodematch
import nodetype
import nfa
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
  Submatches = ref object
    ## Parallel states would be a better name
    sx: seq[(NodeIdx, CaptIdx)]
    # use custom len because setLen(0) is slower,
    # and {.noInit.} makes no difference
    si: int
    ss: set[int16]

func newSubmatches(): Submatches {.inline.} =
  result = new Submatches
  result.sx = newSeq[(NodeIdx, CaptIdx)](8)
  result.si = 0

func `[]`(sm: Submatches, i: int): (NodeIdx, CaptIdx) {.inline.} =
  assert i < sm.si
  sm.sx[i]

func add(sm: var Submatches, item: (NodeIdx, CaptIdx)) {.inline.} =
  assert item[0] notin sm.ss
  assert sm.si <= sm.sx.len
  if (sm.si == sm.sx.len).unlikely:
    sm.sx.setLen(sm.sx.len * 2)
  sm.sx[sm.si] = item
  sm.si += 1 
  sm.ss.incl(item[0])

func len(sm: Submatches): int {.inline.} =
  sm.si

func hasState(sm: Submatches, n: int16): bool {.inline.} =
  n in sm.ss

func clear(sm: var Submatches) {.inline.} =
  for i in 0 .. sm.len-1:
    assert sm.sx[i][0] in sm.ss
    sm.ss.excl sm.sx[i][0]
  sm.si = 0

iterator items(sm: Submatches): (NodeIdx, CaptIdx) {.inline.} =
  for i in 0 .. sm.len-1:
    yield sm.sx[i]

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
      var matchBody: seq[NimNode]
      case regex.nfa[nt].kind
        of reChar:
          let cpLit = newLit regex.nfa[nt].cp.int32
          matchBody.add(quote do:
            if `c` == `cpLit`:
              add(`smB`, (`ntLit`, `capt`)))
        of reWord:
          # XXX fixme
          let trueLit = newLit true
          matchBody.add(quote do:
            if `trueLit`:
              add(`smB`, (`ntLit`, `capt`)))
        of reEoe:
          let cpLit = newLit -1.int32
          matchBody.add(quote do:
            if `c` == `cpLit`:
              add(`smB`, (`ntLit`, `capt`)))
        # XXX add the rest of matchers
        else:
          doAssert false
      let matchBodyStmt = newStmtList matchBody
      branchBodyN.add(quote do:
        if not hasState(`smB`, `ntLit`):
          `matchBodyStmt`)
    doAssert branchBodyN.len > 0
    caseStmtN.add(newTree(nnkOfBranch,
      newLit i.int16,
      newStmtList(
        branchBodyN)))
  caseStmtN.add(newTree(nnkElse,
    newStmtList(
      newTree(nnkDiscardStmt, newEmptyNode()))))
  result.add(newTree(nnkCaseStmt, caseStmtN))
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

func submatch2(
  smA, smB: var Submatches,
  regex: static Regex,
  c: int32
) {.inline.} =
  template t: untyped {.dirty.} = regex.transitions
  template nfa: untyped {.dirty.} = regex.nfa
  smB.clear()
  for n, capt in smA.items:
    for nti, nt in nfa[n].next.pairs:
      if smB.hasState(nt):
        continue
      # XXX do not use match for charKind in macro
      if not match(nfa[nt], c.Rune):
        continue
      smB.add((nt, capt))
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
  smA = newSubmatches()
  smB = newSubmatches()
  smA.add((0'i16, -1'i32))
  while i < len(text):
    fastRuneAt(text, i, c, true)
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
