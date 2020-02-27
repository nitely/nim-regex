import unicode
import sets
import tables
import deques

import nodematch
import nodetype
import common

type
  AlphabetSym* = int32
  Closure* = HashSet[int16]
  DfaRow* = Table[AlphabetSym, int32]
  DfaClosure* = Table[AlphabetSym, int32]
  Dfa* = object
    table*: seq[DfaRow]
    cs*: seq[Closure]
    closures*: seq[DfaClosure]

const
  symEoe* = -1'i32
  symWord* = -3'i32
  symDigit* = -4'i32
  symAny* = -6'i32
  symAnyNl* = -7'i32

func createAlphabet(nfa: seq[Node]): seq[AlphabetSym] =
  var inAlphabet: HashSet[AlphabetSym]
  # speedup ascii matching
  for c in 0 .. 128:
    result.add(c.int32)
    inAlphabet.incl(c.int32)
  # special symbols
  result.add(symEoe)
  result.add(symWord)
  result.add(symDigit)
  result.add(symAny)
  result.add(symAnyNl)
  # expression chars
  for n in nfa:
    case n.kind
    of reChar:
      if n.cp.int32 notin inAlphabet:
        result.add(n.cp.int32)
        inAlphabet.incl(n.cp.int32)
    of reCharCI:
      if n.cp.int32 notin inAlphabet:
        result.add(n.cp.int32)
        inAlphabet.incl(n.cp.int32)
      let cp2 = n.cp.swapCase()
      if cp2.int32 notin inAlphabet:
        result.add(cp2.int32)
        inAlphabet.incl(cp2.int32)
    of reInSet:
      for cp in n.cps:
        if cp.int32 notin inAlphabet:
          result.add(cp.int32)
          inAlphabet.incl(cp.int32)
      for rg in n.ranges:
        for cp in rg:
          if cp.int32 notin inAlphabet:
            result.add(cp.int32)
            inAlphabet.incl(cp.int32)
    else:
      discard
  assert result.toHashSet.len == result.len

func delta(
  nfa: seq[Node],
  states: Closure,
  sym: AlphabetSym
): Closure =
  result = initHashSet[int16](2)
  if sym > -1:
    for s in states:
      if match(nfa[s], sym.Rune):
        result.incl(s)
  else:
    # XXX this will add every sym for reAny, but we should only add symAny
    let kinds = case sym
      of symEoe: {reEoe}
      of symWord: {reAnyNl, reAny, reWord}
      of symDigit: {reAnyNl, reAny, reWord, reDigit}
      of symAny: {reAnyNl, reAny}
      of symAnyNl: {reAnyNl}
      else: {}
    for s in states:
      if nfa[s].kind in kinds:
        result.incl(s)
      if nfa[s].kind == reInSet:
        for sh in nfa[s].shorthands:
          if sh.kind in kinds:
            result.incl(s)
            break

func dfa*(
  nfa: seq[Node],
  alphabet: var seq[AlphabetSym]
): Dfa =
  ## Powerset construction
  template closure(result, states) =
    for s in states:
      for sn in nfa[s].next:
        result.incl(sn)
  alphabet = createAlphabet(nfa)
  let n0 = 0
  var q0: Closure
  closure(q0, [n0])
  var qw = initDeque[Closure]()
  qw.addFirst(q0)
  var qu = initTable[Closure, int32]()
  var quPos = 0'i32
  qu[q0] = quPos
  inc quPos
  result.table.add(initTable[AlphabetSym, int32](2))
  result.closures.add(initTable[AlphabetSym, int32](2))
  var t = initHashSet[int16]()
  var csRev = initTable[Closure, int32]()
  while qw.len > 0:
    let qa = qw.popLast()
    for sym in alphabet:
      let s = delta(nfa, qa, sym)
      if s.len == 0:
        continue
      t.clear()
      closure(t, s)
      if t notin qu:
        qu[t] = quPos
        inc quPos
        qw.addFirst(t)
        result.table.add(initTable[AlphabetSym, int32](2))
        result.closures.add(initTable[AlphabetSym, int32](2))
      result.table[qu[qa]][sym] = qu[t]
      if s in csRev:
        result.closures[qu[qa]][sym] = csRev[s]
      else:
        result.closures[qu[qa]][sym] = result.cs.len.int32
        csRev[s] = result.cs.len.int32
        result.cs.add(s)
  assert result.table.len == result.closures.len
  assert result.cs.toHashSet.len == result.cs.len

func minDfaTable(
  dfa: Dfa,
  p: seq[HashSet[int32]]
): Dfa {.inline.} =
  ## Construct DFA from Hopcroft partitions.
  ## This is O(N*A) where N is the number of
  ## DFA states and A the size of the alphabet,
  ## albeit it can be faster since not every state
  ## has a transition on every alphabet symbol
  # map DFA states to min-DFA states
  var statesMap = newSeq[int32](dfa.table.len)
  for i in 0 .. statesMap.len-1:
    statesMap[i] = -1
  for ri, r in p.pairs:
    for q in r:
      assert statesMap[q] == -1
      statesMap[q] = ri.int32
  # construct min-DFA table
  result.table.setLen(p.len)
  result.closures.setLen(p.len)
  var csRev = initTable[Closure, int32]()
  var closures = initTable[AlphabetSym, Closure]()
  for ri, r in p.pairs:
    assert r.len > 0
    result.table[ri] = initTable[AlphabetSym, int32](2)
    result.closures[ri] = initTable[AlphabetSym, int32](2)
    closures.clear()
    for q in r:
      for c, q2 in dfa.table[q].pairs:
        assert c notin result.table[ri] or
          result.table[ri][c] == statesMap[q2]
        result.table[ri][c] = statesMap[q2]
        if c notin closures:
          closures[c] = initHashSet[int16](2)
        closures[c].incl(dfa.cs[dfa.closures[q][c]])
    for c, closure in closures.pairs:
      if closure in csRev:
        result.closures[ri][c] = csRev[closure]
      else:
        result.closures[ri][c] = result.cs.len.int32
        csRev[closure] = result.cs.len.int32
        result.cs.add(closure)      
  assert result.table.len == result.closures.len
  assert result.cs.toHashSet.len == result.cs.len

func reverse(dfa: Dfa): Dfa {.inline.} =
  ## return reversed dfa table
  result.table.setLen(dfa.table.len)
  for i in 0 .. dfa.table.len-1:
    result.table[i] = initTable[AlphabetSym, int32](2)
  for i, t in dfa.table.pairs:
    for c, q in t.pairs:
      # add dup key, for multiple 
      # in-transitions of same symbol
      result.table[q].add(c, i.int32)

func xF(dfa: Dfa): HashSet[int32] {.inline.} =
  ## return all final states
  result = initHashSet[int32](2)
  for i, t in dfa.table.pairs:
    if symEoe in t:
      result.incl(i.int32)
  doAssert result.len > 0

func xQ(dfa: Dfa): HashSet[int32] {.inline.} =
  ## return all states
  result = initHashSet[int32](2)
  for i in 0'i32 .. (dfa.table.len-1).int32:
    result.incl(i)

func delta(
  dfa: Dfa,
  s: HashSet[int32],
  c: AlphabetSym
): HashSet[int32] {.inline.} =
  ## return set of states that can reach `s` on `c`,
  ## expects the reversed dfa
  result = initHashSet[int32](2)
  for q in s:
    for q2 in dfa.table[q].allValues(c):
      result.incl(q2)

func canPartition(r, i: HashSet[int32]): bool {.inline.} =
  ## return true if:
  ## * intersection of R and I is not empty,
  ## * and the complement of R and I is not empty
  var intr = 0
  for q in r:
    intr += int(q in i)
  result = 0 < intr and intr < r.len

func partition(
  r, i: HashSet[int32]
): (HashSet[int32], HashSet[int32]) {.inline.} =
  ## partition r into r1 and r2, such as r1 is the intersection
  ## of r and i, and r2 is r - such intersection
  result = (
    initHashSet[int32](2),
    initHashSet[int32](2))
  for x in r:
    if x in i:
      result[0].incl(x)
    else:
      result[1].incl(x)

# without minimize
# 43745 lines compiled; 8.679 sec total; 256.574MiB peakmem
# unoptimized minimize
# 43746 lines compiled; 35.277 sec total; 309.113MiB peakmem;
# removing p[p.find(r)] and (r in w)
# 43746 lines compiled; 32.970 sec total; 319.766MiB peakmem;
# removing two (r - i) intersections of hashSets
# 43756 lines compiled; 16.145 sec total; 308.73MiB peakmem;
# dfa.reverse and init all hashsets to 2, except q-f is 64
# 43779 lines compiled; 12.209 sec total; 309.234MiB peakmem
# optimized dfa table construction
# 43825 lines compiled; 11.985 sec total; 258.664MiB peakmem;
func minimize*(
  dfa: Dfa,
  alphabet: seq[AlphabetSym]
): Dfa =
  ## Hopcroft
  template r: untyped {.dirty.} = p[ri]
  let dfaRev = dfa.reverse()
  let f = dfa.xF()
  let q = dfa.xQ()
  var w: seq[HashSet[int32]]
  w.add(f)
  w.add(q - f)
  var p: seq[HashSet[int32]]
  p.add(f)
  p.add(q - f)
  while w.len > 0:
    let s = w.pop
    for c in alphabet:  # XXX take alphabet from `for q in s: dfa[q]`
      let i = delta(dfaRev, s, c)
      if i.len == 0:
        continue
      for ri in 0 .. p.len-1:
        if not canPartition(r, i):
          continue
        let wi = w.find r
        let (r1, r2) = partition(r, i)
        r = r1
        p.add r2
        if wi > -1:
          w[wi] = r1
          w.add r2
        elif r1.len <= r2.len:
          w.add r1
        else:
          w.add r2
  assert p.len <= dfa.table.len, "not a min DFA, wtf?"
  # make the initial state the first state
  var ri0 = -1
  for ri, r in p.pairs:
    if 0 in r:
      ri0 = ri
      break
  assert ri0 > -1
  swap p[0], p[ri0]
  result = minDfaTable(dfa, p)
