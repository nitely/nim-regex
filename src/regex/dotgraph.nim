import std/strutils
import std/hashes
import std/os

import ./nfatype
import ./types

func getEpsilonTransitions(nfa: Nfa, n: Node, nti: int): seq[int] =
  doAssert not isEpsilonTransition(n)
  doAssert nti <= n.next.len-1
  result = newSeq[int]()
  for i in nti+1 .. n.next.len-1:
    if not isEpsilonTransition(nfa.s[n.next[i]]):
      break
    result.add n.next[i]

func color(n: Node): string =
  case n.kind
  of matchableKind: "black"
  else: "blue"

func graph*(nfa: Nfa): string =
  result = "digraph graphname {\n"
  let tab = "    "
  var qi = 0
  for i, n in pairs nfa.s:
    if isEpsilonTransition(n):
      continue
    result.add tab
    result.add($i & " [label=\"q" & $qi & "\";color=" & n.color & "];")
    result.add '\n'
    inc qi
  for i, n in pairs nfa.s:
    if n.next.len == 0:
      continue
    if isEpsilonTransition(n):
      continue
    result.add tab
    var t = ""
    var ii = 0
    for nti, n2 in pairs n.next:
      if isEpsilonTransition(nfa.s[n2]):
        continue
      for n3 in getEpsilonTransitions(nfa, n, nti):
        if t.len > 0:
          t &= ", "
        t &= $nfa.s[n3]
      if t.len > 0:
        t = ", {" & t & "}"
      let label = ($nfa.s[n2] & t & ", i=" & $ii).replace(r"\", r"\\")
      result.add($i & " -> " & $n2 & " [label=\"" & label & "\"];")
      t = ""
      inc ii
    result.add '\n'
  result.add "}\n"

func graph*(regex: Regex): string =
  result = graph(regex.nfa)

func graphToFile*(regex: Regex, dir: string) =
  {.noSideEffect.}:
    if dir.len > 0:
      let content = graph(regex)
      let fname = $hash(content) & ".dot"
      try:
        writeFile(dir / fname, content)
      except IOError:
        debugEcho "write file error"
