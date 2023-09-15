import std/strutils
when (NimMajor, NimMinor) >= (1, 2):
  import std/hashes
  import std/os

import ./nfatype
import ./types

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
    for n2 in n.next:
      if isEpsilonTransition(nfa.s[n2]):
        if t.len > 0:
          t &= ", "
        t &= $nfa.s[n2]
        continue
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

when (NimMajor, NimMinor) >= (1, 2):
  func graphToFile*(regex: Regex, dir: string) =
    {.noSideEffect.}:
      if dir.len > 0:
        let content = graph(regex)
        let fname = $hash(content) & ".dot"
        try:
          writeFile(dir / fname, content)
        except IOError:
          debugEcho "write file error"
