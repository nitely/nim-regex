import std/hashes
import std/os
import std/strutils

import ./nfa
import ./nfatype
import ./nodetype

func color(n: Node): string =
  case n.kind
  of matchableKind: "black"
  else: "blue"

func graph*(nfa: Nfa, tns: Transitions): string =
  result = "digraph graphname {\n"
  let tab = "    "
  for i, n in pairs nfa:
    result.add tab
    result.add($i & " [label=\"q" & $i & "\";color=" & n.color & "];")
    result.add '\n'
  for i, n in pairs nfa:
    if n.next.len == 0:
      continue
    result.add tab
    for i2, n2 in pairs n.next:
      var t = ""
      if tns.allZ[i][i2] > -1:
        for i3, z in pairs tns.z[tns.allZ[i][i2]]:
          if i3 > 0: t &= ", "
          t &= $z
        t = ", {" & t & "}"
      let label = ($nfa[n2] & t & ", i=" & $i2).replace(r"\", r"\\")
      result.add($i & " -> " & $n2 & " [label=\"" & label & "\"];")
    result.add '\n'
  result.add "}\n"

func graph*(regex: Regex): string =
  result = graph(regex.nfa, regex.transitions)

func graphToFile*(regex: Regex, dir: string) =
  {.noSideEffect.}:
    if dir.len > 0:
      let content = graph(regex)
      let fname = $hash(content) & ".dot"
      try:
        writeFile(dir / fname, content)
      except IOError:
        debugEcho "write file error"
