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
  for i, n in pairs nfa.s:
    result.add tab
    result.add($i & " [label=\"q" & $i & "\";color=" & n.color & "];")
    result.add '\n'
  for i, n in pairs nfa.s:
    if n.next.len == 0:
      continue
    result.add tab
    for i2, n2 in pairs n.next:
      var t = ""
      if nfa.t.allZ[i][i2] > -1:
        for i3, z in pairs nfa.t.z[nfa.t.allZ[i][i2]]:
          if i3 > 0: t &= ", "
          t &= $z
        t = ", {" & t & "}"
      let label = ($nfa.s[n2] & t & ", i=" & $i2).replace(r"\", r"\\")
      result.add($i & " -> " & $n2 & " [label=\"" & label & "\"];")
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
