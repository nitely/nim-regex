import ./parser
import ./exptransformation
import ./nodetype
import ./nfatype
import ./nfa
import ./litopt
when defined(regexDotDir):
  import ./dotgraph

when false:
  func pass2(s: string, nodes: var seq[Node]) =
    ## Resolve lookarounds
    for n in mitems nodes:
      if n.kind in lookaroundKind:
        var nodes2 = s.parse(n.ab.a, n.ab.b)
        pass2(s, nodes2)
        var groups: GroupsCapture
        let rpn = nodes2.transformExp(groups)
        var transitions: Transitions
        let nfa = rpn.nfa2(transitions)
        n.subExp.nfa = nfa
        n.subExp.tns = transitions

func reImpl*(s: string): Regex {.inline.} =
  var groups: GroupsCapture
  let rpn = s
    .parse
    .transformExp(groups)
  let nfa = rpn.nfa2()
  let opt = rpn.litopt2()
  result = Regex(
    nfa: nfa,
    groupsCount: groups.count,
    namedGroups: groups.names,
    litOpt: opt)
  when defined(regexDotDir) and (NimMajor, NimMinor) >= (1, 2):
    const regexDotDir {.strdefine.} = ""
    graphToFile(result, regexDotDir)

func reCt*(s: string): Regex {.compileTime.} =
  reImpl(s)
