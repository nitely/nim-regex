import ./parser
import ./exptransformation
import ./types
import ./nfatype
import ./nfa
import ./litopt
when defined(regexDotDir):
  import ./dotgraph

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
