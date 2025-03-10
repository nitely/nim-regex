import ./common
import ./parser
import ./exptransformation
import ./types
import ./nfatype
import ./nfa
import ./litopt
when defined(regexDotDir):
  import ./dotgraph

func reImpl*(s: string, flags: RegexFlags = {}): Regex =
  if regexArbitraryBytes notin flags and verifyUtf8(s) != -1:
    raise newException(RegexError, "Invalid utf-8 regex")
  var groups = default(GroupsCapture)
  let rpn = s
    .parse(flags)
    .transformExp(groups, flags)
  let nfa = rpn.nfa2()
  let opt = rpn.litopt3(flags)
  result = Regex(
    nfa: nfa,
    groupsCount: groups.count,
    namedGroups: groups.names,
    flags: flags,
    litOpt: opt
  )
  when defined(regexDotDir):
    const regexDotDir {.strdefine.} = ""
    graphToFile(result, regexDotDir)

func reCt*(s: string, flags: RegexFlags = {}): Regex {.compileTime.} =
  reImpl(s, flags)
