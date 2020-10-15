import ./parser
import ./exptransformation
import ./nfatype
import ./nfa
import ./litopt

func reImpl*(s: string): Regex {.inline.} =
  var groups: GroupsCapture
  let rpn = s
    .parse
    .transformExp(groups)
  var transitions: Transitions
  let nfa = rpn.nfa2(transitions)
  let opt = rpn.litopt2()
  result = Regex(
    nfa: nfa,
    transitions: transitions,
    groupsCount: groups.count,
    namedGroups: groups.names,
    litOpt: opt)

func reCt*(s: string): Regex {.compileTime.} =
  reImpl(s)
