import regex/parser
import regex/exptransformation
import regex/nfatype
import regex/nfa
import regex/litopt

template reImpl(s: untyped): Regex =
  var groups: GroupsCapture
  let rpn = s
    .parse
    .transformExp(groups)
  var transitions: Transitions
  let nfa = rpn.nfa2(transitions)
  let opt = rpn.litopt2()
  Regex(
    nfa: nfa,
    transitions: transitions,
    groupsCount: groups.count,
    namedGroups: groups.names,
    litOpt: opt)

func re*(s: string): Regex {.compileTime.} =
  reImpl(s)
