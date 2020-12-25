import ./nodetype

type
  Exp* = object
    s*: seq[Node]
  AtomsExp* = object
    s*: seq[Node]
  RpnExp* = object
    s*: seq[Node]
