import regex

proc isMatch*(s: string, pattern: Regex): bool =
  s.match(pattern).isSome

proc toStrCaptures*(
    m: Option[RegexMatch],
    s: string): seq[seq[string]] =
  assert m.isSome
  let mm = m.get()
  result = newSeq[seq[string]](mm.groupsCount)
  var j = 0
  for i in 0 ..< mm.groupsCount:
    result[i] = newSeq[string](mm.group(i).len)
    j = 0
    for cbounds in mm.group(i):
      result[i][j] = s[cbounds]
      inc j

proc matchWithCapt*(s: string, pattern: Regex): seq[seq[string]] =
  s.match(pattern).toStrCaptures(s)

proc findWithCapt*(s: string, pattern: Regex): seq[seq[string]] =
  s.find(pattern).toStrCaptures(s)

proc findAllb*(s: string, pattern: Regex): seq[Slice[int]] =
  result = newSeqOfCap[Slice[int]](s.len)
  for m in findAll(s, pattern):
    result.add(m.boundaries)

proc raises*(pattern: string): bool =
  result = false
  try:
    discard pattern.toPattern()
  except RegexError:
    result = true

proc raisesMsg*(pattern: string): string =
  try:
    discard pattern.toPattern()
  except RegexError as e:
    result = e.msg
