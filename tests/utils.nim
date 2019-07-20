import regex

proc isMatch*(s: string, pattern: Regex): bool =
  var m: RegexMatch
  result = match(s, pattern, m)

proc toStrCaptures*(m: RegexMatch, s: string): seq[seq[string]] =
  result = newSeq[seq[string]](m.groupsCount)
  var j = 0
  for i in 0 ..< m.groupsCount:
    result[i] = newSeq[string](m.group(i).len)
    j = 0
    for cbounds in m.group(i):
      result[i][j] = s[cbounds]
      inc j

proc matchWithCapt*(s: string, pattern: Regex): seq[seq[string]] =
  var m: RegexMatch
  doAssert match(s, pattern, m)
  result = m.toStrCaptures(s)

proc findWithCapt*(s: string, pattern: Regex): seq[seq[string]] =
  var m: RegexMatch
  doAssert find(s, pattern, m)
  result = m.toStrCaptures(s)

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
  except RegexError:
    result = getCurrentExceptionMsg()
