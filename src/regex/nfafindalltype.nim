import std/unicode
import std/tables

import ./nodetype
import ./nfatype

func bwRuneAt*(s: string, n: int): Rune =
  ## Take rune ending at ``n``
  doAssert n >= 0
  doAssert n <= s.len-1
  var n = n
  while n > 0 and s[n].ord shr 6 == 0b10:
    dec n
  fastRuneAt(s, n, result, false)

type
  MatchItemIdx* = int
  MatchItem* = tuple
    capt: CaptIdx
    bounds: Bounds
  Matches* = object
    s*: seq[MatchItem]
    i*: int
  RegexMatches* = object
    a*, b*: Submatches
    m*: Matches
    c*: Capts

func len*(ms: Matches): int {.inline.} =
  ms.i

# XXX make it log(n)? altough this does
#     not add to the time complexity
func add*(ms: var Matches, m: MatchItem) {.inline.} =
  ## Add `m` to `ms`. Remove all overlapped matches.
  var size = 0
  for i in countdown(ms.len-1, 0):
    if max(ms.s[i].bounds.b, ms.s[i].bounds.a) < m.bounds.a:
      size = i+1
      break
  ms.i = size
  if ms.i <= ms.s.len-1:
    ms.s[ms.i] = m
  else:
    ms.s.add m
  ms.i += 1

func clear*(ms: var Matches) {.inline.} =
  ms.i = 0

template initMaybeImpl*(
  ms: var RegexMatches,
  size: int
) =
  if ms.a == nil:
    assert ms.b == nil
    ms.a = newSubmatches(size)
    ms.b = newSubmatches(size)
  doAssert ms.a.cap >= size and
    ms.b.cap >= size

func hasMatches*(ms: RegexMatches): bool {.inline.} =
  return ms.m.len > 0

func clear*(ms: var RegexMatches) {.inline.} =
  ms.a.clear()
  ms.b.clear()
  ms.m.clear()
  ms.c.setLen 0

func len*(ms: var RegexMatches): int {.inline.} =
  ms.m.len

iterator bounds*(ms: RegexMatches): Slice[int] {.inline.} =
  for i in 0 .. ms.m.len-1:
    yield ms.m.s[i].bounds

iterator items*(ms: RegexMatches): MatchItemIdx {.inline.} =
  for i in 0 .. ms.m.len-1:
    yield i

func fillMatchImpl*(
  m: var RegexMatch,
  mi: MatchItemIdx,
  ms: RegexMatches,
  regex: Regex
) {.inline.} =
  if m.namedGroups.len != regex.namedGroups.len:
    m.namedGroups = regex.namedGroups
  #constructSubmatches(
  #  m.captures, ms.c, ms.m.s[mi].capt, regex.groupsCount)
  m.boundaries = ms.m.s[mi].bounds
