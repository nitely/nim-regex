import std/unicode
import std/strutils

when (NimMajor, NimMinor, NimPatch) < (0, 20, 0):
  import sets
  proc initHashSet*[T](size = 2): HashSet[T] =
    result = initSet[T](size)
  proc toHashSet*[T](keys: openArray[T]): HashSet[T] =
    result = toSet[T](keys)

type
  RegexError* = object of ValueError
  ## raised when the pattern
  ## is not a valid regex

const
  # This is used as start
  # and end of string. It should
  # be invalid code, but while it
  # works it simplifies things a bit.
  # An alternative would be opt[Rune]
  # or just using int32 and convert
  # Rune to int when needed
  invalidRune* = Rune(-1)
  # `\n` is platform specific in
  # Nim and not the actual `\n`
  lineBreakRune* = Rune(10)

proc toRune*(s: string): Rune =
  result = s.runeAt(0)

proc `<=`*(x, y: Rune): bool =
  x.int <= y.int

proc cmp*(x, y: Rune): int =
  x.int - y.int

func bwRuneAt*(s: string, n: int): Rune =
  ## Take rune ending at ``n``
  doAssert n >= 0
  doAssert n <= s.len-1
  var n = n
  while n > 0 and s[n].ord shr 6 == 0b10:
    dec n
  fastRuneAt(s, n, result, false)

template bwFastRuneAt*(
  s: string, n: var int, result: var Rune
): untyped =
  ## Take rune ending at ``n``
  doAssert n > 0
  doAssert n <= s.len
  dec n
  while n > 0 and s[n].ord shr 6 == 0b10:
    dec n
  fastRuneAt(s, n, result, false)

proc `%%`*(
  formatstr: string,
  a: openArray[string]
): string {.noSideEffect, raises: [].} =
  ## same as ``"$#" % ["foo"]`` but
  ## returns empty string on error
  try:
    formatstr % a
  except ValueError:
    ""

proc `%%`*(formatstr: string, a: string): string =
  formatstr %% [a]

# XXX this is to support literal optimization
#     for unicode. It needs testing
when false:
  # XXX impl simpler find when memchr is not available?
  func find*(s: string, r: Rune, start: Natural = 0): int =
    ## Find unicode rune in a string.
    if r.ord < 0xff:
      return find(s, r.char, start)
    let c = (r.ord and 0xff).char
    let rsize = r.size()
    var i = start+rsize-1
    var r2 = 0'u32
    doAssert rsize >= 1 and rsize <= 4
    while i < len(s):
      i = find(s, c, i)
      if i == -1:
        return -1
      for j in i-rsize-1 .. i:
        r2 = (r2 shl 8) or s[j].uint32
      if r.uint32 == r2:
        return i-rsize-1
      r2 = 0
      inc i
    return -1
