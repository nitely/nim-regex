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

type
  verifyUtf8State = enum
    vusError, vusStart, vusA, vusB, vusC, vusD, vusE, vusF, vusG

# Taken from nim-unicodeplus
func verifyUtf8*(s: string): int =
  ## Return `-1` if `s` is a valid utf-8 string.
  ## Otherwise, return the index of the first bad char.
  var state = vusStart
  var i = 0
  let L = s.len
  while i < L:
    case state:
    of vusStart:
      result = i
      state = if uint8(s[i]) in 0x00'u8 .. 0x7F'u8: vusStart
      elif uint8(s[i]) in 0xC2'u8 .. 0xDF'u8: vusA
      elif uint8(s[i]) in 0xE1'u8 .. 0xEC'u8 or uint8(s[i]) in 0xEE'u8 .. 0xEF'u8: vusB
      elif uint8(s[i]) == 0xE0'u8: vusC
      elif uint8(s[i]) == 0xED'u8: vusD
      elif uint8(s[i]) in 0xF1'u8 .. 0xF3'u8: vusE
      elif uint8(s[i]) == 0xF0'u8: vusF
      elif uint8(s[i]) == 0xF4'u8: vusG
      else: vusError
    of vusA:
      state = if uint8(s[i]) in 0x80'u8 .. 0xBF'u8: vusStart else: vusError
    of vusB:
      state = if uint8(s[i]) in 0x80'u8 .. 0xBF'u8: vusA else: vusError
    of vusC:
      state = if uint8(s[i]) in 0xA0'u8 .. 0xBF'u8: vusA else: vusError
    of vusD:
      state = if uint8(s[i]) in 0x80'u8 .. 0x9F'u8: vusA else: vusError
    of vusE:
      state = if uint8(s[i]) in 0x80'u8 .. 0xBF'u8: vusB else: vusError
    of vusF:
      state = if uint8(s[i]) in 0x90'u8 .. 0xBF'u8: vusB else: vusError
    of vusG:
      state = if uint8(s[i]) in 0x80'u8 .. 0x8F'u8: vusB else: vusError
    of vusError:
      break
    inc i
  if state == vusStart:
    result = -1
