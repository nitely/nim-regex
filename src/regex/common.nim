import std/unicode
import std/strutils
import std/algorithm

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

func toRune*(c: char): Rune =
  result = Rune(c.ord)

func `<=`*(x, y: Rune): bool =
  x.int32 <= y.int32

func cmp*(x, y: Rune): int =
  x.int32 - y.int32

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
  result = -1
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

type
  SortedSeq*[T] = object
    s: seq[T]

func initSortedSeq*[T]: SortedSeq[T] {.inline.} =
  SortedSeq[T](s: @[])

func len*[T](s: SortedSeq[T]): int {.inline.} =
  s.s.len

func add*[T](s: var SortedSeq[T], x: openArray[T]) =
  if x.len == 0:
    return
  s.s.add x
  sort s.s, cmp

func contains*[T](s: SortedSeq[T], x: T): bool =
  if s.len <= 10:
    return x in s.s
  return binarySearch(s.s, x, cmp) != -1

iterator items*[T](s: SortedSeq[T]): T {.inline.} =
  for i in 0 .. s.s.len-1:
    yield s.s[i]


when isMainModule:
  block:
    var s = initSortedSeq[int]()
    doAssert s.s.len == 0
    s.add @[2,1,3]
    doAssert s.s == @[1,2,3]
    s.add @[5,4,6,7]
    doAssert s.s == @[1,2,3,4,5,6,7]
  block:
    var s = initSortedSeq[int]()
    doAssert s.len == 0
    s.add @[2,1,3]
    doAssert s.len == 3
  block:
    var s = initSortedSeq[int]()
    doAssert 1 notin s
    s.add @[2,1,3]
    doAssert 1 in s
    doAssert 2 in s
    doAssert 3 in s
    doAssert 4 notin s
    doAssert 0 notin s
  block:
    var s = initSortedSeq[int]()
    s.add @[2,1,3]
    var ss = newSeq[int]()
    for x in s:
      ss.add x
    doAssert ss == @[1,2,3]
  block:
    var nums = newSeq[int]()
    for x in 100 .. 200:
      nums.add x
    for x in 0 .. 100:
      nums.add x
    var s = initSortedSeq[int]()
    s.add nums
    for x in 0 .. 200:
      doAssert x in s
  echo "ok"