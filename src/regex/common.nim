import std/unicode
import std/strutils
import std/algorithm
when defined(nimPreviewSlimSystem):
  import std/assertions

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

when NimMajor < 2:
  # Copied from std/unicode in Nim's stdlib because Nim older than ver 2
  # doesn't have `fastRuneAt`/`runeAt` with `openArray[char]` param.

  template ones(n: untyped): untyped = ((1 shl n)-1)
  const replRune = Rune(0xFFFD)

  template fastRuneAt*(s: openArray[char], i: int, result: untyped, doInc = true) =
    ## Returns the rune ``s[i]`` in ``result``.
    ##
    ## If ``doInc == true`` (default), ``i`` is incremented by the number
    ## of bytes that have been processed.
    bind ones
    if uint(s[i]) <= 127:
      result = Rune(uint(s[i]))
      when doInc: inc(i)
    elif uint(s[i]) shr 5 == 0b110:
      # assert(uint(s[i+1]) shr 6 == 0b10)
      if i <= s.len - 2:
        result = Rune((uint(s[i]) and (ones(5))) shl 6 or
                      (uint(s[i+1]) and ones(6)))
        when doInc: inc(i, 2)
      else:
        result = replRune
        when doInc: inc(i)
    elif uint(s[i]) shr 4 == 0b1110:
      # assert(uint(s[i+1]) shr 6 == 0b10)
      # assert(uint(s[i+2]) shr 6 == 0b10)
      if i <= s.len - 3:
        result = Rune((uint(s[i]) and ones(4)) shl 12 or
                      (uint(s[i+1]) and ones(6)) shl 6 or
                      (uint(s[i+2]) and ones(6)))
        when doInc: inc(i, 3)
      else:
        result = replRune
        when doInc: inc(i)
    elif uint(s[i]) shr 3 == 0b11110:
      # assert(uint(s[i+1]) shr 6 == 0b10)
      # assert(uint(s[i+2]) shr 6 == 0b10)
      # assert(uint(s[i+3]) shr 6 == 0b10)
      if i <= s.len - 4:
        result = Rune((uint(s[i]) and ones(3)) shl 18 or
                      (uint(s[i+1]) and ones(6)) shl 12 or
                      (uint(s[i+2]) and ones(6)) shl 6 or
                      (uint(s[i+3]) and ones(6)))
        when doInc: inc(i, 4)
      else:
        result = replRune
        when doInc: inc(i)
    elif uint(s[i]) shr 2 == 0b111110:
      # assert(uint(s[i+1]) shr 6 == 0b10)
      # assert(uint(s[i+2]) shr 6 == 0b10)
      # assert(uint(s[i+3]) shr 6 == 0b10)
      # assert(uint(s[i+4]) shr 6 == 0b10)
      if i <= s.len - 5:
        result = Rune((uint(s[i]) and ones(2)) shl 24 or
                  (uint(s[i+1]) and ones(6)) shl 18 or
                  (uint(s[i+2]) and ones(6)) shl 12 or
                  (uint(s[i+3]) and ones(6)) shl 6 or
                  (uint(s[i+4]) and ones(6)))
        when doInc: inc(i, 5)
      else:
        result = replRune
        when doInc: inc(i)
    elif uint(s[i]) shr 1 == 0b1111110:
      # assert(uint(s[i+1]) shr 6 == 0b10)
      # assert(uint(s[i+2]) shr 6 == 0b10)
      # assert(uint(s[i+3]) shr 6 == 0b10)
      # assert(uint(s[i+4]) shr 6 == 0b10)
      # assert(uint(s[i+5]) shr 6 == 0b10)
      if i <= s.len - 6:
        result = Rune((uint(s[i]) and ones(1)) shl 30 or
                      (uint(s[i+1]) and ones(6)) shl 24 or
                      (uint(s[i+2]) and ones(6)) shl 18 or
                      (uint(s[i+3]) and ones(6)) shl 12 or
                      (uint(s[i+4]) and ones(6)) shl 6 or
                      (uint(s[i+5]) and ones(6)))
        when doInc: inc(i, 6)
      else:
        result = replRune
        when doInc: inc(i)
    else:
      result = Rune(uint(s[i]))
      when doInc: inc(i)

  proc runeAt*(s: openArray[char], i: Natural): Rune =
    fastRuneAt(s, i, result, false)

func bwRuneAt*(s: openArray[char], n: int): Rune =
  ## Take rune ending at ``n``
  doAssert n >= 0
  doAssert n <= s.len-1
  var n = n
  while n > 0 and s[n].ord shr 6 == 0b10:
    dec n
  fastRuneAt(s, n, result, false)

template bwFastRuneAt*(
  s: openArray[char], n: var int, result: var Rune
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
func verifyUtf8*(s: openArray[char]): int =
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

func find*(a: SkipTable, s, sub: openArray[char], start: Natural = 0, last = -1): int =
  # copied from std/strutils as it doesn't have `find` for `openArray[char]`.
  let
    last = if last < 0: s.high else: last
    subLast = sub.len - 1

  if subLast == -1:
    # this was an empty needle string,
    # we count this as match in the first possible position:
    return start

  # This is an implementation of the Boyer-Moore Horspool algorithms
  # https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm
  result = -1
  var skip = start

  while last - skip >= subLast:
    var i = subLast
    while s[skip + i] == sub[i]:
      if i == 0:
        return skip
      dec i
    inc skip, a[s[skip + subLast]]

when not (defined(js) or defined(nimdoc) or defined(nimscript)):
  from system/ansi_c import c_memchr

  const hasCStringBuiltin = true
else:
  const hasCStringBuiltin = false

func find*(s: openArray[char], sub: char, start: Natural = 0, last = -1): int =
  # copied from std/strutils as it doesn't have `find` for `openArray[char]`.
  result = -1
  let last = if last < 0: s.high else: last

  template findImpl =
    for i in int(start)..last:
      if s[i] == sub:
        return i

  when nimvm:
    findImpl()
  else:
    when hasCStringBuiltin:
      let length = last-start+1
      if length > 0:
        let found = c_memchr(unsafeAddr s[start], cint(sub), cast[csize_t](length))
        if not found.isNil:
          return cast[int](found) -% cast[int](unsafeAddr s[0])
    else:
      findImpl()

when defined(linux):
  proc memmem(haystack: pointer, haystacklen: csize_t,
              needle: pointer, needlelen: csize_t): pointer {.importc, header: """#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <string.h>""".}
elif defined(bsd) or (defined(macosx) and not defined(ios)):
  proc memmem(haystack: pointer, haystacklen: csize_t,
              needle: pointer, needlelen: csize_t): pointer {.importc, header: "#include <string.h>".}

func find*(s: openArray[char], sub: string, start: Natural = 0, last = -1): int =
  # copied from std/strutils as it doesn't have `find` for `openArray[char]`.
  if sub.len > s.len - start: return -1
  if sub.len == 1: return find(s, sub[0], start, last)

  template useSkipTable =
    result = find(initSkipTable(sub), s, sub, start, last)

  when nimvm:
    useSkipTable()
  else:
    when declared(memmem):
      let subLen = sub.len
      if last < 0 and start < s.len and subLen != 0:
        let found = memmem(unsafeAddr s[start], csize_t(s.len - start), unsafeAddr sub[0], csize_t(subLen))
        result = if not found.isNil:
            cast[int](found) -% cast[int](unsafeAddr s[0])
          else:
            -1
      else:
        useSkipTable()
    else:
      useSkipTable()

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