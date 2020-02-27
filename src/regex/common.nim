import unicode
import strutils

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
