# Package

version = "0.12.0"
author = "Esteban Castro Borsani (@nitely)"
description = "Linear time regex matching"
license = "MIT"
srcdir = "src"
skipDirs = @["tests"]

requires "nim >= 0.19.0"
requires "unicodedb >= 0.7.2"
requires "unicodeplus >= 0.5.0"

task test, "Test":
  exec "nim c -r src/regex.nim"
  exec "nim c -r tests/tests.nim"
  exec "nim c -r -d:forceRegexAtRuntime tests/tests.nim"
  when (NimMajor, NimMinor, NimPatch) >= (0, 20, 0):
    exec "nim c -d:runTestAtCT tests/tests.nim"
  #exec "nim js -r src/regex.nim"
  # These two fail with Node.js OOM error
  # https://github.com/nitely/nim-regex/issues/38
  #exec "nim js -r tests/tests.nim"
  #exec "nim js -r -d:forceRegexAtRuntime tests/tests.nim"

  # Test runnable examples
  exec "nim doc -o:./docs/ugh/ugh.html ./src/regex.nim"

task docs, "Docs":
  exec "nim doc -o:./docs/index.html ./src/regex.nim"
