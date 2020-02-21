# Package

version = "0.13.0"
author = "Esteban Castro Borsani (@nitely)"
description = "Linear time regex matching"
license = "MIT"
srcDir = "src"
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
  # js target should work in older versions, but
  # the docker image for CI has it since Nim 1.0.4,
  # so I'll only test it there
  when (NimMajor, NimMinor, NimPatch) >= (1, 0, 4):
    exec "nim js -r --styleCheck:off src/regex.nim"
    exec "nim js -r --styleCheck:off tests/tests.nim"
    exec "nim js -r --styleCheck:off -d:forceRegexAtRuntime tests/tests.nim"

  # Test runnable examples
  exec "nim doc -o:./docs/ugh/ugh.html ./src/regex.nim"

task docs, "Docs":
  exec "nim doc --project -o:./docs ./src/regex.nim"
