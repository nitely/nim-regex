# Package

version = "0.20.2"
author = "Esteban Castro Borsani (@nitely)"
description = "Linear time regex matching"
license = "MIT"
srcDir = "src"
skipDirs = @["tests", "bench", "docs"]

requires "nim >= 1.0.0"
requires "unicodedb >= 0.7.2"

task test, "Test":
  exec "nim c -r -o:bin/regex src/regex.nim"
  exec "nim c -r -o:bin/litopt src/regex/litopt.nim"
  exec "nim c -r tests/tests.nim"
  exec "nim c -r -d:forceRegexAtRuntime tests/tests.nim"
  exec "nim c -r -d:forceRegexAtRuntime -d:noRegexOpt tests/tests.nim"
  exec "nim c -r -d:noRegexOpt tests/tests.nim"
  when (NimMajor, NimMinor, NimPatch) >= (0, 20, 2):
    exec "nim c -d:runTestAtCT tests/tests.nim"
  # js target should work in older versions, but
  # the docker image for CI has it since Nim 1.0.4,
  # so I'll only test it there
  when (NimMajor, NimMinor, NimPatch) >= (1, 0, 4) and
      (NimMajor, NimMinor) != (1, 4):  # issue #88
    exec "nim js -r src/regex.nim"
    exec "nim js -r tests/tests.nim"
    exec "nim js -r -d:forceRegexAtRuntime tests/tests.nim"

  # Test runnable examples
  when (NimMajor, NimMinor) >= (1, 1):
    exec "nim doc -o:./docs/ugh/ugh.html ./src/regex.nim"

task docs, "Docs":
  exec "nim doc --project -o:./docs ./src/regex.nim"
