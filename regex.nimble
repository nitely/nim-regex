# Package

version = "0.25.0"
author = "Esteban Castro Borsani (@nitely)"
description = "Linear time regex matching"
license = "MIT"
srcDir = "src"
skipDirs = @["tests", "bench", "docs"]

requires "nim >= 1.6.0"
requires "unicodedb >= 0.7.2"

task test2, "Test":
  exec "nim c -r -o:bin/regex src/regex.nim"
  exec "nim c -r -o:bin/litopt src/regex/litopt.nim"
  exec "nim c -r -o:bin/altopt src/regex/altopt.nim"
  exec "nim c -r -o:bin/nfatype src/regex/nfatype.nim"
  exec "nim c -r tests/tests2.nim"
  exec "nim c -r -d:forceRegexAtRuntime tests/tests2.nim"
  exec "nim c -r -d:forceRegexAtRuntime -d:noRegexOpt tests/tests2.nim"
  exec "nim c -r -d:noRegexOpt tests/tests2.nim"
  exec "nim c -d:runTestAtCT tests/tests2.nim"
  exec "nim c -r tests/tests_misc.nim"
  exec "nim c -r -d:forceRegexAtRuntime tests/tests_misc.nim"
  exec "nim c -r -d:forceRegexAtRuntime -d:noRegexOpt tests/tests_misc.nim"
  exec "nim c -r -d:noRegexOpt tests/tests_misc.nim"
  # JS
  exec "nim js -r src/regex.nim"
  exec "nim js -r tests/tests2.nim"
  exec "nim js -r -d:forceRegexAtRuntime tests/tests2.nim"
  exec "nim js -r tests/tests_misc.nim"
  exec "nim js -r -d:forceRegexAtRuntime tests/tests_misc.nim"
  # test release/danger mode
  exec "nim c -r -d:release -o:bin/regex src/regex.nim"
  exec "nim c -r -d:danger -o:bin/regex src/regex.nim"
  # Test runnable examples
  exec "nim doc -o:./docs/ugh/ugh.html ./src/regex.nim"

task oldtest, "Test":
  # OLD DEPRECATED API TESTS
  exec "nim c -r tests/tests.nim"
  exec "nim c -r -d:forceRegexAtRuntime tests/tests.nim"
  exec "nim c -r -d:forceRegexAtRuntime -d:noRegexOpt tests/tests.nim"
  exec "nim c -r -d:noRegexOpt tests/tests.nim"
  exec "nim c -d:runTestAtCT tests/tests.nim"
  exec "nim js -r tests/tests.nim"
  exec "nim js -r -d:forceRegexAtRuntime tests/tests.nim"

task test, "Test":
  exec "nimble test2"
  exec "nimble oldtest"

task docs, "Docs":
  exec "nim doc --project -o:./docs ./src/regex.nim"
