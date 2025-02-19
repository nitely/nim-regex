# Package

version = "0.26.3"
author = "Esteban Castro Borsani (@nitely)"
description = "Linear time regex matching"
license = "MIT"
srcDir = "src"
skipDirs = @["tests", "bench", "docs"]

requires "nim >= 1.6.0"
requires "unicodedb >= 0.13.2"

template execTest(lang, target: static string) =
  doAssert lang in ["c", "js"]
  exec "nim " & lang & " --mm:refc " & target
  if NimMajor >= 2:
    exec "nim " & lang & " --mm:orc " & target

task test2, "Test":
  execTest "c", "-r -o:bin/regex src/regex.nim"
  execTest "c", "-r -o:bin/litopt src/regex/litopt.nim"
  execTest "c", "-r -o:bin/nfatype src/regex/nfatype.nim"
  execTest "c", "-r -o:bin/common src/regex/common.nim"
  execTest "c", "-r tests/tests2.nim"
  execTest "c", "-r -d:forceRegexAtRuntime tests/tests2.nim"
  execTest "c", "-r -d:forceRegexAtRuntime -d:noRegexOpt tests/tests2.nim"
  execTest "c", "-r -d:noRegexOpt tests/tests2.nim"
  execTest "c", "-d:runTestAtCT tests/tests2.nim"
  execTest "c", "-r tests/tests_misc.nim"
  execTest "c", "-r -d:forceRegexAtRuntime tests/tests_misc.nim"
  execTest "c", "-r -d:forceRegexAtRuntime -d:noRegexOpt tests/tests_misc.nim"
  execTest "c", "-r -d:noRegexOpt tests/tests_misc.nim"
  # JS
  execTest "js", "-r src/regex.nim"
  execTest "js", "-r tests/tests2.nim"
  execTest "js", "-r -d:forceRegexAtRuntime tests/tests2.nim"
  execTest "js", "-r tests/tests_misc.nim"
  execTest "js", "-r -d:forceRegexAtRuntime tests/tests_misc.nim"
  # test release/danger mode
  execTest "c", "-r -d:release -o:bin/regex src/regex.nim"
  execTest "c", "-r -d:danger -o:bin/regex src/regex.nim"
  # Test runnable examples
  exec "nim doc -o:./docs/ugh/ugh.html ./src/regex.nim"

task oldtest, "Test":
  # OLD DEPRECATED API TESTS
  execTest "c", "-r tests/tests.nim"
  execTest "c", "-r -d:forceRegexAtRuntime tests/tests.nim"
  execTest "c", "-r -d:forceRegexAtRuntime -d:noRegexOpt tests/tests.nim"
  execTest "c", "-r -d:noRegexOpt tests/tests.nim"
  execTest "c", "-d:runTestAtCT tests/tests.nim"
  execTest "js", "-r tests/tests.nim"
  execTest "js", "-r -d:forceRegexAtRuntime tests/tests.nim"

task test, "Test":
  exec "nimble test2"
  exec "nimble oldtest"

task docs, "Docs":
  exec "nim doc --project -o:./docs ./src/regex.nim"
