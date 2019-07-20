# Package

version = "0.12.0"
author = "Esteban Castro Borsani (@nitely)"
description = "Linear time regex matching"
license = "MIT"
srcdir = "src"
skipDirs = @["tests"]

requires "nim >= 0.18.0"
requires "unicodedb >= 0.7.2"
requires "unicodeplus >= 0.5.0"

task test, "Test":
  exec "nim c -r src/regex.nim"
  exec "nim c -r tests/tests.nim"
  exec "nim c -r -d:forceRegexAtRuntime tests/tests.nim"
  exec "nim c -d:runTestAtCT tests/tests.nim"
  #exec "nim js -r src/regex.nim"
  # These two fail with Node.js OOM error
  # https://github.com/nitely/nim-regex/issues/38
  #exec "nim js -r tests/tests.nim"
  #exec "nim js -r -d:forceRegexAtRuntime tests/tests.nim"

task docs, "Docs":
  exec "nim doc2 -o:./docs/index.html ./src/regex.nim"
