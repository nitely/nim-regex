# Package

version = "0.7.0"
author = "Esteban Castro Borsani (@nitely)"
description = "Linear time regex matching"
license = "MIT"
srcDir = "src"
skipDirs = @["tests"]

requires "nim >= 0.17.2"
requires "unicodedb >= 0.4.0 & < 0.5"
requires "unicodeplus >= 0.2.0 & < 0.3"

task test, "Test":
  exec "nim c -r src/regex.nim"
  exec "nim c -r tests/tests.nim"

task docs, "Docs":
  exec "nim doc2 -o:./docs/index.html ./src/regex.nim"
