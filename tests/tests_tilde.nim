import std/macros
import ../src/regex

macro genAsserts(n: static int): untyped =
  result = newStmtList()
  for i in 1 .. n:
    let lhs = newLit("ab" & $i)
    let rhs = newLit("ab" & $i & $i & $i)
    result.add(quote do:
      doAssert(~`lhs` in `rhs`)
    )

block:
  proc threadFunc() {.thread.} =
    doAssert ~r"ab" in "abcd"
    regexDestroyCache()

  proc test() =
    var th: Thread[void]
    createThread(th, threadFunc)
    joinThread(th)

  test()
  regexDestroyCache()

block:
  proc test() =
    var reg = @[~r"ab1", ~r"ab2", ~r"ab3"]
    doAssert reg[0] in "ab123"
    doAssert reg[^1] in "ab321"

  test()
  regexDestroyCache()

block:
  proc test() =
    proc foo(reg: Regex2) =
      genAsserts(128)
      doAssert reg in "abcd"
    foo(~r"ab")

  test()
  regexDestroyCache()

block:
  proc threadFunc() {.thread.} =
    genAsserts(128)
    doAssert ~r"ab" in "abcd"
    regexDestroyCache()

  proc test() =
    var th = newSeq[Thread[void]](16)
    for i in 0 ..< th.len:
      createThread(th[i], threadFunc)
    joinThreads(th)

  test()
  regexDestroyCache()

echo "ok"
