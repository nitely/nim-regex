#[
 *
 *	Glenn Fowler <glenn.s.fowler@gmail.com>
 *	AT&T Research
 *
 * PLEASE: publish your tests so everyone can benefit
 *
 * The following license covers testregex.c and all associated test data.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of THIS SOFTWARE FILE (the "Software"), to deal in the Software
 * without restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, and/or sell copies of the
 * Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following disclaimer:
 *
 * THIS SOFTWARE IS PROVIDED BY AT&T ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL AT&T BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *

Copyright (c) 2014 The Rust Project Developers

Permission is hereby granted, free of charge, to any
person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the
Software without restriction, including without
limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software
is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice
shall be included in all copies or substantial portions
of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
]#

from std/sequtils import map
import ../src/regex

const nonCapture = reNonCapture

template test(desc: string, body: untyped): untyped =
  (proc() =
    echo "[CT/RT] " & desc
    body)()

template check(condition: bool) =
  doAssert(condition)

func findAllCapt(s: string, reg: Regex2): seq[seq[Slice[int]]] =
  result = map(
    findAll(s, reg),
    func (m: RegexMatch2): seq[Slice[int]] =
      for i in 0 .. m.groupsCount-1:
        result.add m.group(i))

when (NimMajor, NimMinor) >= (2, 0):
  type MyAssertionDefect = ref AssertionDefect
else:
  type MyAssertionDefect = ref AssertionError

test "sanity":
  try:
    check false
    doAssert false
  except MyAssertionDefect:
    check true
  check match("a", re2"a")
  check(not match("b", re2"a"))
  check findAllBounds("a", re2"a") == @[0 .. 0]
  check findAllBounds("b", re2"a").len == 0

test "fowler_basic":
  check findAllBounds("abracadabracadabra", re2"abracadabra$") == @[7 .. 17]
  check findAllBounds("abababbb", re2"a...b") == @[2 .. 6]
  check findAllBounds("..XXXXXX", re2"XXXXXX") == @[2 .. 7]
  check findAllBounds("()", re2"\)") == @[1 .. 1]
  check findAllBounds("a]a", re2"a]") == @[0 .. 1]
  check findAllBounds("}", re2"}") == @[0 .. 0]
  check findAllBounds("}", re2"\}") == @[0 .. 0]
  check findAllBounds("]", re2"\]") == @[0 .. 0]
  check findAllBounds("]", re2"]") == @[0 .. 0]
  check findAllBounds("{", re2"{") == @[0 .. 0]
  check findAllBounds("ax", re2"^a") == @[0 .. 0]
  check findAllBounds("a^a", re2"\^a") == @[1 .. 2]
  check findAllBounds("a^", re2"a\^") == @[0 .. 1]
  check findAllBounds("aa", re2"a$") == @[1 .. 1]
  check findAllBounds("a$", re2"a\$") == @[0 .. 1]
  check findAllBounds("", re2"^$") == @[0 .. -1]
  check findAllBounds("", re2"$^") == @[0 .. -1]
  check findAllBounds("aa", re2"a($)") == @[1 .. 1]
  check findAllCapt("aa", re2"a($)") == @[@[2 .. 1]]
  check findAllBounds("aa", re2"a*(^a)") == @[0 .. 0]
  check findAllCapt("aa", re2"a*(^a)") == @[@[0 .. 0]]
  check findAllBounds("a", re2"(..)*(...)*") == @[0 .. -1, 1 .. 0]
  check findAllBounds("abcd", re2"(..)*(...)*") == @[0 .. 3, 4 .. 3]
  check findAllCapt("abcd", re2"(..)*(...)*") ==
    @[@[2 .. 3, nonCapture], @[nonCapture, nonCapture]]
  check findAllBounds("abc", re2"(ab|a)(bc|c)") == @[0 .. 2]
  check findAllCapt("abc", re2"(ab|a)(bc|c)") == @[@[0 .. 1, 2 .. 2]]
  check findAllBounds("abc", re2"(ab)c|abc") == @[0 .. 2]
  check findAllCapt("abc", re2"(ab)c|abc") == @[@[0 .. 1]]
  #check findAllBounds("ab", re2"a{0}b") == @[1 .. 1]  # XXX Fix?
  check findAllBounds("aaabbbbbbb", re2"(a*)(b?)(b+)b{3}") == @[0 .. 9]
  check findAllCapt("aaabbbbbbb", re2"(a*)(b?)(b+)b{3}") ==
    @[@[0 .. 2, 3 .. 3, 4 .. 6]]
  check findAllBounds("aaabbbbbbb", re2"(a*)(b{0,1})(b{1,})b{3}") == @[0 .. 9]
  check findAllCapt("aaabbbbbbb", re2"(a*)(b{0,1})(b{1,})b{3}") ==
    @[@[0 .. 2, 3 .. 3, 4 .. 6]]
  check findAllBounds("a", re2"((a|a)|a)") == @[0 .. 0]
  check findAllCapt("a", re2"((a|a)|a)") == @[@[0 .. 0, 0 .. 0]]
  check findAllBounds("aaaa", re2"(a*)(a|aa)") == @[0 .. 3]
  check findAllCapt("aaaa", re2"(a*)(a|aa)") == @[@[0 .. 2, 3 .. 3]]
  check findAllBounds("aaaa", re2"a*(a.|aa)") == @[0 .. 3]
  check findAllCapt("aaaa", re2"a*(a.|aa)") == @[@[2 .. 3]]
  check findAllBounds("aef", re2"a(b)|c(d)|a(e)f") == @[0 .. 2]
  check findAllCapt("aef", re2"a(b)|c(d)|a(e)f") ==
    @[@[nonCapture, nonCapture, 1 .. 1]]
  check findAllBounds("b", re2"(a|b)?.*") == @[0 .. 0, 1 .. 0]
  check findAllCapt("b", re2"(a|b)?.*") == @[@[0 .. 0], @[nonCapture]]
  check findAllBounds("ac", re2"(a|ac)c|a(b|c)") == @[0 .. 1]
  check findAllCapt("ac", re2"(a|ac)c|a(b|c)") == @[@[0 .. 0, nonCapture]]
  check findAllBounds("ab", re2"(a|b)c|a(b|c)") == @[0 .. 1]
  check findAllCapt("ab", re2"(a|b)c|a(b|c)") == @[@[nonCapture, 1 .. 1]]
  check findAllBounds("abc", re2"(a|b)*c|(a|ab)*c") == @[0 .. 2]
  check findAllCapt("abc", re2"(a|b)*c|(a|ab)*c") == @[@[1 .. 1, nonCapture]]
  check findAllBounds("xc", re2"(a|b)*c|(a|ab)*c") == @[1 .. 1]
  check findAllCapt("xc", re2"(a|b)*c|(a|ab)*c") ==
    @[@[nonCapture, nonCapture]]
  check findAllBounds("xa", re2"(.a|.b).*|.*(.a|.b)") == @[0 .. 1]
  check findAllCapt("xa", re2"(.a|.b).*|.*(.a|.b)") == @[@[0 .. 1, nonCapture]]
  check findAllBounds("abab", re2"a?(ab|ba)ab") == @[0 .. 3]
  check findAllCapt("abab", re2"a?(ab|ba)ab") == @[@[0 .. 1]]
  #check findAllBounds("abab", re2"a?(ac{0}b|ba)ab") == @[0 .. 3]
  #check findAllCapt("abab", re2"a?(ac{0}b|ba)ab") == @[@[0 .. 1]]
  check findAllBounds("abbabab", re2"ab|abab") == @[0 .. 1, 3 .. 4, 5 .. 6]
  check findAllBounds("baaabbbaba", re2"aba|bab|bba") == @[5 .. 7]
  check findAllBounds("baaabbbaba", re2"aba|bab") == @[6 .. 8]
  check findAllBounds("aa", re2"(aa|aaa)*|(a|aaaaa)") == @[0 .. 1, 2 .. 1]
  check findAllCapt("aa", re2"(aa|aaa)*|(a|aaaaa)") ==
    @[@[0 .. 1, nonCapture], @[nonCapture, nonCapture]]
  check findAllBounds("aa", re2"(a.|.a.)*|(a|.a...)") == @[0 .. 1, 2 .. 1]
  check findAllCapt("aa", re2"(a.|.a.)*|(a|.a...)") ==
    @[@[0 .. 1, nonCapture], @[nonCapture, nonCapture]]
  check findAllBounds("xabc", re2"ab|a") == @[1 .. 2]
  check findAllBounds("xxabc", re2"ab|a") == @[2 .. 3]
  check findAllBounds("aBcD", re2"(?i)(Ab|cD)*") == @[0 .. 3, 4 .. 3]
  check findAllBounds("--a", re2"[^-]") == @[2 .. 2]
  check findAllBounds("--a", re2"[a-]*") == @[0 .. 2, 3 .. 2]
  check findAllBounds("--amoma--", re2"[a-m-]*") ==
    @[0 .. 3, 4 .. 3, 5 .. 8, 9 .. 8]
  check findAllBounds(":::0:::1:::1:::0:", re2":::1:::0:|:::1:1:0:") ==
    @[8 .. 16]
  check findAllBounds(":::0:::1:::1:::0:", re2":::1:::0:|:::1:1:1:") ==
    @[8 .. 16]
  check findAllBounds("A", re2"[[:upper:]]") == @[0 .. 0]
  check findAllBounds("`az{", re2"[[:lower:]]+") == @[1 .. 2]
  check findAllBounds("@AZ[", re2"[[:upper:]]+") == @[1 .. 2]
  check findAllBounds("[[-]]", re2"[[-]]") == @[2 .. 3]
  check findAllBounds("[[-]]", re2"[[-]]") == @[2 .. 3]
  check findAllBounds("\n", re2"\n") == @[0 .. 0]
  check findAllBounds("\n", re2"[^a]") == @[0 .. 0]
  check findAllBounds("\na", re2"\na") == @[0 .. 1]
  check findAllBounds("abc", re2"(a)(b)(c)") == @[0 .. 2]
  check findAllCapt("abc", re2"(a)(b)(c)") ==
    @[@[0 .. 0, 1 .. 1, 2 .. 2]]
  check findAllBounds("xxx", re2"xxx") == @[0 .. 2]
  check findAllBounds("feb 6,", re2"(^|[ (,;])((([Ff]eb[^ ]* *|0*2/|\* */?)0*[6-7]))([^0-9]|$)") ==
    @[0 .. 5]
  check findAllBounds("feb 6,", re2"(?:^|[ (,;])(?:(?:(?:[Ff]eb[^ ]* *|0*2/|\* */?)0*[6-7]))(?:[^0-9]|$)") ==
    @[0 .. 5]
  check findAllBounds("2/7", re2"(^|[ (,;])((([Ff]eb[^ ]* *|0*2/|\* */?)0*[6-7]))([^0-9]|$)") ==
    @[0 .. 2]
  check findAllBounds("2/7", re2"(?:^|[ (,;])(?:(?:(?:[Ff]eb[^ ]* *|0*2/|\* */?)0*[6-7]))(?:[^0-9]|$)") ==
    @[0 .. 2]
  check findAllBounds("feb 1,Feb 6", re2"(^|[ (,;])((([Ff]eb[^ ]* *|0*2/|\* */?)0*[6-7]))([^0-9]|$)") ==
    @[5 .. 10]
  check findAllBounds("feb 1,Feb 6", re2"(?:^|[ (,;])(?:(?:(?:[Ff]eb[^ ]* *|0*2/|\* */?)0*[6-7]))(?:[^0-9]|$)") ==
    @[5 .. 10]
  check findAllBounds("x", re2"((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))") ==
    @[0 .. 0]
  check findAllCapt("x", re2"((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))") ==
    @[@[0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0,
        0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0,
        0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0,
        0 .. 0, 0 .. 0, 0 .. 0]]
  check findAllBounds("x", re2"(((?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:(?:x))))))))))))))))))))))))))))))") ==
    @[0 .. 0]
  check findAllBounds("xx", re2"((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))*") ==
    @[0 .. 1, 2 .. 1]
  check findAllBounds("xx", re2"((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))*") ==
    @[0 .. 1, 2 .. 1]
  check findAllBounds("ababababababababababababababababababababababababababababababababababababababababa", re2"a?(ab|ba)*") ==
    @[0 .. 80, 81 .. 80]
  check findAllBounds("ababbabbbabbbabbbbabbbbaa", re2"abaa|abbaa|abbbaa|abbbbaa") ==
    @[18 .. 24]
  check findAllBounds("ababbabbbabbbabbbbabaa", re2"abaa|abbaa|abbbaa|abbbbaa") ==
    @[18 .. 21]
  check findAllBounds("baaabbbabac", re2"aaac|aabc|abac|abbc|baac|babc|bbac|bbbc") ==
    @[7 .. 10]
  #check findAllBounds("\x01\xff", re2".*") == @[0 .. 1]  # Invalid utf-8 input
  check findAllBounds("XaaaXbbbXcccXdddXeeeXfffXgggXhhhXiiiXjjjXkkkXlllXcbaXaaaa", re2"aaaa|bbbb|cccc|ddddd|eeeeee|fffffff|gggg|hhhh|iiiii|jjjjj|kkkkk|llll") ==
    @[53 .. 56]
  check findAllBounds("XaaaXbbbXcccXdddXeeeXfffXgggXhhhXiiiXjjjXkkkXlllXcbaXaaaa", re2"aaaa\nbbbb\ncccc\nddddd\neeeeee\nfffffff\ngggg\nhhhh\niiiii\njjjjj\nkkkkk\nllll").len == 0
  check findAllBounds("aaaaaaaaab", re2"a*a*a*a*a*b") == @[0 .. 9]
  check findAllBounds("", re2"^") == @[0 .. -1]
  check findAllBounds("", re2"$") == @[0 .. -1]
  check findAllBounds("", re2"^$") == @[0 .. -1]
  check findAllBounds("a", re2"^a$") == @[0 .. 0]
  check findAllBounds("abc", re2"abc") == @[0 .. 2]
  check findAllBounds("xabcy", re2"abc") == @[1 .. 3]
  check findAllBounds("ababc", re2"abc") == @[2 .. 4]
  check findAllBounds("abc", re2"ab*c") == @[0 .. 2]
  check findAllBounds("abc", re2"ab*bc") == @[0 .. 2]
  check findAllBounds("abbc", re2"ab*bc") == @[0 .. 3]
  check findAllBounds("abbbbc", re2"ab*bc") == @[0 .. 5]
  check findAllBounds("abbc", re2"ab+bc") == @[0 .. 3]
  check findAllBounds("abbbbc", re2"ab+bc") == @[0 .. 5]
  check findAllBounds("abbc", re2"ab?bc") == @[0 .. 3]
  check findAllBounds("abc", re2"ab?bc") == @[0 .. 2]
  check findAllBounds("abc", re2"ab?c") == @[0 .. 2]
  check findAllBounds("abc", re2"^abc$") == @[0 .. 2]
  check findAllBounds("abc", re2"^abc") == @[0 .. 2]
  check findAllBounds("aabc", re2"abc$") == @[1 .. 3]
  check findAllBounds("abc", re2"^") == @[0 .. -1]
  check findAllBounds("abc", re2"$") == @[3 .. 2]
  check findAllBounds("abc", re2"a.c") == @[0 .. 2]
  check findAllBounds("axc", re2"a.c") == @[0 .. 2]
  check findAllBounds("axyzc", re2"a.*c") == @[0 .. 4]
  check findAllBounds("abd", re2"a[bc]d") == @[0 .. 2]
  check findAllBounds("ace", re2"a[b-d]e") == @[0 .. 2]
  check findAllBounds("aac", re2"a[b-d]") == @[1 .. 2]
  check findAllBounds("a-", re2"a[-b]") == @[0 .. 1]
  check findAllBounds("a-", re2"a[b-]") == @[0 .. 1]
  check findAllBounds("a]", re2"a]") == @[0 .. 1]
  check findAllBounds("a]b", re2"a[]]b") == @[0 .. 2]
  check findAllBounds("aed", re2"a[^bc]d") == @[0 .. 2]
  check findAllBounds("adc", re2"a[^-b]c") == @[0 .. 2]
  check findAllBounds("adc", re2"a[^]b]c") == @[0 .. 2]
  check findAllBounds("abcd", re2"ab|cd") == @[0 .. 1, 2 .. 3]
  check findAllBounds("a(b", re2"a\(b") == @[0 .. 2]
  check findAllBounds("ab", re2"a\(*b") == @[0 .. 1]
  check findAllBounds("a((b", re2"a\(*b") == @[0 .. 3]
  check findAllBounds("abc", re2"((a))") == @[0 .. 0]
  check findAllCapt("abc", re2"((a))") ==
    @[@[0 .. 0, 0 .. 0]]
  check findAllBounds("abc", re2"(a)b(c)") == @[0 .. 2]
  check findAllCapt("abc", re2"(a)b(c)") ==
    @[@[0 .. 0, 2 .. 2]]
  check findAllBounds("aabbabc", re2"a+b+c") == @[4 .. 6]
  check findAllBounds("aaa", re2"a*") == @[0 .. 2, 3 .. 2]
  check findAllBounds("-", re2"(a*)*") == @[0 .. -1, 1 .. 0]
  check findAllBounds("-", re2"(a*)+") == @[0 .. -1, 1 .. 0]
  check findAllBounds("-", re2"(a*|b)*") == @[0 .. -1, 1 .. 0]
  check findAllBounds("ab", re2"(a+|b)*") == @[0 .. 1, 2 .. 1]
  check findAllBounds("ab", re2"(a+|b)+") == @[0 .. 1]
  check findAllCapt("ab", re2"(a+|b)+") == @[@[1 .. 1]]
  check findAllBounds("ab", re2"(a+|b)?") == @[0 .. 0, 1 .. 1, 2 .. 1]
  check findAllCapt("ab", re2"(a+|b)?") ==
    @[@[0 .. 0], @[1 .. 1], @[nonCapture]]
  check findAllBounds("cde", re2"[^ab]*") == @[0 .. 2, 3 .. 2]
  check findAllBounds("-", re2"(^)*") == @[0 .. -1, 1 .. 0]
  check findAllCapt("-", re2"(^)*") == @[@[0 .. -1], @[nonCapture]]
  check findAllBounds("", re2"a*") == @[0 .. -1]
  check findAllBounds("abbbcd", re2"([abc])*d") == @[0 .. 5]
  check findAllCapt("abbbcd", re2"([abc])*d") == @[@[4 .. 4]]
  check findAllBounds("abcd", re2"([abc])*bcd") == @[0 .. 3]
  check findAllCapt("abcd", re2"([abc])*bcd") == @[@[0 .. 0]]
  check findAllBounds("e", re2"a|b|c|d|e") == @[0 .. 0]
  check findAllBounds("ef", re2"(a|b|c|d|e)f") == @[0 .. 1]
  check findAllCapt("ef", re2"(a|b|c|d|e)f") == @[@[0 .. 0]]
  check findAllBounds("-", re2"((a*|b))*") == @[0 .. -1, 1 .. 0]
  check findAllCapt("-", re2"((a*|b))*") ==
    @[@[0 .. -1, 0 .. -1], @[1 .. 0, 1 .. 0]]
  check findAllBounds("abcdefg", re2"abcd*efg") == @[0 .. 6]
  check findAllBounds("xabyabbbz", re2"ab*") == @[1 .. 2, 4 .. 7]
  check findAllBounds("xayabbbz", re2"ab*") == @[1 .. 1, 3 .. 6]
  check findAllBounds("abcde", re2"(ab|cd)e") == @[2 .. 4]
  check findAllCapt("abcde", re2"(ab|cd)e") == @[@[2 .. 3]]
  check findAllBounds("hij", re2"[abhgefdc]ij") == @[0 .. 2]
  check findAllBounds("abcd", re2"(a|b)c*d") == @[1 .. 3]
  check findAllCapt("abcd", re2"(a|b)c*d") == @[@[1 .. 1]]
  check findAllBounds("abc", re2"(ab|ab*)bc") == @[0 .. 2]
  check findAllCapt("abc", re2"(ab|ab*)bc") == @[@[0 .. 0]]
  check findAllBounds("abc", re2"a([bc]*)c*") == @[0 .. 2]
  check findAllCapt("abc", re2"a([bc]*)c*") == @[@[1 .. 2]]
  check findAllBounds("abcd", re2"a([bc]*)(c*d)") == @[0 .. 3]
  check findAllCapt("abcd", re2"a([bc]*)(c*d)") == @[@[1 .. 2, 3 .. 3]]
  check findAllBounds("abcd", re2"a([bc]+)(c*d)") == @[0 .. 3]
  check findAllCapt("abcd", re2"a([bc]+)(c*d)") == @[@[1 .. 2, 3 .. 3]]
  check findAllBounds("abcd", re2"a([bc]*)(c+d)") == @[0 .. 3]
  check findAllCapt("abcd", re2"a([bc]*)(c+d)") == @[@[1 .. 1, 2 .. 3]]
  check findAllBounds("adcdcde", re2"a[bcd]*dcdcde") == @[0 .. 6]
  check findAllBounds("abc", re2"(ab|a)b*c") == @[0 .. 2]
  check findAllCapt("abc", re2"(ab|a)b*c") == @[@[0 .. 1]]
  check findAllBounds("abcd", re2"((a)(b)c)(d)") == @[0 .. 3]
  check findAllCapt("abcd", re2"((a)(b)c)(d)") ==
    @[@[0 .. 2, 0 .. 0, 1 .. 1, 3 .. 3]]
  check findAllBounds("alpha", re2"[A-Za-z_][A-Za-z0-9_]*") == @[0 .. 4]
  check findAllBounds("abh", re2"^a(bc+|b[eh])g|.h$") == @[1 .. 2]
  check findAllBounds("effgz", re2"(bc+d$|ef*g.|h?i(j|k))") == @[0 .. 4]
  check findAllCapt("effgz", re2"(bc+d$|ef*g.|h?i(j|k))") ==
    @[@[0 .. 4, nonCapture]]
  check findAllBounds("ij", re2"(bc+d$|ef*g.|h?i(j|k))") == @[0 .. 1]
  check findAllCapt("ij", re2"(bc+d$|ef*g.|h?i(j|k))") ==
    @[@[0 .. 1, 1 .. 1]]
  check findAllBounds("reffgz", re2"(bc+d$|ef*g.|h?i(j|k))") == @[1 .. 5]
  check findAllCapt("reffgz", re2"(bc+d$|ef*g.|h?i(j|k))") ==
    @[@[1 .. 5, nonCapture]]
  check findAllBounds("a", re2"(((((((((a)))))))))") == @[0 .. 0]
  check findAllCapt("a", re2"(((((((((a)))))))))") ==
    @[@[0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0, 0 .. 0]]
  check findAllBounds("multiple words yeah", re2"multiple words") == @[0 .. 13]
  check findAllBounds("abcde", re2"(.*)c(.*)") == @[0 .. 4]
  check findAllCapt("abcde", re2"(.*)c(.*)") ==
    @[@[0 .. 1, 3 .. 4]]
  check findAllBounds("abcd", re2"abcd") == @[0 .. 3]
  check findAllBounds("abcd", re2"a(bc)d") == @[0 .. 3]
  check findAllCapt("abcd", re2"a(bc)d") == @[@[1 .. 2]]
  check findAllBounds("ac", re2"a[-]?c") == @[0 .. 2]
  check findAllBounds("Muammar Qaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 14]
  check findAllCapt("Muammar Qaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 10 .. 11]]
  check findAllBounds("Mo'ammar Gadhafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 15]
  check findAllCapt("Mo'ammar Gadhafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 11 .. 12]]
  check findAllBounds("Muammar Kaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 14]
  check findAllCapt("Muammar Kaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 10 .. 11]]
  check findAllBounds("Muammar Qadhafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 14]
  check findAllCapt("Muammar Qadhafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 10 .. 11]]
  check findAllBounds("Muammar Gadafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 13]
  check findAllCapt("Muammar Gadafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 10 .. 10]]
  check findAllBounds("Mu'ammar Qadafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 14]
  check findAllCapt("Mu'ammar Qadafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 11 .. 11]]
  check findAllBounds("Moamar Gaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 13]
  check findAllCapt("Moamar Gaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 9 .. 10]]
  check findAllBounds("Mu'ammar Qadhdhafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 17]
  check findAllCapt("Mu'ammar Qadhdhafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 13 .. 14]]
  check findAllBounds("Muammar Khaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 15]
  check findAllCapt("Muammar Khaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 11 .. 12]]
  check findAllBounds("Muammar Ghaddafy", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 15]
  check findAllCapt("Muammar Ghaddafy", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 11 .. 12]]
  check findAllBounds("Muammar Ghadafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 14]
  check findAllCapt("Muammar Ghadafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 11 .. 11]]
  check findAllBounds("Muammar Ghaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 15]
  check findAllCapt("Muammar Ghaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 11 .. 12]]
  check findAllBounds("Muamar Kaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 13]
  check findAllCapt("Muamar Kaddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 9 .. 10]]
  check findAllBounds("Muammar Quathafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 15]
  check findAllCapt("Muammar Quathafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 11 .. 12]]
  check findAllBounds("Muammar Gheddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 15]
  check findAllCapt("Muammar Gheddafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 11 .. 12]]
  check findAllBounds("Moammar Khadafy", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 14]
  check findAllCapt("Moammar Khadafy", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 11 .. 11]]
  check findAllBounds("Moammar Qudhafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[0 .. 14]
  check findAllCapt("Moammar Qudhafi", re2"M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]") ==
    @[@[nonCapture, 10 .. 11]]
  check findAllBounds("aabcdd", re2"a+(b|c)*d+") == @[0 .. 5]
  check findAllCapt("aabcdd", re2"a+(b|c)*d+") == @[@[3 .. 3]]
  check findAllBounds("vivi", re2"^.+$") == @[0 .. 3]
  check findAllBounds("vivi", re2"^(.+)$") == @[0 .. 3]
  check findAllCapt("vivi", re2"^(.+)$") == @[@[0 .. 3]]
  check findAllBounds("gryphon.att.com!eby", re2"^([^!.]+).att.com!(.+)$") ==
    @[0 .. 18]
  check findAllCapt("gryphon.att.com!eby", re2"^([^!.]+).att.com!(.+)$") ==
    @[@[0 .. 6, 16 .. 18]]
  check findAllBounds("bas", re2"^([^!]+!)?([^!]+)$") == @[0 .. 2]
  check findAllCapt("bas", re2"^([^!]+!)?([^!]+)$") == @[@[nonCapture, 0 .. 2]]
  check findAllBounds("bar!bas", re2"^([^!]+!)?([^!]+)$") == @[0 .. 6]
  check findAllCapt("bar!bas", re2"^([^!]+!)?([^!]+)$") ==
    @[@[0 .. 3, 4 .. 6]]
  check findAllBounds("foo!bas", re2"^([^!]+!)?([^!]+)$") == @[0 .. 6]
  check findAllCapt("foo!bas", re2"^([^!]+!)?([^!]+)$") ==
    @[@[0 .. 3, 4 .. 6]]
  check findAllBounds("foo!bar!bas", re2"^.+!([^!]+!)([^!]+)$") == @[0 .. 10]
  check findAllCapt("foo!bar!bas", re2"^.+!([^!]+!)([^!]+)$") ==
    @[@[4 .. 7, 8 .. 10]]
  check findAllBounds("bar!bas", re2"((foo)|(bar))!bas") == @[0 .. 6]
  check findAllCapt("bar!bas", re2"((foo)|(bar))!bas") ==
    @[@[0 .. 2, nonCapture, 0 .. 2]]
  check findAllBounds("foo!bar!bas", re2"((foo)|(bar))!bas") == @[4 .. 10]
  check findAllCapt("foo!bar!bas", re2"((foo)|(bar))!bas") ==
    @[@[4 .. 6, nonCapture, 4 .. 6]]
  check findAllBounds("foo!bas", re2"((foo)|(bar))!bas") == @[0 .. 6]
  check findAllCapt("foo!bas", re2"((foo)|(bar))!bas") ==
    @[@[0 .. 2, 0 .. 2, nonCapture]]
  check findAllBounds("bar!bas", re2"((foo)|bar)!bas") == @[0 .. 6]
  check findAllCapt("bar!bas", re2"((foo)|bar)!bas") ==
    @[@[0 .. 2, nonCapture]]
  check findAllBounds("foo!bar!bas", re2"((foo)|bar)!bas") == @[4 .. 10]
  check findAllCapt("foo!bar!bas", re2"((foo)|bar)!bas") ==
    @[@[4 .. 6, nonCapture]]
  check findAllBounds("foo!bas", re2"((foo)|bar)!bas") == @[0 .. 6]
  check findAllCapt("foo!bas", re2"((foo)|bar)!bas") ==
    @[@[0 .. 2, 0 .. 2]]
  check findAllBounds("bar!bas", re2"(foo|(bar))!bas") == @[0 .. 6]
  check findAllCapt("bar!bas", re2"(foo|(bar))!bas") ==
    @[@[0 .. 2, 0 .. 2]]
  check findAllBounds("foo!bar!bas", re2"(foo|(bar))!bas") == @[4 .. 10]
  check findAllCapt("foo!bar!bas", re2"(foo|(bar))!bas") ==
    @[@[4 .. 6, 4 .. 6]]
  check findAllBounds("foo!bas", re2"(foo|(bar))!bas") == @[0 .. 6]
  check findAllCapt("foo!bas", re2"(foo|(bar))!bas") ==
    @[@[0 .. 2, nonCapture]]
  check findAllBounds("bar!bas", re2"(foo|bar)!bas") == @[0 .. 6]
  check findAllCapt("bar!bas", re2"(foo|bar)!bas") == @[@[0 .. 2]]
  check findAllBounds("foo!bar!bas", re2"(foo|bar)!bas") == @[4 .. 10]
  check findAllCapt("foo!bar!bas", re2"(foo|bar)!bas") == @[@[4 .. 6]]
  check findAllBounds("foo!bas", re2"(foo|bar)!bas") == @[0 .. 6]
  check findAllCapt("foo!bas", re2"(foo|bar)!bas") == @[@[0 .. 2]]
  check findAllBounds("foo!bar!bas", re2"^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$") ==
    @[0 .. 10]
  check findAllCapt("foo!bar!bas", re2"^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$") ==
    @[@[0 .. 10, nonCapture, nonCapture, 4 .. 7, 8 .. 10]]
  check findAllBounds("bas", re2"^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$") ==
    @[0 .. 2]
  check findAllCapt("bas", re2"^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$") ==
    @[@[nonCapture, 0 .. 2, nonCapture, nonCapture]]
  check findAllBounds("bar!bas", re2"^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$") ==
    @[0 .. 6]
  check findAllCapt("bar!bas", re2"^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$") ==
    @[@[0 .. 3, 4 .. 6, nonCapture, nonCapture]]
  check findAllBounds("foo!bar!bas", re2"^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$") ==
    @[0 .. 10]
  check findAllCapt("foo!bar!bas", re2"^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$") ==
    @[@[nonCapture, nonCapture, 4 .. 7, 8 .. 10]]
  check findAllBounds("foo!bas", re2"^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$") ==
    @[0 .. 6]
  check findAllCapt("foo!bas", re2"^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$") ==
    @[@[0 .. 3, 4 .. 6, nonCapture, nonCapture]]
  check findAllBounds("bas", re2"^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$") ==
    @[0 .. 2]
  check findAllCapt("bas", re2"^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$") ==
    @[@[0 .. 2, nonCapture, 0 .. 2, nonCapture, nonCapture]]
  check findAllBounds("bar!bas", re2"^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$") ==
    @[0 .. 6]
  check findAllCapt("bar!bas", re2"^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$") ==
    @[@[0 .. 6, 0 .. 3, 4 .. 6, nonCapture, nonCapture]]
  check findAllBounds("foo!bar!bas", re2"^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$") ==
    @[0 .. 10]
  check findAllCapt("foo!bar!bas", re2"^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$") ==
    @[@[0 .. 10, nonCapture, nonCapture, 4 .. 7, 8 .. 10]]
  check findAllBounds("foo!bas", re2"^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$") ==
    @[0 .. 6]
  check findAllCapt("foo!bas", re2"^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$") ==
    @[@[0 .. 6, 0 .. 3, 4 .. 6, nonCapture, nonCapture]]
  check findAllBounds("/XXX", re2".*(/XXX).*") == @[0 .. 3]
  check findAllCapt("/XXX", re2".*(/XXX).*") == @[@[0 .. 3]]
  check findAllBounds(r"\XXX", re2".*(\\XXX).*") == @[0 .. 3]
  check findAllCapt(r"\XXX", re2".*(\\XXX).*") == @[@[0 .. 3]]
  check findAllBounds(r"\XXX", re2"\\XXX") == @[0 .. 3]
  check findAllBounds(r"/000", re2".*(/000).*") == @[0 .. 3]
  check findAllCapt(r"/000", re2".*(/000).*") == @[@[0 .. 3]]
  check findAllBounds(r"\000", re2".*(\\000).*") == @[0 .. 3]
  check findAllCapt(r"\000", re2".*(\\000).*") == @[@[0 .. 3]]
  check findAllBounds(r"\000", re2"\\000") == @[0 .. 3]

test "fowler_categorize":
  check findAllBounds("xaxaax", re2"aa*") == @[1 .. 1, 3 .. 4]
  check findAllBounds("abc", re2"(a*)(ab)*(b*)") == @[0 .. 1, 2 .. 1, 3 .. 2]
  check findAllCapt("abc", re2"(a*)(ab)*(b*)") ==
    @[@[0 .. 0, nonCapture, 1 .. 1],
      @[2 .. 1, nonCapture, 2 .. 1],
      @[3 .. 2, nonCapture, 3 .. 2]]
  check findAllBounds("aba", re2"((a*)(ab)*)((b*)(a*))") == @[0 .. 2, 3 .. 2]
  check findAllCapt("aba", re2"((a*)(ab)*)((b*)(a*))") ==
    @[@[0 .. 0, 0 .. 0, nonCapture, 1 .. 2, 1 .. 1, 2 .. 2],
      @[3 .. 2, 3 .. 2, nonCapture, 3 .. 2, 3 .. 2, 3 .. 2]]
  check findAllBounds("xxxxxx", re2"(...?.?)*") == @[0 .. 5, 6 .. 5]
  check findAllCapt("xxxxxx", re2"(...?.?)*") == @[@[4 .. 5], @[nonCapture]]
  check findAllBounds("abcabc", re2"(a|ab)(bc|c)") == @[0 .. 2, 3 .. 5]
  check findAllCapt("abcabc", re2"(a|ab)(bc|c)") == 
    @[@[0 .. 0, 1 .. 2], @[3 .. 3, 4 .. 5]]
  check findAllBounds("ababa", re2"(aba|a*b)(aba|a*b)") == @[0 .. 3]
  check findAllCapt("ababa", re2"(aba|a*b)(aba|a*b)") == 
    @[@[0 .. 2, 3 .. 3]]
  check findAllBounds("a", re2"(a*)*") == @[0 .. 0, 1 .. 0]
  check findAllCapt("a", re2"(a*)*") == @[@[1 .. 0], @[1 .. 0]]  # XXX ?
  check findAllBounds("ababa", re2"(aba|a*b)*") == @[0 .. 3, 4 .. 3, 5 .. 4]
  check findAllCapt("ababa", re2"(aba|a*b)*") ==
    @[@[3 .. 3], @[nonCapture], @[nonCapture]]
  check findAllBounds("aba", re2"(a(b)?)+") == @[0 .. 2]
  check findAllCapt("aba", re2"(a(b)?)+") == @[@[2 .. 2, 1 .. 1]]  # same as python
  check findAllBounds("abab", re2"(a?)((ab)?)(b?)a?(ab)?b?") == @[0 .. 3, 4 .. 3]
  check findAllCapt("abab", re2"(a?)((ab)?)(b?)a?(ab)?b?") ==
    @[@[0 .. 0, 1 .. 0, nonCapture, 1 .. 1, nonCapture],
      @[4 .. 3, 4 .. 3, nonCapture, 4 .. 3, nonCapture]]  # same as python
  check findAllBounds("ab", re2".*(.*)") == @[0 .. 1, 2 .. 1]
  check findAllCapt("ab", re2".*(.*)") == @[@[2 .. 1], @[2 .. 1]]

test "fowler_nullsubexpr":
  check findAllBounds("aaaaaa", re2"(a*)*") == @[0 .. 5, 6 .. 5]
  check findAllCapt("aaaaaa", re2"(a*)*") == @[@[6 .. 5], @[6 .. 5]]
  check findAllBounds("a", re2"(a*)+") == @[0 .. 0, 1 .. 0]
  check findAllCapt("a", re2"(a*)+") == @[@[1 .. 0], @[1 .. 0]]
  check findAllBounds("a", re2"(a+)*") == @[0 .. 0, 1 .. 0]
  check findAllCapt("a", re2"(a+)*") == @[@[0 .. 0], @[nonCapture]]
  check findAllBounds("a", re2"(a+)+") == @[0 .. 0]
  check findAllCapt("a", re2"(a+)+") == @[@[0 .. 0]]
  check findAllBounds("a", re2"([a]*)*") == @[0 .. 0, 1 .. 0]
  check findAllCapt("a", re2"([a]*)*") == @[@[1 .. 0], @[1 .. 0]]
  check findAllBounds("aaaaaab", re2"([^b]*)*") == @[0 .. 5, 6 .. 5, 7 .. 6]
  check findAllCapt("aaaaaab", re2"([^b]*)*") ==
    @[@[6 .. 5], @[6 .. 5], @[7 .. 6]]
  check findAllBounds("zabcde", re2"((z)+|a)*") ==
    @[0 .. 1, 2 .. 1, 3 .. 2, 4 .. 3, 5 .. 4, 6 .. 5]
  check findAllCapt("zabcde", re2"((z)+|a)*") ==
    @[@[1 .. 1, 0 .. 0],
      @[nonCapture, nonCapture],
      @[nonCapture, nonCapture],
      @[nonCapture, nonCapture],
      @[nonCapture, nonCapture],
      @[nonCapture, nonCapture]]
  check findAllBounds("axa", re2"(a*)*(x)") == @[0 .. 1]
  check findAllCapt("axa", re2"(a*)*(x)") == @[@[1 .. 0, 1 .. 1]]
  check findAllBounds("axa", re2"(a*)+(x)") == @[0 .. 1]
  check findAllCapt("axa", re2"(a*)+(x)") == @[@[1 .. 0, 1 .. 1]]

test "fowler_repetition":
  check findAllBounds("", re2"((..)|(.))").len == 0
  check findAllCapt("", re2"((..)|(.))").len == 0
  check findAllBounds("", re2"((..)|(.))*") == @[0 .. -1]
  check findAllCapt("", re2"((..)|(.))*") ==
    @[@[nonCapture, nonCapture, nonCapture]]
  check findAllBounds("a", re2"((..)|(.))") == @[0 .. 0]
  check findAllCapt("a", re2"((..)|(.))") ==
    @[@[0 .. 0, nonCapture, 0 .. 0]]
  check findAllBounds("a", re2"((..)|(.))*") == @[0 .. 0, 1 .. 0]
  check findAllCapt("a", re2"((..)|(.))*") ==
    @[@[0 .. 0, nonCapture, 0 .. 0],
      @[nonCapture, nonCapture, nonCapture]]
  check findAllBounds("aa", re2"((..)|(.))") == @[0 .. 1]
  check findAllCapt("aa", re2"((..)|(.))") ==
    @[@[0 .. 1, 0 .. 1, nonCapture]]
  check findAllBounds("X1234567Y", re2"X(.?){0,}Y") == @[0 .. 8]
  check findAllCapt("X1234567Y", re2"X(.?){0,}Y") == @[@[8 .. 7]]
  check findAllBounds("X1234567Y", re2"X(.?){7,}Y") == @[0 .. 8]
  check findAllCapt("X1234567Y", re2"X(.?){7,}Y") == @[@[8 .. 7]]
  check findAllBounds("X1234567Y", re2"X(.?){8,}Y") == @[0 .. 8]
  check findAllCapt("X1234567Y", re2"X(.?){8,}Y") == @[@[8 .. 7]]
  check findAllBounds("X1234567Y", re2"X(.?){0,8}Y") == @[0 .. 8]
  check findAllCapt("X1234567Y", re2"X(.?){0,8}Y") == @[@[8 .. 7]]
  check findAllBounds("X1234567Y", re2"X(.?){1,8}Y") == @[0 .. 8]
  check findAllCapt("X1234567Y", re2"X(.?){1,8}Y") == @[@[8 .. 7]]
  check findAllBounds("X1234567Y", re2"X(.?){8,8}Y") == @[0 .. 8]
  check findAllCapt("X1234567Y", re2"X(.?){8,8}Y") == @[@[8 .. 7]]
  check findAllBounds("ababcd", re2"(a|ab|c|bcd){0,}(d*)") ==
    @[0 .. 0, 1 .. 0, 2 .. 5, 6 .. 5]
  check findAllCapt("ababcd", re2"(a|ab|c|bcd){0,}(d*)") ==
    @[@[0 .. 0, 1 .. 0],
      @[nonCapture, 1 .. 0],
      @[3 .. 5, 6 .. 5],
      @[nonCapture, 6 .. 5]]
  check findAllBounds("ababcd", re2"(a|ab|c|bcd){1,}(d*)") == @[0 .. 0, 2 .. 5]
  check findAllCapt("ababcd", re2"(a|ab|c|bcd){1,}(d*)") ==
    @[@[0 .. 0, 1 .. 0], @[3 .. 5, 6 .. 5]]
  check findAllBounds("ababcd", re2"(a|ab|c|bcd){2,}(d*)") == @[0 .. 5]
  check findAllCapt("ababcd", re2"(a|ab|c|bcd){2,}(d*)") ==
    @[@[3 .. 5, 6 .. 5]]
  check findAllBounds("ababcd", re2"(a|ab|c|bcd){3,}(d*)") == @[0 .. 5]
  check findAllCapt("ababcd", re2"(a|ab|c|bcd){3,}(d*)") ==
    @[@[3 .. 5, 6 .. 5]]
  check findAllBounds("ababcd", re2"(a|ab|c|bcd){4,}(d*)").len == 0
  check findAllCapt("ababcd", re2"(a|ab|c|bcd){4,}(d*)").len == 0
  check findAllBounds("ababcd", re2"(ab|a|c|bcd){0,}(d*)") == @[0 .. 5, 6 .. 5]
  check findAllCapt("ababcd", re2"(ab|a|c|bcd){0,}(d*)") ==
    @[@[4 .. 4, 5 .. 5], @[nonCapture, 6 .. 5]]
  check findAllBounds("ababcd", re2"(ab|a|c|bcd){1,}(d*)") == @[0 .. 5]
  check findAllCapt("ababcd", re2"(ab|a|c|bcd){1,}(d*)") ==
    @[@[4 .. 4, 5 .. 5]]
  check findAllBounds("ababcd", re2"(ab|a|c|bcd){2,}(d*)") == @[0 .. 5]
  check findAllCapt("ababcd", re2"(ab|a|c|bcd){2,}(d*)") ==
    @[@[4 .. 4, 5 .. 5]]
  check findAllBounds("ababcd", re2"(ab|a|c|bcd){3,}(d*)") == @[0 .. 5]
  check findAllCapt("ababcd", re2"(ab|a|c|bcd){3,}(d*)") ==
    @[@[4 .. 4, 5 .. 5]]
  check findAllBounds("ababcd", re2"(ab|a|c|bcd){4,}(d*)").len == 0
  check findAllCapt("ababcd", re2"(ab|a|c|bcd){4,}(d*)").len == 0

test "rust_regression":
  check findAllBounds("A_", re2"(?i-u)[a_]+") == @[0 .. 1]
  check findAllBounds("a_", re2"(?i-u)[a_]+") == @[0 .. 1]
  check findAllBounds("x", re2"(?i)[^x]").len == 0
  check findAllBounds("X", re2"(?i)[^x]").len == 0
  check findAllBounds("_", re2"[[:word:]]") == @[0 .. 0]
  check findAllBounds("abx", re2"([a-f]){2}(?P<foo>[x-z])") == @[0 .. 2]
  check findAllCapt("abx", re2"([a-f]){2}(?P<foo>[x-z])") == @[@[1 .. 1, 2 .. 2]]
  check findAllBounds("az", re2"ab?|$") == @[0 .. 0, 2 .. 1]
  check findAllBounds("azb", re2"z*azb") == @[0 .. 2]
  check findAllBounds("int", re2"1|2|3|4|5|6|7|8|9|10|int") == @[0 .. 2]
  check findAllBounds("Should this (work?)", re2"\b") ==
    @[0 .. -1, 6 .. 5, 7 .. 6, 11 .. 10, 13 .. 12, 17 .. 16]
  check findAllBounds("a b c", re2"\b") ==
    @[0 .. -1, 1 .. 0, 2 .. 1, 3 .. 2, 4 .. 3, 5 .. 4]
  check findAllBounds("ba", re2"^a|b") == @[0 .. 0]
  check findAllBounds("yyyyya", re2"^a|z").len == 0
  check findAllBounds("ayyyyy", re2"a$|z").len == 0
  check findAllBounds("CDAX", re2"(?:ABC|CDA|BC)X") == @[0 .. 3]
  check findAllBounds("CIMG2341", re2"((IMG|CAM|MG|MB2)_|(DSCN|CIMG))(?P<n>[0-9]+)$") ==
    @[0 .. 7]
  check findAllCapt("CIMG2341", re2"((IMG|CAM|MG|MB2)_|(DSCN|CIMG))(?P<n>[0-9]+)$") ==
    @[@[0 .. 3, nonCapture, 0 .. 3, 4 .. 7]]
  check findAllBounds("abcbX", re2"a(b*(X|$))?") == @[0 .. 0]
  check findAllBounds("a0.0c", re2"(a)\d*\.?\d+\b") == @[0 .. 1]
  check findAllCapt("a0.0c", re2"(a)\d*\.?\d+\b") == @[@[0 .. 0]]
  check findAllBounds("test", re2"typename type\-parameter\-[0-9]+\-[0-9]+::.+").len == 0
  check findAllBounds("foo Foo bar Bar", re2"(?:(?i)foo)|Bar") ==
    @[0 .. 2, 4 .. 6, 12 .. 14]
  check findAllBounds("I have 12, he has 2!", re2"\b..\b") ==
    @[0 .. 1, 7 .. 8, 9 .. 10, 11 .. 12, 17 .. 18]
  check findAllBounds("153.230000", re2"[0-4][0-4][0-4]000") == @[4 .. 9]
  check findAllBounds("153.230000\n", re2"[0-4][0-4][0-4]000") == @[4 .. 9]
  check findAllBounds("line1\nline2", re2"(?m)^(?:[^ ]+?)$") == @[0 .. 4, 6 .. 10]
  check findAllBounds("A\nB", re2"(?m)^(?:[^ ]+?)$") == @[0 .. 0, 2 .. 2]
  check findAllBounds("a ", re2"^a[[:^space:]]").len == 0
  check findAllBounds("foo boo a", re2"^a[[:^space:]]").len == 0
  check findAllBounds("r-f", re2"^-[a-z]").len == 0
  check findAllBounds("ab", re2"(a$)b$").len == 0
  check findAllBounds("ab", re2"^(a|ab)$") == @[0 .. 1]
  check findAllCapt("ab", re2"^(a|ab)$") == @[@[0 .. 1]]
  check findAllBounds("h", re2"(?:(?-u:\b)|(?u:h))+") == @[0 .. -1, 1 .. 0]
  check findAllBounds("鋸", re2"(?u:\B)").len == 0
  check findAllBounds("oB", re2"(?:(?u:\b)|(?s-u:.))+") == @[0 .. -1, 1 .. 1, 2 .. 1]
  check findAllBounds("\u{FEF80}", re2"(?:(?-u:\B)|(?su:.))+") == @[0 .. -1, 4 .. 3]
  check findAllBounds("\n‣", re2"(?m:$)(?m:^)(?su:.)") == @[0 .. 0]
  check findAllBounds("\n", re2"(?m:$)^(?m:^)") == @[0 .. -1]
  check findAllBounds("dodo", re2"(?P<kp>(?iu:do)(?m:$))*") ==
    @[0 .. -1, 1 .. 0, 2 .. 3, 4 .. 3]
  check findAllCapt("dodo", re2"(?P<kp>(?iu:do)(?m:$))*") ==
    @[@[nonCapture], @[nonCapture], @[2 .. 3], @[nonCapture]]
  check findAllBounds("\n\n", re2"((?m:$)(?-u:\B)(?s-u:.)(?-u:\B)$)") == @[1 .. 1]
  check findAllCapt("\n\n", re2"((?m:$)(?-u:\B)(?s-u:.)(?-u:\B)$)") == @[@[1 .. 1]]
  check findAllBounds("\n\u0081¨\u200a", re2"(?m:$)(?m:$)^(?su:.)") == @[0 .. 0]
  check findAllBounds("0\n", re2"(?-u:\B)(?m:^)") == @[2 .. 1]
  check findAllBounds("0", re2"(?:(?u:\b)|(?-u:.))+") == @[0 .. -1, 1 .. 0]
  check findAllBounds("ubi-Darwin-x86_64.tar.gz", re2"(?i:(?:\b|_)win(?:32|64|dows)?(?:\b|_))").len == 0
  check findAllBounds("ubi-Windows-x86_64.zip", re2"(?i:(?:\b|_)win(?:32|64|dows)?(?:\b|_))") ==
    @[4 .. 10]
  check findAllBounds("B", re2".*[^\s\S]A|B") == @[0 .. 0]
  check findAllBounds("Zeee.eZZZZZZZZeee>eeeeeee>", re2"e..+e.ee>") == @[1 .. 25]
  check findAllBounds("102:12:39", re2"(?:(\d+)[:.])?(\d{1,2})[:.](\d{2})") == @[0 .. 8]
  check findAllCapt("102:12:39", re2"(?:(\d+)[:.])?(\d{1,2})[:.](\d{2})") ==
    @[@[0 .. 2, 4 .. 5, 7 .. 8]]
  check findAllBounds("β77\n", re2".+\b\n") == @[0 .. 4]
  check findAllBounds("a-b", re2"^[[:alnum:]./-]+$") == @[0 .. 2]
  check findAllBounds(r"hiya \N{snowman} bye", re2"(\\N\{[^}]+})|([{}])") == @[5 .. 15]
  check findAllCapt(r"hiya \N{snowman} bye", re2"(\\N\{[^}]+})|([{}])") ==
    @[@[5 .. 15, nonCapture]]
