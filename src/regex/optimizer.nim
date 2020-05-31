## Literals optimization.
## This is about optimizing the find operation
## by quickly searching Regular Expression literals
## within the input string. See issue #59.

#[
Only optimizations that are
guaranteed to run in linear
time are implemented.

re"(?ms)^.+abc"
can be optimized by running the match
at every \n. We can do a quick memchr to find
every \n.

There is a gotcha, though. If we do this on
an input such as `\n\n\n\n\n` the matcher will
consume the whole string for every `\n`, hence
runtime is cuadratic.

The solution is to run a special kind of find,
instead of a regular match. We need a special find,
because the regular find will always consume the
whole string, so we'd only be able to skip the first
part of the input until the first `\n`, instead
of skipping multiple parts of the input.

The special find works just like the regular one,
except it stops when the current character of the
input cannot be matched by the Regular Expression (RE).
Once the special find stops, we can run a memchr from
the index where the find stopped.

We never process a character more than once, hence
the algorithm runs in linear time.

^ There is a bunch of possible literal optimizations,
  but this is the meat of it.

re"\wabc"
This can be optimized the same way as
the first example, except going back
one character after running memchr.

The runtime is again linear, every
character can be consumed at most 2 times:
one fordward (memchr), and one fordward
(special find). The "go back" operation does
not consumes characters, it just does
index - len(prefix).

The prefix may be longer than just one
character, as long as the lenght of it
is fixed we can just go back the lenght of
the prefix and run a find. We can do
this even when the prefix contain alternations
as long as they have the same lenght, for
example re"(x|y)abc".

The alternative to going back X chars
is to run the regex prefix in reverse, and
then run the special find for the full regex.
This would support prefixes with mixed
alternation lenghts (i.e: re"(\w\w|\d)foo").
Since this is needed, the going back X
chars may just be an optimal optimization
(meaning I may not implement it).

re"\w+abc"
This can be optimized by doing a memchr
for "a", running a special match for the
regex prefix ("\w+") in reverse, and running
the special find for the full regex.

The special match is a regular match that returns
the longest match.

We cannot divide the regex in a prefix and suffix
of "a", and just run that because that would
run in cuadratic time (see the first "\n\n\n" input example).
Also, we need to support captures.

re"\w(a|b)"
This can be optimized by running two scans,
one for "a" and one for "b" at the start and
after each match, and then trying to match the one
with smaller char index first (i.e: the one
that we found first).

My experiments show PCRE do this with up to
two literal alternations. When there are more
than two alternations, it's likely better to
generate a DFA. For example:
re"foo|bar|baz|quz" would generate a DFA
that matches re"f|b|q". We'd use the DFA
instead of memchr.

Single literals should be preferred over
alternations. For example: re"\w(a|b)c" would
memchr the "c" character. Meaning other listed
optimizations are preferred.
]#

import nfa

# re"(abc)+" -> a
# re"(abc)*" -> -1
# re"(abc)*xyz" -> x
func lonelyLit(nfa: Nfa): int32 =
  discard
