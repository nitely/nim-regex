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
of skip multiple parts of the input.

The special find works just like the regular one,
except it stops when the current character of the
input cannot be matched by the Regular Expression (RE).
Once the special find stops, we can run a memchr from
the index at which the find stopped.

We never process a character more than once, hence
the algorithm runs in linear time.

^ There is a bunch of possible literal optimizations,
  but this is the meat of it.
]#

import nfa

func prefixLit(nfa: Nfa): int32 =
  discard
