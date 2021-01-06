v0.19.0
==================

* Adds support for unbounded lookaround assertions
* Fix: parsing `{n,m}` repetitions is less strict;
  `{}`, `{abc}`, etc are parsed as characters. This is
  closer to PCRE, but it won't allow error prone instances
  such as missing brackets: `{123`.
* Fix: double repetitions: `**`, `++`, `*+`, `???`, `{n}*`, `{n}+`,
  and other combinations are no longer allowed. The `++` PCRE hack
  is not allowed, as it won't work the same way anyway.

v0.18.0
==================

* Adds `escapeRe(string): string` function
* Removed `unicodeplus` dependency

v0.17.1
==================

* Fix: regression related to repetitions,
  and lonely assertions; issue #83
* Fix: make it compile with ARC; thanks to @timotheecour

v0.17.0
==================

* Removes macro usage for regular function APIs
* Adds `match` block macro
* Fix: sub-matches with nested optional operators
  (ex: `(a?)*`) to work the same as PCRE

v0.16.2
==================

* Adds `findAllBounds`
* Adds some minor perf improvements

v0.16.1
==================

* Adds `groupFirstCapture`, `groupLastCapture`, and
  `group(1): seq[string]` for group numbers
* Adds support for negative lookaround assertions

v0.16.0
==================

* Adds literals optimization to `findAll`,
  `split`, `splitIncl`, and `replace`; this makes
  some regexes run ~100x faster.
* Support lookbehind; this is limited to one character
* Fix: `findAll` ran in quadratic time for some regexes;
  `split`, `splitIncl`, and `replace` were also affected;
  Linear time is now guaranteed.
* Fix: regex compilation errors were swallow due to
  a Nim bug; A workaround to raise the error was implemented

v0.15.0
==================

* Fix multiline not working with beginning of line, issue #13
* Fix replace re"", issue #29
* Fixes a number of issues related to empty matches
  in the `findAll`, `split`, and `replace` APIs

v0.14.1
==================

* Fix non-greedy find, issue #61 (v0.14.0 regression)

v0.14
==================

* Drop Nim 0.19.0 support (0.19.6 is supported)
* Changed all `proc` to `func`
* Faster macro for static regex
* Deprecated `toPattern`
* New `match` API that does not require the `MatchRegex` parameter

v0.13.1
==================

* Fix nested non-capturing group repetition #46
* Remove stylecheck config #55

v0.13
==================

* Add `groupFirstCapture`, `groupLastCapture`, and
  `group(1): seq[string]` (thanks to @xmonader)
* Add Nim 1.0.0 to CI
* Drop Nim 0.18 support
* Fix nested captures with repetition range; issue #46
* Fix Nim `sets` warnings

v0.12
==================

* Support matching at compile-time; issue #4
  (thanks to @timotheecour)

v0.11.2
==================

* Added `isInitialized*(re: Regex)`

v0.11.1
==================

* Fix `\w` not matching `_` on ASCII mode
* Fixes to support the JS backend

v0.11
==================

* Update to Unicode 12.1

v0.10.1
==================

* Fix for Nim devel (PR #34)

v0.10.0
==================

* Add Nim 0.18 support back

v0.9.0 (unreleased)
==================

* Drop Nim 0.18 support
* Improved `re` API to support compile-time
  or run-time compilation depending on input
  (thanks to @timotheecour)

v0.8.0
==================

* Drop Nim 0.17 support
* Add Nim 0.19 support
* Update dependencies
* Remove deprecated `match` and `find`
  returning `Option[RegexMatch]`

v0.7.4
==================

* Add `splitIncl`, similar to `split` but
  includes captured groups

v0.7.3
==================

* Fix deprecation warnings on devel

v0.7.2
==================

* Fixes for devel (#17)

v0.7.1
==================

* Update dependencies

v0.7.0
==================

* New API taking `var RegexMatch` instead
  of returning `Option[RegexMatch]`
* Deprecate `match` and `find`
  returning `Option[RegexMatch]`
* Update to unicode 11

v0.6.3
==================

* Pretty error messages

v0.6.2
==================

* Adds limited lookahead support
* Improves compilation time

v0.6.1
==================

* Initial release
