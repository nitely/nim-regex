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
