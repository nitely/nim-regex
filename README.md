# Regex

[![Build Status](https://img.shields.io/travis/nitely/nim-regex.svg?style=flat-square)](https://travis-ci.org/nitely/nim-regex)
[![licence](https://img.shields.io/github/license/nitely/nim-regex.svg?style=flat-square)](https://raw.githubusercontent.com/nitely/nim-regex/master/LICENSE)

A library for parsing, compiling, and executing regular expressions.

Features:

* The match time is linear in the length of the input string
* Regular expressions are (optionally) compiled at compile-time
* Captures all group repetitions (not just the last one)
* Unicode level-1 support

> This is a WIP. API is not stable

## Status

- [ ] `\pN`
- [ ] `\PN`
- [ ] `\p{Greek}`
- [ ] `\P{Greek}`
- [ ] `x` group flag
- [ ] `\123`
- [ ] `\x7F`
- [ ] `\x{10FFFF}`
- [ ] Character classes matching as described in
  [UTS#18](http://www.unicode.org/reports/tr18/#Compatibility_Properties)
- [ ] `[[:alnum:]]`, etc
- [ ] User friendly compiling errors
- [ ] APIs (nre/re/re2/rust-regex API parity)

## Docs

[Read the docs](https://nitely.github.io/nim-regex/)

## Tests

```
nimble test
```

## LICENSE

MIT
