# Regex

[![Build Status](https://img.shields.io/travis/nitely/nim-regex.svg?style=flat-square)](https://travis-ci.org/nitely/nim-regex)
[![licence](https://img.shields.io/github/license/nitely/nim-regex.svg?style=flat-square)](https://raw.githubusercontent.com/nitely/nim-regex/master/LICENSE)

A library for parsing, compiling, and executing
regular expressions. All searches execute in linear
time with respect to the size of the regular expression
and search text. It can parse, compile and match
regular expression at compile-time.

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
