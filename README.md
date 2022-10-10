# Regex

[![Build Status](https://img.shields.io/github/workflow/status/nitely/nim-regex/CI/master?style=flat-square)](https://github.com/nitely/nim-regex/actions?query=workflow%3ACI)
[![licence](https://img.shields.io/github/license/nitely/nim-regex.svg?style=flat-square)](https://raw.githubusercontent.com/nitely/nim-regex/master/LICENSE)

A library for parsing, compiling, and executing regular expressions.

Features:

* The match time is linear in the length of the input string
* Regular expressions are (optionally) compiled at compile-time
* Captures all group repetitions (not just the last one)
* Unicode level-1 support
* Descriptive error messages
* Supports matching at compile-time (Nim +0.20)
* PCRE syntax and semantics

## Install

```
nimble install regex
```

# Compatibility

Nim +1.0.0

## Docs

[Read the docs](https://nitely.github.io/nim-regex/)

## Tests

```
nimble test
```

## Debugging

Compile with `-d:regexDotDir:.` to generate [dot files](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) of the regexes (NFAs) within the nim file. A dot file can be viewed in [Graphviz](https://dreampuf.github.io/GraphvizOnline/). Requires Nim +1.2.

## LICENSE

MIT
