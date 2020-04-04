## Benchmarks

Run first:

```
nimble install nimbench
nimble develop
```

Run benchmarks:

```
nim c -r -d:release bench/bench.nim
```

> Try -d:danger as well, but release is what most users will set
